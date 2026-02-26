import Foundation
import GRDB

/// Central state manager — drives all 6 tab views.
@MainActor
final class AppSession: ObservableObject {

    // MARK: - Published state

    @Published var isLoggedIn: Bool
    @Published var selectedDate = Date()
    @Published var athletes: [Athlete] = []
    @Published var selectedAthleteIDs: Set<Int> = []
    @Published var isLoading = false
    @Published var loadingMessage = ""
    @Published var errorMessage: String?

    // Filters (changes automatically re-filter all derived views)
    @Published var analysisDistanceM: Double = AppConfig.analysisDistanceM
    @Published var analysisStartTime: Date?     // nil = use all data (set to gpsStart/gpsEnd on load)
    @Published var analysisEndTime:   Date?

    // Raw all-athlete data — do not read from views directly
    private var _allGpsRecords: [Int: [GPSRecord]] = [:] { willSet { objectWillChange.send() } }
    private var _allConditionedSegments: [ConditionedSegment] = [] { willSet { objectWillChange.send() } }

    // Computed filtered views — these are what views consume

    /// Helper: filter records by the global time window.
    private func timeFiltered(_ records: [GPSRecord]) -> [GPSRecord] {
        guard let start = analysisStartTime, let end = analysisEndTime else { return records }
        return records.filter { $0.ts >= start && $0.ts <= end }
    }

    /// Full-resolution GPS records filtered by selection + time window.
    var gpsRecordsByAthlete: [Int: [GPSRecord]] {
        let ids = selectedAthleteIDs
        return _allGpsRecords
            .filter { ids.contains($0.key) }
            .mapValues { timeFiltered($0) }
    }

    /// One GPS record per minute per athlete. Used by the map to keep rendering fast.
    var mapGpsRecordsByAthlete: [Int: [GPSRecord]] {
        gpsRecordsByAthlete.mapValues { records in
            var seen = Set<Int>()
            return records.filter { seen.insert(Int($0.ts.timeIntervalSince1970) / 60).inserted }
        }
    }
    /// All athlete IDs that have GPS data for the loaded date, regardless of selection.
    var allGpsAthleteIds: Set<Int> { Set(_allGpsRecords.keys) }

    /// All GPS records for every athlete (ignores selection filter), applies global time window.
    var allGpsRecords: [Int: [GPSRecord]] {
        _allGpsRecords.mapValues { timeFiltered($0) }
    }

    var conditionedSegments: [ConditionedSegment] {
        _allConditionedSegments.filter { selectedAthleteIDs.contains($0.segment.athleteID) }
    }
    var leagueEntries: [LeagueEntry] {
        let dict = Dictionary(uniqueKeysWithValues: athletes.map { ($0.id, $0) })
        return LeagueBuilder.build(segments: conditionedSegments, athletes: dict)
    }

    @Published var exerciseAthleteIds: Set<Int> = []
    @Published var gpsStart: Date? = nil   // from ALL bbox athletes, used for animation/SIMCOSTA
    @Published var gpsEnd:   Date? = nil
    @Published var buoyReadings: [BuoyReading] = []
    @Published var currentGrid: CurrentGrid?

    // MARK: - Stores

    let client = TreinusClient.shared
    let athleteStore = AthleteStore(dbQueue: AppDatabase.shared.dbQueue)
    let exerciseStore = ExerciseStore(dbQueue: AppDatabase.shared.dbQueue)
    let buoyStore = BuoyStore(dbQueue: AppDatabase.shared.dbQueue)

    /// Keeps the athletes table observation alive.
    private var athleteObservation: AnyDatabaseCancellable?
    /// Live count of exercises that still lack GPS analysis (auto-updates via GRDB observation).
    @Published var backfillPending: Int = 0
    private var backfillObservation: AnyDatabaseCancellable?

    /// Single background-sync task. Cancelled before any foreground pipeline run.
    private var backgroundSyncTask: Task<Void, Never>?
    /// In-flight foreground load task. Cancelled when a new load starts.
    private var loadTask: Task<Void, Never>?
    /// Throttle exercise-list syncs to at most once per interval (in-memory; resets on relaunch).
    private var lastExerciseListSync: Date?
    private let exerciseListSyncInterval: TimeInterval = 30 * 60  // 30 minutes

    // MARK: - Init

    init() {
        isLoggedIn = CredentialManager.hasCredentials
        if let teamId = CredentialManager.load()?.teamId {
            // Synchronous pre-load for immediate display on launch.
            athletes = (try? AppDatabase.shared.dbQueue.read { db in
                try Athlete
                    .filter(Column("team_id") == teamId)
                    .order(Column("id"))
                    .fetchAll(db)
            }) ?? []
            selectedAthleteIDs = Set(athletes.map(\.id))
            startObservingAthletes(teamId: teamId)
            startObservingBackfill(teamId: teamId)
        }
    }

    /// Starts a live observation of the athletes table for the given team.
    /// Any write to the table (from runPipeline, backgroundSync, etc.) automatically
    /// pushes an updated list to `athletes` on the main queue.
    private func startObservingAthletes(teamId: Int) {
        athleteObservation = ValueObservation
            .tracking { db in
                try Athlete
                    .filter(Column("team_id") == teamId)
                    .order(Column("id"))
                    .fetchAll(db)
            }
            .start(
                in: AppDatabase.shared.dbQueue,
                scheduling: .async(onQueue: .main),
                onError: { _ in },
                onChange: { [weak self] updated in
                    guard let self else { return }
                    let prev = self.athletes
                    self.athletes = updated
                    // Auto-select any newly discovered athletes
                    let newIds = Set(updated.map(\.id)).subtracting(Set(prev.map(\.id)))
                    self.selectedAthleteIDs.formUnion(newIds)
                }
            )
    }

    /// Tracks how many exercises still lack GPS analysis. Auto-decrements as downloads complete.
    private func startObservingBackfill(teamId: Int) {
        backfillObservation = ValueObservation
            .tracking { db in
                try Int.fetchOne(db, sql: """
                    SELECT COUNT(*) FROM exercises
                    WHERE team_id = ?
                      AND NOT EXISTS (
                        SELECT 1 FROM analysis_raw
                        WHERE analysis_raw.exercise_id = exercises.id
                          AND analysis_raw.athlete_id  = exercises.athlete_id
                          AND analysis_raw.team_id     = exercises.team_id
                      )
                    """, arguments: [teamId]) ?? 0
            }
            .start(
                in: AppDatabase.shared.dbQueue,
                scheduling: .async(onQueue: .main),
                onError: { _ in },
                onChange: { [weak self] count in self?.backfillPending = count }
            )
    }

    // MARK: - Authentication

    func login(email: String, password: String, teamId: Int?) async {
        isLoading = true
        loadingMessage = "Entrando no Treinus…"
        errorMessage = nil
        defer { isLoading = false }
        do {
            try await client.login(email: email, password: password, teamId: teamId)
            try CredentialManager.save(email: email, password: password, teamId: teamId)
            isLoggedIn = true
            if let tid = teamId {
                startObservingAthletes(teamId: tid)
                startObservingBackfill(teamId: tid)
            }
        } catch {
            errorMessage = error.localizedDescription
        }
    }

    /// Silently re-authenticates on launch using saved credentials.
    func autoLogin() async {
        guard let creds = CredentialManager.load() else { return }
        do {
            try await client.login(
                email: creds.email,
                password: creds.password,
                teamId: creds.teamId
            )
        } catch {
            // Stale credentials — user sees error when they tap "Carregar"
        }
    }

    func logout() {
        loadTask?.cancel()
        loadTask = nil
        backgroundSyncTask?.cancel()
        backgroundSyncTask = nil
        athleteObservation = nil
        backfillObservation = nil
        try? CredentialManager.clear()
        isLoggedIn = false
        athletes = []
        selectedAthleteIDs = []
        _allGpsRecords = [:]
        _allConditionedSegments = []
        exerciseAthleteIds = []
        gpsStart = nil; gpsEnd = nil
        buoyReadings = []
        currentGrid = nil
    }

    // MARK: - Data loading

    /// Load from DB + download conditions; does NOT call Treinus API.
    /// After loading, kicks off a silent background sync to keep the cache warm.
    /// Safe to call concurrently — cancels any in-flight load first.
    func loadFromCache() {
        loadTask?.cancel()
        loadTask = Task { @MainActor [weak self] in
            guard let self else { return }
            guard let teamId = await client.authenticatedTeamId else {
                errorMessage = "Faça login primeiro."
                return
            }
            backgroundSyncTask?.cancel()
            backgroundSyncTask = nil
            isLoading = true
            loadingMessage = "Carregando do banco…"
            errorMessage = nil
            defer {
                isLoading = false
                backgroundSyncTask = Task.detached(priority: .background) { [weak self] in
                    await self?.backgroundSync()
                }
            }
            do { try await runPipeline(teamId: teamId, syncTreinus: false) }
            catch { if !(error is CancellationError) { errorMessage = error.localizedDescription } }
        }
    }

    /// Sync exercises + GPS analysis from Treinus, then run the full pipeline.
    func syncWithTreinus() {
        loadTask?.cancel()
        loadTask = Task { @MainActor [weak self] in
            guard let self else { return }
            guard let teamId = await client.authenticatedTeamId else {
                errorMessage = "Faça login primeiro."
                return
            }
            backgroundSyncTask?.cancel()
            backgroundSyncTask = nil
            isLoading = true
            loadingMessage = "Sincronizando com Treinus…"
            errorMessage = nil
            defer {
                isLoading = false
                backgroundSyncTask = Task.detached(priority: .background) { [weak self] in
                    await self?.backgroundSync()
                }
            }
            do { try await runPipeline(teamId: teamId, syncTreinus: true) }
            catch { if !(error is CancellationError) { errorMessage = error.localizedDescription } }
        }
    }

    /// Silently fetches exercises + GPS analysis for all known athletes, all dates.
    /// Does not touch isLoading or errorMessage — runs fully in background.
    func backgroundSync() async {
        guard let teamId = await client.authenticatedTeamId else { return }
        let knownAthletes = (try? await athleteStore.fetchAll(teamId: teamId)) ?? []
        guard !knownAthletes.isEmpty else { return }
        let allIds = knownAthletes.map(\.id)

        // Fetch & cache exercise lists from Treinus for all athletes — throttled.
        let now = Date()
        let shouldSync = lastExerciseListSync.map { now.timeIntervalSince($0) >= exerciseListSyncInterval } ?? true
        if shouldSync {
            await withTaskGroup(of: Void.self) { group in
                for id in allIds {
                    group.addTask {
                        guard !Task.isCancelled else { return }
                        guard let exs = try? await self.client.getExercises(athleteId: id, teamId: teamId)
                        else { return }
                        try? await self.exerciseStore.syncExercises(exs, athleteId: id, teamId: teamId)
                    }
                }
            }
            // Record completion time only after the group finishes so that a cancelled
            // backgroundSync (rapid date navigation) doesn't block the next 30-min window.
            lastExerciseListSync = Date()
        }

        // Priority: download GPS for the selected date first so the UI has data immediately.
        let dayExercises = (try? await exerciseStore.fetchExercises(
            athleteIds: allIds, datePrefix: bahiaDayPrefix(selectedDate)
        )) ?? []
        await downloadMissingAnalysis(for: dayExercises, teamId: teamId)

        // Backfill: download GPS for every exercise that still lacks it, most-recent first.
        // fetchExercisesWithoutAnalysis excludes what was just downloaded above.
        // If this task is cancelled (user changes date), progress is preserved in DB;
        // the next backgroundSync resumes from where it left off.
        // On subsequent syncs after the backfill is complete, this list naturally contains
        // only new exercises — giving us "refresh only new" for free.
        let allMissing = (try? await exerciseStore.fetchExercisesWithoutAnalysis(
            athleteIds: allIds, teamId: teamId
        )) ?? []
        await downloadMissingAnalysis(for: allMissing, teamId: teamId)
    }

    private func downloadMissingAnalysis(for exercises: [ExerciseRow], teamId: Int) async {
        // Capture on main actor before entering the concurrent task group.
        let placeholders = Set(athletes.filter { $0.fullName.hasPrefix("#") }.map(\.id))
        // Gentle API usage: at most 2 concurrent calls, at least 1 s between each launch.
        let maxConcurrent = 2
        await withTaskGroup(of: Void.self) { group in
            var inflight = 0
            for row in exercises {
                guard !Task.isCancelled else { break }
                // If at capacity, wait for one slot to free before adding more.
                if inflight >= maxConcurrent {
                    await group.next()
                    inflight -= 1
                }
                inflight += 1
                group.addTask {
                    guard !Task.isCancelled else { return }
                    let needsAnalysis = (try? await self.exerciseStore.hasAnalysis(exerciseId: row.id, athleteId: row.athleteId, teamId: teamId)) == false
                    let needsName     = placeholders.contains(row.athleteId)
                    guard needsAnalysis || needsName else { return }
                    guard !Task.isCancelled else { return }
                    guard let analysis = try? await self.client.getExerciseAnalysis(
                        exerciseId: row.id, athleteId: row.athleteId, teamId: teamId
                    ) else { return }
                    // upsertAnalysis writes GPS records and athlete name in one transaction
                    try? await self.exerciseStore.upsertAnalysis(analysis, teamId: teamId)
                }
                // Throttle: pause before launching the next request.
                try? await Task.sleep(nanoseconds: 1_000_000_000)  // 1 s
            }
        }
    }

    private func runPipeline(teamId: Int, syncTreinus: Bool) async throws {
        // Use ALL known athletes so in-memory selection filter always works without reload.
        let athleteIds = athletes.isEmpty ? Array(selectedAthleteIDs) : athletes.map(\.id)

        // 1. When syncing explicitly: refresh exercise lists from Treinus (lightweight metadata).
        //    Skipped during loadFromCache (syncTreinus: false) — backgroundSync handles this
        //    silently so the foreground path stays fully offline/DB-only.
        if syncTreinus {
            loadingMessage = "Sincronizando exercícios…"
            await withTaskGroup(of: Void.self) { group in
                for id in athleteIds {
                    group.addTask {
                        guard let exs = try? await self.client.getExercises(athleteId: id, teamId: teamId)
                        else { return }
                        try? await self.exerciseStore.syncExercises(exs, athleteId: id, teamId: teamId)
                    }
                }
            }
        }

        // 2. Fetch exercises for selected date ±1 day (GPS timestamps are source of truth,
        //    exercise list just drives which analyses to download).
        var dayCal = Calendar(identifier: .gregorian); dayCal.timeZone = AppConfig.timezone
        let dayBefore = dayCal.date(byAdding: .day, value: -1, to: selectedDate)!
        let dayAfter  = dayCal.date(byAdding: .day, value:  1, to: selectedDate)!
        let windowRows = try await exerciseStore.fetchExercisesInRange(
            athleteIds: athleteIds,
            from: bahiaDayPrefix(dayBefore),
            to:   bahiaDayPrefix(dayAfter)
        )

        if syncTreinus {
            // 4. Download missing analysis for window exercises (writes records + name atomically)
            loadingMessage = "Baixando análises GPS…"
            try await withThrowingTaskGroup(of: Void.self) { group in
                for row in windowRows {
                    group.addTask {
                        guard !(try await self.exerciseStore.hasAnalysis(exerciseId: row.id, athleteId: row.athleteId, teamId: teamId))
                        else { return }
                        let analysis = try await self.client.getExerciseAnalysis(
                            exerciseId: row.id, athleteId: row.athleteId, teamId: teamId
                        )
                        try await self.exerciseStore.upsertAnalysis(analysis, teamId: teamId)
                    }
                }
                try await group.waitForAll()
            }
        }

        // 5. GPS records: SQL bbox+time filter → only athletes in the water today, fast
        loadingMessage = "Carregando GPS…"
        var cal = Calendar(identifier: .gregorian); cal.timeZone = AppConfig.timezone
        let dayStart = cal.startOfDay(for: selectedDate)
        let dayEnd   = cal.date(byAdding: .day, value: 1, to: dayStart)!.addingTimeInterval(-1)
        let fromTs   = Int(dayStart.timeIntervalSince1970)
        let toTs     = Int(dayEnd.timeIntervalSince1970)

        let bboxAthleteIds = try await exerciseStore.fetchAthleteIdsInBbox(
            from: fromTs, to: toTs,
            latMin: AppConfig.toSemicircle(AppConfig.bboxSSA.minLat),
            latMax: AppConfig.toSemicircle(AppConfig.bboxSSA.maxLat),
            lonMin: AppConfig.toSemicircle(AppConfig.bboxSSA.minLon),
            lonMax: AppConfig.toSemicircle(AppConfig.bboxSSA.maxLon)
        )
        guard !bboxAthleteIds.isEmpty else {
            _allGpsRecords = [:]
            _allConditionedSegments = []
            gpsStart = nil; gpsEnd = nil
            return
        }

        let recordRows = try await exerciseStore.fetchRecords(
            athleteIds: bboxAthleteIds, from: fromTs, to: toTs
        )
        var grouped: [Int: [GPSRecord]] = [:]
        for row in recordRows {
            guard let lat = row.positionLat, let lon = row.positionLon else { continue }
            grouped[row.athleteId, default: []].append(GPSRecord(
                exerciseID: row.exerciseId, athleteID: row.athleteId,
                ts: Date(timeIntervalSince1970: Double(row.ts)),
                positionLat: lat, positionLon: lon,
                distanceM: row.distanceM, speedMps: row.speedMps, heartRate: row.heartRate
            ))
        }
        guard !grouped.isEmpty else {
            _allGpsRecords = [:]
            _allConditionedSegments = []
            gpsStart = nil; gpsEnd = nil
            return
        }

        // Athletes are now known — update state before any slow network calls
        exerciseAthleteIds = Set(grouped.keys)
        _allGpsRecords = grouped

        let allRecords = grouped.values.flatMap { $0 }
        gpsStart = allRecords.map(\.ts).min()
        gpsEnd   = allRecords.map(\.ts).max()
        analysisStartTime = gpsStart
        analysisEndTime   = gpsEnd

        // 6. SISCORAR current grid
        loadingMessage = "Baixando correntes SISCORAR…"
        do { currentGrid = try await CurrentDataClient.shared.fetchGrid(for: selectedDate) }
        catch { currentGrid = nil }

        // 7. SIMCOSTA buoy readings — expand to floor(hour)-1 … floor(hour)+2 so on-the-hour
        //    readings at the edges of the session are never missed.
        loadingMessage = "Buscando dados da boia…"
        if let s = gpsStart, let e = gpsEnd {
            var hCal = Calendar(identifier: .gregorian); hCal.timeZone = AppConfig.timezone
            let floorStart = hCal.date(from: hCal.dateComponents([.year,.month,.day,.hour], from: s))!
            let floorEnd   = hCal.date(from: hCal.dateComponents([.year,.month,.day,.hour], from: e))!
            let fetchFrom  = hCal.date(byAdding: .hour, value: -1, to: floorStart)!
            let fetchTo    = hCal.date(byAdding: .hour, value:  2, to: floorEnd)!
            do { buoyReadings = try await SimcostaClient.shared.fetchReadings(from: fetchFrom, to: fetchTo, store: buoyStore) }
            catch { buoyReadings = [] }
        } else {
            buoyReadings = []
        }

        // 8-10. Analysis (fastest segment → env matching → league table)
        loadingMessage = "Calculando tempos…"
        runAnalysis(on: grouped)
    }

    /// Re-runs steps 8-10 in-memory (no network) — called when analysisDistanceM changes.
    func rerunAnalysis() {
        guard !_allGpsRecords.isEmpty else { return }
        runAnalysis(on: _allGpsRecords)
    }

    private func runAnalysis(on gpsData: [Int: [GPSRecord]]) {
        // Apply time filter if set
        let filtered: [Int: [GPSRecord]]
        if let start = analysisStartTime, let end = analysisEndTime {
            filtered = gpsData
                .mapValues { $0.filter { $0.ts >= start && $0.ts <= end } }
                .filter { !$0.value.isEmpty }
        } else {
            filtered = gpsData
        }

        let analyzer = FastestDistanceAnalyzer(distanceM: analysisDistanceM)
        var fastestSegments: [FastestSegment] = []
        for (_, records) in filtered {
            if let seg = analyzer.run(records: records) { fastestSegments.append(seg) }
        }

        var conditioned: [ConditionedSegment]
        if let g = currentGrid {
            conditioned = EnvironmentalMatcher.matchCurrent(segments: fastestSegments, grid: g)
        } else {
            conditioned = fastestSegments.map {
                ConditionedSegment(
                    segment: $0,
                    currentU: nil, currentV: nil, currentSpeedMps: nil, currentDirDeg: nil,
                    buoyWindSpeedMps: nil, buoyWindDirDeg: nil,
                    buoyCurrentSpeedMps: nil, buoyCurrentDirDeg: nil,
                    siscorarCurrentComponent: nil, buoyCurrentComponent: nil, windComponent: nil
                )
            }
        }
        conditioned = EnvironmentalMatcher.matchBuoy(segments: conditioned, readings: buoyReadings)
        conditioned = conditioned.map { ApparentConditions.apply(to: $0) }
        _allConditionedSegments = conditioned
    }

    /// Probes Treinus for athlete IDs, persists them, then bootstraps the last
    /// `bootstrapExerciseCount` GPS analyses per athlete so the app is immediately useful.
    func refreshAthletes() async {
        guard let teamId = await client.authenticatedTeamId else {
            errorMessage = "Faça login primeiro."
            return
        }
        isLoading = true
        loadingMessage = "Buscando atletas… (pode demorar)"
        errorMessage = nil
        defer { isLoading = false }
        do {
            let maxId = max(10, UserDefaults.standard.integer(forKey: "maxAthleteId"))
            let ids = try await client.probeAthletes(teamId: teamId, maxId: maxId)
            let newAthletes = ids.map {
                Athlete(id: $0, teamId: teamId, fullName: "#\($0)", syncedAt: Date())
            }
            try await athleteStore.upsertAll(newAthletes)
            // observation will push updated list; just pre-select new IDs
            selectedAthleteIDs = Set(newAthletes.map(\.id))

            // Bootstrap: fetch exercise lists for all athletes, then download GPS analysis
            // for the most recent exercises. One-time wait; makes app immediately useful.
            loadingMessage = "Baixando listas de exercícios…"
            var allExercises: [TreinusExercise] = []
            await withTaskGroup(of: (Int, [TreinusExercise]).self) { group in
                for id in ids {
                    group.addTask {
                        let exs = (try? await self.client.getExercises(athleteId: id, teamId: teamId)) ?? []
                        return (id, exs)
                    }
                }
                for await (athleteId, exs) in group {
                    try? await exerciseStore.syncExercises(exs, athleteId: athleteId, teamId: teamId)
                    allExercises.append(contentsOf: exs)
                }
            }

            let bootstrapCount = 10
            loadingMessage = "Baixando GPS dos últimos \(bootstrapCount) exercícios por atleta…"
            // Take last N per athlete (sorted by start string, which is lexicographically ordered).
            let recentRows = Dictionary(grouping: allExercises, by: \.athleteId)
                .flatMap { _, exs in
                    exs.sorted { ($0.start ?? "") > ($1.start ?? "") }.prefix(bootstrapCount)
                }
                .map { ex in
                    ExerciseRow(id: ex.id, athleteId: ex.athleteId, teamId: ex.teamId,
                                genreName: ex.genreName, start: ex.start ?? "",
                                distanceM: ex.distanceM, elapsedSec: ex.totalElapsedTimeSec,
                                extraJson: nil)
                }
            await downloadMissingAnalysis(for: recentRows, teamId: teamId)
        } catch {
            errorMessage = error.localizedDescription
        }
    }

    // MARK: - Clear helpers

    func clearAnalysisCache() async {
        do {
            try FileCache.clearCurrentGrids()
            try await exerciseStore.deleteRecordsAndAnalysis()
            _allConditionedSegments = []
            _allGpsRecords = [:]
            currentGrid = nil
        } catch { errorMessage = error.localizedDescription }
    }

    func clearBuoyData() async {
        do {
            try await buoyStore.deleteAll()
            buoyReadings = []
        } catch { errorMessage = error.localizedDescription }
    }

    func clearAll() async {
        do {
            try FileCache.clearCurrentGrids()
            try await AppDatabase.shared.dbQueue.write { db in
                try db.execute(sql: "DELETE FROM athletes")
                try db.execute(sql: "DELETE FROM buoy_observations")
                try db.execute(sql: "DELETE FROM buoy_coverage")
            }
            athletes = []
            selectedAthleteIDs = []
            _allConditionedSegments = []
            _allGpsRecords = [:]
            buoyReadings = []
            currentGrid = nil
        } catch { errorMessage = error.localizedDescription }
    }

    // MARK: - Helpers

    var formattedDate: String {
        let fmt = DateFormatter()
        fmt.locale = Locale(identifier: "pt_BR")
        fmt.dateStyle = .medium
        return fmt.string(from: selectedDate)
    }

    private func bahiaDayPrefix(_ date: Date) -> String {
        let fmt = DateFormatter()
        fmt.locale   = Locale(identifier: "en_US_POSIX")
        fmt.timeZone = AppConfig.timezone
        fmt.dateFormat = "yyyy-MM-dd"
        return fmt.string(from: date)
    }
}
