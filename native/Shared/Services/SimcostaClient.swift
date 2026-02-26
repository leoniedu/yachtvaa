import Foundation

/// Downloads SIMCOSTA buoy data and caches it via BuoyStore.
///
/// Mirrors the gap-filling logic in rsimcosta: only time ranges not already
/// in `buoy_coverage` are fetched from the network.
actor SimcostaClient {
    static let shared = SimcostaClient()

    private let baseURL = URL(string: "https://simcosta.furg.br/api")!
    private let session = URLSession.shared

    /// The four variables needed for wind/current analysis.
    /// Fetching only these keeps response sizes small (~5 KB/day).
    private static let standardParams =
        "Avg_Wnd_Dir_N,Avg_Wnd_Sp,C_Avg_Dir_N,C_Avg_Spd"

    private static let metaCols: Set<String> = [
        "timestamp", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND",
    ]

    // MARK: - Public API

    /// Returns `BuoyReading` array for a calendar day (Bahia timezone),
    /// fetching only time ranges not already in the cache.
    func fetchReadings(
        boiaId: Int = AppConfig.buoyID,
        date: Date,
        store: BuoyStore
    ) async throws -> [BuoyReading] {
        let (from, to) = dayBounds(date)

        // Download only uncached ranges
        let missing = try await store.missingRanges(
            boiaId: boiaId, endpoint: "standard", from: from, to: to
        )
        for (s, e) in missing {
            let obs = try await fetchStandard(boiaId: boiaId, from: s, to: e)
            // Only register coverage when rows were returned. Empty responses
            // mean data isn't available yet (recent session, upload delay) and
            // should be retried on the next call.
            guard !obs.isEmpty else { continue }
            let cov = BuoyCoverage(
                boiaId: boiaId, endpoint: "standard",
                startTs: s, endTs: e,
                downloadedAt: Int(Date().timeIntervalSince1970)
            )
            try await store.insertObservations(obs, coverage: cov)
        }

        let rawObs = try await store.fetchObservations(
            boiaId: boiaId, endpoint: "standard", from: from, to: to
        )
        return pivot(rawObs)
    }

    /// Returns `BuoyReading` array for an explicit time window (e.g., the GPS session period).
    func fetchReadings(
        boiaId: Int = AppConfig.buoyID,
        from: Date,
        to: Date,
        store: BuoyStore
    ) async throws -> [BuoyReading] {
        let fromUnix = Int(from.timeIntervalSince1970)
        let toUnix   = Int(to.timeIntervalSince1970)

        let missing = try await store.missingRanges(
            boiaId: boiaId, endpoint: "standard", from: fromUnix, to: toUnix
        )
        for (s, e) in missing {
            let obs = try await fetchStandard(boiaId: boiaId, from: s, to: e)
            guard !obs.isEmpty else { continue }
            let cov = BuoyCoverage(
                boiaId: boiaId, endpoint: "standard",
                startTs: s, endTs: e,
                downloadedAt: Int(Date().timeIntervalSince1970)
            )
            try await store.insertObservations(obs, coverage: cov)
        }

        let rawObs = try await store.fetchObservations(
            boiaId: boiaId, endpoint: "standard", from: fromUnix, to: toUnix
        )
        return pivot(rawObs)
    }

    // MARK: - Network

    private func fetchStandard(boiaId: Int, from: Int, to: Int) async throws -> [BuoyObservation] {
        var comps = URLComponents(
            url: baseURL.appending(path: "intrans_data"),
            resolvingAgainstBaseURL: true
        )!
        comps.queryItems = [
            URLQueryItem(name: "boiaID", value: "\(boiaId)"),
            URLQueryItem(name: "type",   value: "json"),
            URLQueryItem(name: "time1",  value: "\(from)"),
            URLQueryItem(name: "time2",  value: "\(to)"),
            URLQueryItem(name: "params", value: Self.standardParams),
        ]
        let (data, _) = try await session.data(from: comps.url!)
        return parseStandard(data: data, boiaId: boiaId)
    }

    // MARK: - Parsing

    private func parseStandard(data: Data, boiaId: Int) -> [BuoyObservation] {
        guard let rows = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] else {
            return []
        }
        var result: [BuoyObservation] = []
        for row in rows {
            guard let tsStr = row["timestamp"] as? String,
                  let ts = parseTimestamp(tsStr) else { continue }
            for (key, val) in row where !Self.metaCols.contains(key) {
                let value: Double?
                switch val {
                case let d as Double: value = d
                case let i as Int:    value = Double(i)
                case let s as String: value = Double(s)
                default:              value = nil
                }
                result.append(BuoyObservation(
                    boiaId: boiaId, ts: ts, endpoint: "standard",
                    variable: key, value: value
                ))
            }
        }
        return result
    }

    /// Parses ISO8601 timestamps returned by SIMCOSTA (UTC, with or without fractional seconds).
    private func parseTimestamp(_ s: String) -> Int? {
        // With fractional seconds: "2026-02-25T03:21:40.000Z"
        let isoFrac = ISO8601DateFormatter()
        isoFrac.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        if let date = isoFrac.date(from: s) { return Int(date.timeIntervalSince1970) }
        // Without fractional seconds: "2026-02-25T03:21:40Z"
        let iso = ISO8601DateFormatter()
        if let date = iso.date(from: s) { return Int(date.timeIntervalSince1970) }
        // No Z suffix fallback
        let fmt = DateFormatter()
        fmt.locale   = Locale(identifier: "en_US_POSIX")
        fmt.timeZone = TimeZone(identifier: "UTC")
        fmt.dateFormat = "yyyy-MM-dd'T'HH:mm:ss"
        let stripped = s.replacingOccurrences(of: "Z", with: "")
        return fmt.date(from: stripped).map { Int($0.timeIntervalSince1970) }
    }

    // MARK: - Pivot long → BuoyReading

    private func pivot(_ obs: [BuoyObservation]) -> [BuoyReading] {
        var byTs: [Int: [String: Double]] = [:]
        for o in obs {
            guard let v = o.value else { continue }
            byTs[o.ts, default: [:]][o.variable] = v
        }
        return byTs.keys.sorted().map { ts in
            let v = byTs[ts]!
            return BuoyReading(
                ts: Date(timeIntervalSince1970: TimeInterval(ts)),
                windSpeedMps:        v["Avg_Wnd_Sp"],
                windDirectionDeg:    v["Avg_Wnd_Dir_N"],
                // C_Avg_Spd is in mm/s → m/s
                currentSpeedMps:     v["C_Avg_Spd"].map { $0 * 0.001 },
                currentDirectionDeg: v["C_Avg_Dir_N"]
            )
        }
    }

    // MARK: - Day bounds (America/Bahia)

    private func dayBounds(_ date: Date) -> (Int, Int) {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = AppConfig.timezone
        let start = cal.startOfDay(for: date)
        let end   = cal.date(byAdding: .day, value: 1, to: start)!.addingTimeInterval(-1)
        return (Int(start.timeIntervalSince1970), Int(end.timeIntervalSince1970))
    }
}
