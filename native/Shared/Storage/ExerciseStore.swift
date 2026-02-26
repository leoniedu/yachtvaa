import Foundation
import GRDB

// MARK: - DB record types

/// Row stored in the `exercises` table.
struct ExerciseRow: Codable, FetchableRecord, PersistableRecord {
    static let databaseTableName = "exercises"

    var id: Int
    var athleteId: Int
    var teamId: Int
    var genreName: String?
    var start: String           // ISO8601 local time from Treinus API
    var distanceM: Double?
    var elapsedSec: Double?
    var extraJson: String?      // full Treinus JSON entry, serialised

    enum CodingKeys: String, CodingKey {
        case id
        case athleteId  = "athlete_id"
        case teamId     = "team_id"
        case genreName  = "genre_name"
        case start
        case distanceM  = "distance_m"
        case elapsedSec = "elapsed_sec"
        case extraJson  = "extra_json"
    }
}

/// Row stored in `analysis_raw` — one per exercise.
struct AnalysisRaw: Codable, FetchableRecord, PersistableRecord {
    static let databaseTableName = "analysis_raw"

    var exerciseId: Int
    var athleteId: Int
    var teamId: Int
    var rawJson: String         // full data.Analysis JSON
    var downloadedAt: String    // ISO8601

    enum CodingKeys: String, CodingKey {
        case exerciseId  = "exercise_id"
        case athleteId   = "athlete_id"
        case teamId      = "team_id"
        case rawJson     = "raw_json"
        case downloadedAt = "downloaded_at"
    }
}

/// Row stored in `records` — GPS points parsed from analysis JSON.
struct RecordRow: Codable, FetchableRecord, MutablePersistableRecord {
    static let databaseTableName = "records"

    var id: Int?                // nil before insert (AUTOINCREMENT)
    var exerciseId: Int
    var athleteId: Int
    var teamId: Int
    var ts: Int                 // Unix seconds
    var positionLat: Int?       // Garmin semicircles
    var positionLon: Int?       // Garmin semicircles
    var distanceM: Double?
    var speedMps: Double?
    var heartRate: Int?

    mutating func didInsert(_ inserted: InsertionSuccess) {
        id = Int(inserted.rowID)
    }

    enum CodingKeys: String, CodingKey {
        case id
        case exerciseId  = "exercise_id"
        case athleteId   = "athlete_id"
        case teamId      = "team_id"
        case ts
        case positionLat = "position_lat"
        case positionLon = "position_lon"
        case distanceM   = "distance_m"
        case speedMps    = "speed_mps"
        case heartRate   = "heart_rate"
    }
}

// MARK: - Store

struct ExerciseStore {
    let dbQueue: DatabaseQueue

    // MARK: - Read

    func hasAnalysis(exerciseId: Int, athleteId: Int, teamId: Int) async throws -> Bool {
        try await dbQueue.read { db in
            try AnalysisRaw
                .filter(Column("exercise_id") == exerciseId
                     && Column("athlete_id") == athleteId
                     && Column("team_id") == teamId)
                .fetchCount(db) > 0
        }
    }

    /// Returns exercises whose `start` column begins with `datePrefix` (e.g. "2025-02-25").
    func fetchExercises(athleteIds: [Int], datePrefix: String) async throws -> [ExerciseRow] {
        try await dbQueue.read { db in
            try ExerciseRow
                .filter(athleteIds.contains(Column("athlete_id")))
                .filter(Column("start").like("\(datePrefix)%"))
                .fetchAll(db)
        }
    }

    /// Returns exercises with `start` in [fromPrefix, toPrefix].
    /// Works because `start` is stored as "yyyy-MM-dd …" which sorts lexicographically.
    func fetchExercisesInRange(athleteIds: [Int], from fromPrefix: String, to toPrefix: String) async throws -> [ExerciseRow] {
        try await dbQueue.read { db in
            try ExerciseRow
                .filter(athleteIds.contains(Column("athlete_id")))
                .filter(Column("start") >= fromPrefix)
                .filter(Column("start") <= toPrefix + "~")  // "~" > any digit/letter, caps the day
                .fetchAll(db)
        }
    }

    /// Returns raw GPS rows for an athlete set within a Unix-second time window.
    func fetchRecords(athleteIds: [Int], from: Int, to: Int) async throws -> [RecordRow] {
        try await dbQueue.read { db in
            try RecordRow
                .filter(athleteIds.contains(Column("athlete_id")))
                .filter(Column("ts") >= from && Column("ts") <= to)
                .order(Column("athlete_id"), Column("ts"))
                .fetchAll(db)
        }
    }

    /// Returns distinct athlete IDs that have ≥1 GPS record inside the bbox and time window.
    /// Coordinates must be in Garmin semicircles (same unit as `records.position_lat/lon`).
    func fetchAthleteIdsInBbox(from: Int, to: Int,
                               latMin: Int, latMax: Int,
                               lonMin: Int, lonMax: Int) async throws -> [Int] {
        try await dbQueue.read { db in
            let rows = try Row.fetchAll(db, sql: """
                SELECT DISTINCT athlete_id FROM records
                WHERE ts >= ? AND ts <= ?
                  AND position_lat >= ? AND position_lat <= ?
                  AND position_lon >= ? AND position_lon <= ?
                """, arguments: [from, to, latMin, latMax, lonMin, lonMax])
            return rows.map { $0["athlete_id"] as Int }
        }
    }

    // MARK: - Write

    /// Upserts exercise metadata. Does not overwrite analysis_raw or records.
    func upsertExercises(_ exercises: [TreinusExercise]) async throws {
        let rows = exercises.map { ex -> ExerciseRow in
            ExerciseRow(
                id: ex.id,
                athleteId: ex.athleteId,
                teamId: ex.teamId,
                genreName: ex.genreName,
                start: ex.start ?? "",
                distanceM: ex.distanceM,
                elapsedSec: ex.totalElapsedTimeSec,
                extraJson: jsonString(ex.rawJSON)
            )
        }
        try await dbQueue.write { db in
            for row in rows {
                // save() avoids INSERT OR REPLACE cascade-deleting analysis_raw/records.
                try row.save(db)
            }
        }
    }

    /// Stores analysis JSON, bulk-inserts GPS records, and writes the athlete name — all in one transaction.
    /// Replaces any existing analysis for the exercise (safe to call repeatedly).
    /// The athlete name is written only when the current `athletes` row is a `#id` placeholder.
    func upsertAnalysis(_ analysis: TreinusAnalysis, teamId: Int) async throws {
        let isoNow = ISO8601DateFormatter().string(from: Date())
        let raw = AnalysisRaw(
            exerciseId: analysis.exerciseId,
            athleteId: analysis.athleteId,
            teamId: teamId,
            rawJson: jsonString(analysis.rawJSON) ?? "{}",
            downloadedAt: isoNow
        )
        let records: [RecordRow] = analysis.records.compactMap { r in
            guard let ts = r.timestamp else { return nil }
            return RecordRow(
                id: nil,
                exerciseId: analysis.exerciseId,
                athleteId: analysis.athleteId,
                teamId: teamId,
                ts: ts,
                positionLat: r.positionLat,
                positionLon: r.positionLong,
                distanceM: r.distance,
                speedMps: r.speed,
                heartRate: r.heartRate
            )
        }
        try await dbQueue.write { db in
            try raw.insert(db, onConflict: .replace)
            try db.execute(
                sql: "DELETE FROM records WHERE exercise_id = ? AND athlete_id = ? AND team_id = ?",
                arguments: [analysis.exerciseId, analysis.athleteId, teamId]
            )
            for var record in records {
                try record.insert(db)
            }
            if let name = analysis.athleteName, !name.isEmpty {
                try db.execute(sql: """
                    INSERT INTO athletes (id, team_id, full_name, synced_at)
                    VALUES (?, ?, ?, CAST(strftime('%s','now') AS REAL))
                    ON CONFLICT(id) DO UPDATE
                      SET full_name = excluded.full_name,
                          synced_at = excluded.synced_at
                      WHERE athletes.full_name LIKE '#%'
                    """, arguments: [analysis.athleteId, teamId, name])
            }
        }
    }

    // MARK: - Delete

    /// Clears GPS records and analysis JSON (keeps exercise metadata).
    func deleteRecordsAndAnalysis() async throws {
        try await dbQueue.write { db in
            try db.execute(sql: "DELETE FROM records")
            try db.execute(sql: "DELETE FROM analysis_raw")
        }
    }

    /// Clears everything owned by ExerciseStore.
    func deleteAll() async throws {
        try await dbQueue.write { db in
            try db.execute(sql: "DELETE FROM records")
            try db.execute(sql: "DELETE FROM analysis_raw")
            try db.execute(sql: "DELETE FROM exercises")
        }
    }

    // MARK: - Helper

    private func jsonString(_ dict: [String: Any]) -> String? {
        guard let data = try? JSONSerialization.data(withJSONObject: dict),
              let str = String(data: data, encoding: .utf8) else { return nil }
        return str
    }
}
