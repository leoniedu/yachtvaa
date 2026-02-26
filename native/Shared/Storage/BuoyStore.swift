import Foundation
import GRDB

// MARK: - DB record types

/// One variable measurement from the SIMCOSTA buoy (long format).
struct BuoyObservation: Codable, FetchableRecord {
    var boiaId: Int
    var ts: Int             // Unix seconds
    var endpoint: String    // "standard" | "currents"
    var variable: String    // "wind_speed", "wind_direction", etc.
    var value: Double?

    enum CodingKeys: String, CodingKey {
        case boiaId   = "boia_id"
        case ts, endpoint, variable, value
    }
}

/// A time window that has already been downloaded from SIMCOSTA.
struct BuoyCoverage: Codable, FetchableRecord {
    var boiaId: Int
    var endpoint: String
    var startTs: Int        // Unix seconds
    var endTs: Int          // Unix seconds
    var downloadedAt: Int   // Unix seconds

    enum CodingKeys: String, CodingKey {
        case boiaId       = "boia_id"
        case endpoint
        case startTs      = "start_ts"
        case endTs        = "end_ts"
        case downloadedAt = "downloaded_at"
    }
}

// MARK: - Store

struct BuoyStore {
    let dbQueue: DatabaseQueue

    // MARK: - Coverage queries (mirrors rsimcosta gap-filling logic)

    func fetchCoverage(boiaId: Int, endpoint: String) async throws -> [BuoyCoverage] {
        try await dbQueue.read { db in
            try BuoyCoverage.fetchAll(
                db,
                sql: """
                    SELECT * FROM buoy_coverage
                    WHERE boia_id = ? AND endpoint = ?
                    ORDER BY start_ts
                    """,
                arguments: [boiaId, endpoint]
            )
        }
    }

    /// Computes time ranges not covered by existing windows.
    func missingRanges(
        boiaId: Int,
        endpoint: String,
        from: Int,
        to: Int
    ) async throws -> [(Int, Int)] {
        let coverage = try await fetchCoverage(boiaId: boiaId, endpoint: endpoint)
        var missing: [(Int, Int)] = []
        var cursor = from
        for window in coverage {
            if window.startTs > cursor {
                missing.append((cursor, min(window.startTs - 1, to)))
            }
            cursor = max(cursor, window.endTs + 1)
            if cursor > to { break }
        }
        if cursor <= to { missing.append((cursor, to)) }
        return missing
    }

    // MARK: - Observations

    func fetchObservations(
        boiaId: Int,
        endpoint: String,
        from: Int,
        to: Int
    ) async throws -> [BuoyObservation] {
        try await dbQueue.read { db in
            try BuoyObservation.fetchAll(
                db,
                sql: """
                    SELECT * FROM buoy_observations
                    WHERE boia_id = ? AND endpoint = ? AND ts BETWEEN ? AND ?
                    ORDER BY ts, variable
                    """,
                arguments: [boiaId, endpoint, from, to]
            )
        }
    }

    // MARK: - Write

    /// Bulk-inserts (or replaces) observations and appends a coverage window.
    func insertObservations(
        _ observations: [BuoyObservation],
        coverage: BuoyCoverage
    ) async throws {
        try await dbQueue.write { db in
            for obs in observations {
                try db.execute(
                    sql: """
                        INSERT OR REPLACE INTO buoy_observations
                        (boia_id, ts, endpoint, variable, value)
                        VALUES (?, ?, ?, ?, ?)
                        """,
                    arguments: [obs.boiaId, obs.ts, obs.endpoint, obs.variable, obs.value]
                )
            }
            try db.execute(
                sql: """
                    INSERT INTO buoy_coverage
                    (boia_id, endpoint, start_ts, end_ts, downloaded_at)
                    VALUES (?, ?, ?, ?, ?)
                    """,
                arguments: [
                    coverage.boiaId,
                    coverage.endpoint,
                    coverage.startTs,
                    coverage.endTs,
                    coverage.downloadedAt,
                ]
            )
        }
    }

    // MARK: - Delete

    func deleteAll() async throws {
        try await dbQueue.write { db in
            try db.execute(sql: "DELETE FROM buoy_observations")
            try db.execute(sql: "DELETE FROM buoy_coverage")
        }
    }
}
