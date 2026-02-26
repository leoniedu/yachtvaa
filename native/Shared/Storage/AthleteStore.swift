import Foundation
import GRDB

struct AthleteStore {
    let dbQueue: DatabaseQueue

    // MARK: - Read

    func fetchAll(teamId: Int) async throws -> [Athlete] {
        try await dbQueue.read { db in
            try Athlete
                .filter(Column("team_id") == teamId)
                .order(Column("id"))
                .fetchAll(db)
        }
    }

    // MARK: - Write

    /// Upserts a batch of athletes in a single transaction.
    /// Uses save() (UPDATE if exists, INSERT if new) instead of INSERT OR REPLACE
    /// to avoid cascade-deleting child exercises/analysis_raw rows.
    func upsertAll(_ athletes: [Athlete]) async throws {
        try await dbQueue.write { db in
            for athlete in athletes {
                try athlete.save(db)
            }
        }
    }

    /// Deletes all athletes (cascades to exercises and records).
    func deleteAll() async throws {
        try await dbQueue.write { db in
            try db.execute(sql: "DELETE FROM athletes")
        }
    }
}
