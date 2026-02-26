import Foundation
import GRDB

final class AppDatabase {
    static let shared = try! AppDatabase()

    let dbQueue: DatabaseQueue

    init() throws {
        let appSupport = FileManager.default
            .urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appending(component: "YachtVAA")
        try FileManager.default.createDirectory(at: appSupport, withIntermediateDirectories: true)
        let dbURL = appSupport.appending(component: "yachtvaa.sqlite")

        var config = Configuration()
        config.prepareDatabase { db in
            try db.execute(sql: "PRAGMA journal_mode=WAL")
            try db.execute(sql: "PRAGMA foreign_keys=ON")
        }
        dbQueue = try DatabaseQueue(path: dbURL.path, configuration: config)
        try migrate()
    }

    // MARK: - Migrations

    private func migrate() throws {
        var migrator = DatabaseMigrator()

        migrator.registerMigration("v1") { db in
            // Athletes discovered by probing the team
            try db.create(table: "athletes") { t in
                t.column("id", .integer).primaryKey()
                t.column("team_id", .integer).notNull()
                t.column("full_name", .text).notNull()
                t.column("synced_at", .double).notNull()
            }

            // Exercise metadata from GetLastExerciseDone
            try db.create(table: "exercises") { t in
                t.column("id", .integer).primaryKey()
                t.column("athlete_id", .integer).notNull()
                    .references("athletes", onDelete: .cascade)
                t.column("team_id", .integer).notNull()
                t.column("genre_name", .text)
                t.column("start", .text).notNull()
                t.column("distance_m", .double)
                t.column("elapsed_sec", .double)
                t.column("extra_json", .text)
            }
            try db.create(
                index: "idx_exercises_athlete_start",
                on: "exercises",
                columns: ["athlete_id", "start"]
            )
            try db.create(
                index: "idx_exercises_start",
                on: "exercises",
                columns: ["start"]
            )

            // Full ExerciseAnalysis API response (raw JSON preserved)
            try db.create(table: "analysis_raw") { t in
                t.column("exercise_id", .integer).primaryKey()
                    .references("exercises", onDelete: .cascade)
                t.column("athlete_id", .integer).notNull()
                t.column("raw_json", .text).notNull()
                t.column("downloaded_at", .text).notNull()
            }

            // GPS records parsed from analysis JSON
            try db.create(table: "records") { t in
                t.autoIncrementedPrimaryKey("id")
                t.column("exercise_id", .integer).notNull()
                    .references("exercises", onDelete: .cascade)
                t.column("athlete_id", .integer).notNull()
                t.column("ts", .integer).notNull()
                t.column("position_lat", .integer)
                t.column("position_lon", .integer)
                t.column("distance_m", .double)
                t.column("speed_mps", .double)
                t.column("heart_rate", .integer)
            }
            try db.create(
                index: "idx_records_athlete_ts",
                on: "records",
                columns: ["athlete_id", "ts"]
            )
            try db.create(
                index: "idx_records_exercise_id",
                on: "records",
                columns: ["exercise_id"]
            )

            // SIMCOSTA buoy observations (long format, mirrors rsimcosta schema)
            try db.create(table: "buoy_observations") { t in
                t.column("boia_id", .integer).notNull()
                t.column("ts", .integer).notNull()
                t.column("endpoint", .text).notNull()
                t.column("variable", .text).notNull()
                t.column("value", .double)
                t.primaryKey(["boia_id", "ts", "endpoint", "variable"])
            }

            // Coverage windows — tracks which time ranges have been fetched
            try db.create(table: "buoy_coverage") { t in
                t.autoIncrementedPrimaryKey("rowid")
                t.column("boia_id", .integer).notNull()
                t.column("endpoint", .text).notNull()
                t.column("start_ts", .integer).notNull()
                t.column("end_ts", .integer).notNull()
                t.column("downloaded_at", .integer).notNull()
            }
            try db.create(
                index: "idx_buoy_coverage",
                on: "buoy_coverage",
                columns: ["boia_id", "endpoint", "start_ts", "end_ts"]
            )
        }

        // Exercise IDs in Treinus are per-athlete (not globally unique).
        // Drop and recreate exercises / analysis_raw / records with composite PKs.
        migrator.registerMigration("v2") { db in
            try db.drop(table: "records")
            try db.drop(table: "analysis_raw")
            try db.drop(table: "exercises")

            try db.create(table: "exercises") { t in
                t.column("id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                    .references("athletes", onDelete: .cascade)
                t.column("team_id", .integer).notNull()
                t.column("genre_name", .text)
                t.column("start", .text).notNull()
                t.column("distance_m", .double)
                t.column("elapsed_sec", .double)
                t.column("extra_json", .text)
                t.primaryKey(["id", "athlete_id"])
            }
            try db.create(index: "idx_exercises_athlete_start",
                          on: "exercises", columns: ["athlete_id", "start"])
            try db.create(index: "idx_exercises_start",
                          on: "exercises", columns: ["start"])

            try db.create(table: "analysis_raw") { t in
                t.column("exercise_id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                t.column("raw_json", .text).notNull()
                t.column("downloaded_at", .text).notNull()
                t.primaryKey(["exercise_id", "athlete_id"])
                t.foreignKey(["exercise_id", "athlete_id"],
                             references: "exercises", columns: ["id", "athlete_id"],
                             onDelete: .cascade)
            }

            try db.create(table: "records") { t in
                t.autoIncrementedPrimaryKey("id")
                t.column("exercise_id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                t.column("ts", .integer).notNull()
                t.column("position_lat", .integer)
                t.column("position_lon", .integer)
                t.column("distance_m", .double)
                t.column("speed_mps", .double)
                t.column("heart_rate", .integer)
                t.foreignKey(["exercise_id", "athlete_id"],
                             references: "exercises", columns: ["id", "athlete_id"],
                             onDelete: .cascade)
            }
            try db.create(index: "idx_records_athlete_ts",
                          on: "records", columns: ["athlete_id", "ts"])
            try db.create(index: "idx_records_exercise_id",
                          on: "records", columns: ["exercise_id"])
        }

        // Exercise IDs are unique per (id, athlete_id, team_id).
        // Add team_id to exercises PK and carry it into analysis_raw/records.
        migrator.registerMigration("v3") { db in
            try db.drop(table: "records")
            try db.drop(table: "analysis_raw")
            try db.drop(table: "exercises")

            try db.create(table: "exercises") { t in
                t.column("id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                    .references("athletes", onDelete: .cascade)
                t.column("team_id", .integer).notNull()
                t.column("genre_name", .text)
                t.column("start", .text).notNull()
                t.column("distance_m", .double)
                t.column("elapsed_sec", .double)
                t.column("extra_json", .text)
                t.primaryKey(["id", "athlete_id", "team_id"])
            }
            try db.create(index: "idx_exercises_athlete_start",
                          on: "exercises", columns: ["athlete_id", "start"])
            try db.create(index: "idx_exercises_start",
                          on: "exercises", columns: ["start"])

            try db.create(table: "analysis_raw") { t in
                t.column("exercise_id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                t.column("team_id", .integer).notNull()
                t.column("raw_json", .text).notNull()
                t.column("downloaded_at", .text).notNull()
                t.primaryKey(["exercise_id", "athlete_id", "team_id"])
                t.foreignKey(["exercise_id", "athlete_id", "team_id"],
                             references: "exercises", columns: ["id", "athlete_id", "team_id"],
                             onDelete: .cascade)
            }

            try db.create(table: "records") { t in
                t.autoIncrementedPrimaryKey("id")
                t.column("exercise_id", .integer).notNull()
                t.column("athlete_id", .integer).notNull()
                t.column("team_id", .integer).notNull()
                t.column("ts", .integer).notNull()
                t.column("position_lat", .integer)
                t.column("position_lon", .integer)
                t.column("distance_m", .double)
                t.column("speed_mps", .double)
                t.column("heart_rate", .integer)
                t.foreignKey(["exercise_id", "athlete_id", "team_id"],
                             references: "exercises", columns: ["id", "athlete_id", "team_id"],
                             onDelete: .cascade)
            }
            try db.create(index: "idx_records_athlete_ts",
                          on: "records", columns: ["athlete_id", "ts"])
            try db.create(index: "idx_records_exercise_id",
                          on: "records", columns: ["exercise_id"])
            // Covers the bbox query: WHERE ts BETWEEN ? AND ? AND position_lat/lon in range
            try db.create(index: "idx_records_ts_bbox",
                          on: "records", columns: ["ts", "position_lat", "position_lon"])
        }

        try migrator.migrate(dbQueue)
    }

    // MARK: - Reset

    /// Removes the SQLite file and its WAL/SHM companions. Called by "Limpar tudo".
    static func destroy() throws {
        let appSupport = FileManager.default
            .urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appending(component: "YachtVAA")
        let base = appSupport.appending(component: "yachtvaa.sqlite").path
        for suffix in ["", "-wal", "-shm"] {
            let path = base + suffix
            if FileManager.default.fileExists(atPath: path) {
                try FileManager.default.removeItem(atPath: path)
            }
        }
    }
}
