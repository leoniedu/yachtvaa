import Foundation
import GRDB

struct Athlete: Identifiable, Hashable {
    let id: Int
    let teamId: Int
    let fullName: String
    let syncedAt: Date
}

// MARK: - GRDB persistence

extension Athlete: Codable, FetchableRecord, PersistableRecord {
    static let databaseTableName = "athletes"

    enum CodingKeys: String, CodingKey {
        case id
        case teamId   = "team_id"
        case fullName = "full_name"
        case syncedAt = "synced_at"
    }
}
