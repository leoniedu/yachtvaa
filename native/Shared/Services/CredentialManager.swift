import Foundation
import KeychainAccess

/// Manages Treinus credentials in the Keychain, with env-var override for tests.
///
/// Priority:
/// 1. Environment variables `TREINUS_EMAIL`, `TREINUS_PASSWORD`, `TREINUS_TEAM_ID`
///    (set in Xcode scheme → Run/Test → Environment Variables, or in shell for CI)
/// 2. Keychain (set by `saveCredentials` from SettingsView)
struct CredentialManager {

    private static let keychain = Keychain(service: "com.eleon.yachtvaa")

    struct Credentials {
        let email: String
        let password: String
        let teamId: Int?
    }

    /// Returns credentials from env vars (tests/CI) or Keychain (production).
    static func load() -> Credentials? {
        // 1. Env vars — same names as rtreinus R package
        let env = ProcessInfo.processInfo.environment
        if let email    = env["TREINUS_EMAIL"],
           let password = env["TREINUS_PASSWORD"],
           !email.isEmpty, !password.isEmpty {
            let teamId = env["TREINUS_TEAM_ID"].flatMap(Int.init)
            return Credentials(email: email, password: password, teamId: teamId)
        }

        // 2. Keychain
        guard let email    = keychain["treinus.email"],
              let password = keychain["treinus.password"],
              !email.isEmpty else { return nil }
        let teamId = keychain["treinus.teamId"].flatMap(Int.init)
        return Credentials(email: email, password: password, teamId: teamId)
    }

    static func save(email: String, password: String, teamId: Int?) throws {
        try keychain.set(email,    key: "treinus.email")
        try keychain.set(password, key: "treinus.password")
        if let teamId {
            try keychain.set("\(teamId)", key: "treinus.teamId")
        } else {
            try keychain.remove("treinus.teamId")
        }
    }

    static func clear() throws {
        try keychain.remove("treinus.email")
        try keychain.remove("treinus.password")
        try keychain.remove("treinus.teamId")
    }

    static var hasCredentials: Bool { load() != nil }
}
