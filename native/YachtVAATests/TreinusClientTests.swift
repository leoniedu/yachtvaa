import XCTest
@testable import YachtVAA

/// Integration tests for TreinusClient.
///
/// Credentials are read from environment variables — the same names used by the
/// rtreinus R package (.Renviron or shell):
///
///   TREINUS_EMAIL     your Treinus login email
///   TREINUS_PASSWORD  your Treinus password
///   TREINUS_TEAM_ID   integer team ID (optional if single-team account)
///   TREINUS_ATHLETE_ID  a known athlete ID in the team (for analysis test)
///
/// In Xcode: scheme → Edit Scheme → Test → Environment Variables.
/// In CI: set as shell env vars before running `xcodebuild test`.
final class TreinusClientTests: XCTestCase {

    // MARK: - Helpers

    private var env: [String: String] { ProcessInfo.processInfo.environment }

    private func skipUnlessCredentials() throws {
        try XCTSkipUnless(
            env["TREINUS_EMAIL"] != nil && env["TREINUS_PASSWORD"] != nil,
            "Set TREINUS_EMAIL and TREINUS_PASSWORD environment variables to run live tests"
        )
    }

    private func makeClient() -> TreinusClient { TreinusClient() }

    // MARK: - CredentialManager

    func testCredentialManagerReadsEnvVars() {
        guard let email    = env["TREINUS_EMAIL"],
              let password = env["TREINUS_PASSWORD"] else {
            // Nothing to check without env vars — pass silently.
            return
        }
        let creds = CredentialManager.load()
        XCTAssertNotNil(creds, "CredentialManager should return credentials from env vars")
        XCTAssertEqual(creds?.email,    email)
        XCTAssertEqual(creds?.password, password)
        if let teamIdStr = env["TREINUS_TEAM_ID"], let teamId = Int(teamIdStr) {
            XCTAssertEqual(creds?.teamId, teamId)
        }
    }

    // MARK: - Login

    func testLoginSucceeds() async throws {
        try skipUnlessCredentials()
        let client = makeClient()
        let email    = env["TREINUS_EMAIL"]!
        let password = env["TREINUS_PASSWORD"]!
        let teamId   = env["TREINUS_TEAM_ID"].flatMap(Int.init)

        try await client.login(email: email, password: password, teamId: teamId)

        let storedTeamId = await client.authenticatedTeamId
        if let teamId {
            XCTAssertEqual(storedTeamId, teamId, "authenticatedTeamId should match supplied teamId")
        }
    }

    func testLoginFailsWithWrongPassword() async throws {
        let client = makeClient()
        do {
            try await client.login(email: "fake@example.com", password: "wrong")
            XCTFail("Expected loginFailed error")
        } catch TreinusError.loginFailed {
            // Expected
        } catch {
            XCTFail("Unexpected error type: \(error)")
        }
    }

    // MARK: - Exercises

    func testGetExercisesReturnsResultsForKnownAthlete() async throws {
        try skipUnlessCredentials()
        guard let athleteIdStr = env["TREINUS_ATHLETE_ID"],
              let athleteId    = Int(athleteIdStr),
              let teamIdStr    = env["TREINUS_TEAM_ID"],
              let teamId       = Int(teamIdStr) else {
            throw XCTSkip("Set TREINUS_ATHLETE_ID and TREINUS_TEAM_ID to run this test")
        }

        let client = makeClient()
        let email    = env["TREINUS_EMAIL"]!
        let password = env["TREINUS_PASSWORD"]!
        try await client.login(email: email, password: password, teamId: teamId)

        let exercises = try await client.getExercises(athleteId: athleteId, teamId: teamId)
        XCTAssertFalse(exercises.isEmpty, "Known athlete should have exercises")

        // Spot-check structure
        let ex = exercises[0]
        XCTAssertGreaterThan(ex.id, 0)
        XCTAssertEqual(ex.athleteId, athleteId)
    }

    func testGetExercisesReturnsEmptyForUnknownAthlete() async throws {
        try skipUnlessCredentials()
        guard let teamIdStr = env["TREINUS_TEAM_ID"],
              let teamId    = Int(teamIdStr) else {
            throw XCTSkip("Set TREINUS_TEAM_ID to run this test")
        }

        let client = makeClient()
        try await client.login(
            email:    env["TREINUS_EMAIL"]!,
            password: env["TREINUS_PASSWORD"]!,
            teamId:   teamId
        )
        // ID 0 should not exist in any team
        let exercises = try await client.getExercises(athleteId: 0, teamId: teamId)
        XCTAssertTrue(exercises.isEmpty, "ID 0 should return empty (not in team)")
    }

    // MARK: - Exercise analysis

    func testGetExerciseAnalysisReturnsRecords() async throws {
        try skipUnlessCredentials()
        guard let athleteIdStr  = env["TREINUS_ATHLETE_ID"],
              let athleteId     = Int(athleteIdStr),
              let teamIdStr     = env["TREINUS_TEAM_ID"],
              let teamId        = Int(teamIdStr) else {
            throw XCTSkip("Set TREINUS_ATHLETE_ID and TREINUS_TEAM_ID to run this test")
        }

        let client = makeClient()
        try await client.login(
            email:    env["TREINUS_EMAIL"]!,
            password: env["TREINUS_PASSWORD"]!,
            teamId:   teamId
        )

        // Fetch exercise list to get a valid exercise ID
        let exercises = try await client.getExercises(athleteId: athleteId, teamId: teamId)
        guard let exercise = exercises.first else {
            throw XCTSkip("No exercises available for TREINUS_ATHLETE_ID")
        }

        let analysis = try await client.getExerciseAnalysis(
            exerciseId: exercise.id,
            athleteId:  athleteId,
            teamId:     teamId
        )

        XCTAssertFalse(analysis.records.isEmpty, "Analysis should have GPS records")
        XCTAssertEqual(analysis.athleteId, athleteId)

        // Spot-check coordinate decoding
        let rec = analysis.records.first { $0.positionLat != nil && $0.positionLong != nil }
        if let rec {
            // Baia de Todos os Santos is roughly -12° to -13° S, -38° to -39° W
            let lat = rec.latDeg ?? 0
            let lon = rec.lonDeg ?? 0
            XCTAssertTrue((-90...90).contains(lat),   "lat \(lat) out of range")
            XCTAssertTrue((-180...180).contains(lon),  "lon \(lon) out of range")
        }
    }

    // MARK: - TreinusRecord decoding

    func testRecordDecodesSnakeCaseFields() {
        let json: [String: Any] = [
            "timestamp":     1_740_470_400,
            "position_lat":  -154_000_000,
            "position_long": -434_000_000,
            "distance":      100.0,
            "speed":         1.5,
            "heart_rate":    120,
        ]
        let rec = TreinusRecord(json: json)
        XCTAssertEqual(rec.timestamp,    1_740_470_400)
        XCTAssertEqual(rec.positionLat,  -154_000_000)
        XCTAssertEqual(rec.positionLong, -434_000_000)
        XCTAssertEqual(rec.distance,     100.0)
        XCTAssertEqual(rec.speed,        1.5)
        XCTAssertEqual(rec.heartRate,    120)
    }

    func testRecordDecodesPascalCaseFields() {
        let json: [String: Any] = [
            "Timestamp":    1_740_470_400,
            "PositionLat":  -154_000_000,
            "PositionLong": -434_000_000,
            "Distance":     200.0,
            "Speed":        2.0,
            "HeartRate":    130,
        ]
        let rec = TreinusRecord(json: json)
        XCTAssertEqual(rec.timestamp,    1_740_470_400)
        XCTAssertEqual(rec.positionLat,  -154_000_000)
        XCTAssertEqual(rec.positionLong, -434_000_000)
    }

    func testRecordSemicirclesConversion() {
        // Known: -154_000_000 semicircles ≈ -12.91°S
        let json: [String: Any] = ["position_lat": -154_000_000, "position_long": -456_500_000]
        let rec = TreinusRecord(json: json)
        XCTAssertEqual(rec.latDeg ?? 0, -154_000_000.0 * (180.0 / pow(2, 31)), accuracy: 1e-6)
        XCTAssertEqual(rec.lonDeg ?? 0, -456_500_000.0 * (180.0 / pow(2, 31)), accuracy: 1e-6)
    }

    // MARK: - TreinusExercise decoding

    func testExerciseDecodesSnakeCaseId() {
        let json: [String: Any] = ["id_exercise": 42, "start": "2025-02-25T08:00:00"]
        let ex = TreinusExercise(json: json, athleteId: 1, teamId: 99)
        XCTAssertNotNil(ex)
        XCTAssertEqual(ex?.id, 42)
        XCTAssertEqual(ex?.start, "2025-02-25T08:00:00")
    }

    func testExerciseDecodesStringId() {
        let json: [String: Any] = ["id_exercise": "123"]
        let ex = TreinusExercise(json: json, athleteId: 1, teamId: 99)
        XCTAssertEqual(ex?.id, 123)
    }

    func testExerciseReturnsNilWithoutId() {
        let json: [String: Any] = ["genre_name": "Remo"]
        let ex = TreinusExercise(json: json, athleteId: 1, teamId: 99)
        XCTAssertNil(ex)
    }
}
