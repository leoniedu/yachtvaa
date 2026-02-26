import Foundation
import SwiftSoup

// MARK: - Errors

enum TreinusError: LocalizedError {
    case missingCredentials
    case loginFailed(String)
    case teamSelectionRequired([TreinusTeam])
    case apiError(String)
    case unexpectedResponse(String)

    var errorDescription: String? {
        switch self {
        case .missingCredentials:
            return "Treinus credentials not configured"
        case .loginFailed(let msg):
            return "Login failed: \(msg)"
        case .teamSelectionRequired(let teams):
            return "Team selection required. Available: \(teams.map(\.name).joined(separator: ", "))"
        case .apiError(let msg):
            return "API error: \(msg)"
        case .unexpectedResponse(let msg):
            return "Unexpected response: \(msg)"
        }
    }
}

// MARK: - Response models

struct TreinusTeam: Identifiable {
    let id: Int
    let name: String
}

struct TreinusExercise: Identifiable {
    let id: Int
    let athleteId: Int
    let teamId: Int
    let genreName: String?
    let start: String?              // ISO8601 local time
    let distanceM: Double?
    let totalElapsedTimeSec: Double?
    let rawJSON: [String: Any]

    /// Decode from one element of the `data` array returned by GetLastExerciseDone.
    /// The API uses mixed-case field names; we probe both PascalCase and snake_case.
    init?(json: [String: Any], athleteId: Int, teamId: Int) {
        guard let id = Self.intField(json, "id_exercise", "Id_Exercise", "IdExercise") else {
            return nil
        }
        self.id        = id
        self.athleteId = athleteId
        self.teamId    = teamId
        self.genreName = Self.strField(json, "genre_name", "Genre_Name", "GenreName")
        self.start     = Self.strField(json, "start", "Start")
        self.distanceM = Self.dblField(json, "distance", "Distance")
        self.totalElapsedTimeSec = Self.dblField(
            json, "total_elapsed_time", "Total_Elapsed_Time", "TotalElapsedTime"
        )
        self.rawJSON = json
    }
}

struct TreinusAnalysis {
    let exerciseId: Int
    let athleteId: Int
    let athleteName: String?
    let records: [TreinusRecord]
    let distanceUnit: String?   // "m" or "km"
    let speedUnit: String?      // "m/s" or "km/h"
    let rawJSON: [String: Any]

    init(json: [String: Any], exerciseId: Int) {
        self.exerciseId   = exerciseId
        self.athleteId    = (json["IdAthlete"] as? Int) ?? 0
        self.distanceUnit = json["DistanceUnit"] as? String
        self.speedUnit    = json["SpeedUnit"]    as? String
        self.athleteName  = (json["User"] as? [String: Any])?["FullName"] as? String
        self.records      = (json["Records"] as? [[String: Any]] ?? []).map { TreinusRecord(json: $0) }
        self.rawJSON      = json
    }
}

struct TreinusRecord {
    let timestamp: Int?         // Unix seconds
    let positionLat: Int?       // Garmin semicircles
    let positionLong: Int?      // Garmin semicircles ("position_long" in rtreinus after clean_names)
    let distance: Double?
    let speed: Double?
    let heartRate: Int?

    var latDeg: Double?  { positionLat.map  { Double($0) * (180.0 / pow(2, 31)) } }
    var lonDeg: Double?  { positionLong.map { Double($0) * (180.0 / pow(2, 31)) } }

    init(json: [String: Any]) {
        // Timestamp is "yyyy-MM-dd HH:mm:ss" in Bahia local time (not Unix int)
        let tsRaw = json["timestamp"] ?? json["Timestamp"]
        if let i = tsRaw as? Int {
            timestamp = i
        } else if let s = tsRaw as? String {
            timestamp = Self.parseTsString(s)
        } else {
            timestamp = nil
        }
        positionLat  = (json["position_lat"]  ?? json["PositionLat"])   as? Int
        positionLong = (json["position_long"] ?? json["PositionLong"])  as? Int
        distance     = (json["distance"]      ?? json["Distance"])      as? Double
        speed        = (json["speed"]         ?? json["Speed"])         as? Double
        heartRate    = (json["heart_rate"]    ?? json["HeartRate"])     as? Int
    }

    private static let tsFmt: DateFormatter = {
        let f = DateFormatter()
        f.locale   = Locale(identifier: "en_US_POSIX")
        f.timeZone = TimeZone(identifier: "UTC")!  // "yyyy-MM-dd HH:mm:ss" strings from Treinus are UTC
        f.dateFormat = "yyyy-MM-dd HH:mm:ss"
        return f
    }()

    private static func parseTsString(_ s: String) -> Int? {
        tsFmt.date(from: s).map { Int($0.timeIntervalSince1970) }
    }
}

// MARK: - Client

actor TreinusClient {

    static let shared = TreinusClient()

    private let session: URLSession
    private let baseURL = URL(string: "https://webapp.treinus.com.br")!
    private(set) var authenticatedTeamId: Int?
    private var cookieString: String = ""

    init() {
        // Isolated cookie store — does not share cookies with Safari or other apps.
        let config = URLSessionConfiguration.default
        config.httpCookieAcceptPolicy = .always
        config.httpShouldSetCookies   = true
        config.httpCookieStorage      = HTTPCookieStorage()
        session = URLSession(configuration: config)
    }

    // MARK: - Authentication

    /// Login with email + password. Mirrors the rtreinus R flow exactly:
    /// - Login POST uses followlocation=FALSE (captured via StopRedirectDelegate)
    /// - Cookies are extracted from the 302 Set-Cookie headers
    /// - Redirect is followed manually with an explicit Cookie header
    /// - Team selection POST uses the same explicit cookie string
    func login(email: String, password: String, teamId: Int? = 2994) async throws {
        // 1. GET login page → __VIEWSTATE tokens
        let loginURL = baseURL.appending(path: "Default.aspx")
        let (loginPageData, _) = try await session.data(from: loginURL)
        let loginPageHTML = String(data: loginPageData, encoding: .utf8) ?? ""

        let doc          = try SwiftSoup.parse(loginPageHTML)
        let viewstate    = try doc.select("input[name='__VIEWSTATE']").attr("value")
        let viewstateGen = try doc.select("input[name='__VIEWSTATEGENERATOR']").attr("value")
        guard !viewstate.isEmpty else {
            throw TreinusError.unexpectedResponse("Missing __VIEWSTATE on login page")
        }

        // 2. POST login form — do NOT follow the 302 redirect (mirrors R's followlocation=FALSE).
        let pfx = "ctl00$ctl00$ctl00$FormContent$FormContent$body$LoginControl1$LoginUser$"
        var loginReq = URLRequest(url: loginURL)
        loginReq.httpMethod = "POST"
        loginReq.setValue("application/x-www-form-urlencoded", forHTTPHeaderField: "Content-Type")
        loginReq.httpBody = formEncode([
            "__EVENTTARGET"        : "",
            "__EVENTARGUMENT"      : "",
            "__VIEWSTATE"          : viewstate,
            "__VIEWSTATEGENERATOR" : viewstateGen,
            "\(pfx)UserName"       : email,
            "\(pfx)Password"       : password,
            "\(pfx)RememberMe"     : "C",
            "\(pfx)LoginButton"    : "Entrar",
        ])

        let (_, rawResp) = try await session.data(for: loginReq, delegate: StopRedirectDelegate())
        guard let resp302 = rawResp as? HTTPURLResponse else {
            throw TreinusError.unexpectedResponse("Login POST response not HTTP")
        }
        guard resp302.statusCode < 400 else {
            throw TreinusError.loginFailed("Login returned HTTP \(resp302.statusCode)")
        }

        // 3. Extract cookies from the 302 Set-Cookie headers (mirrors R's cookie_string).
        let cookies = extractCookieString(from: resp302)
        self.cookieString = cookies

        // 4. Resolve and follow the redirect manually with an explicit Cookie header.
        guard let locationRaw = resp302.allHeaderFields["Location"] as? String else {
            throw TreinusError.loginFailed("Login POST did not redirect — check credentials")
        }
        let redirectURL = resolveURL(locationRaw, base: baseURL)

        var redirectReq = URLRequest(url: redirectURL)
        if !cookies.isEmpty { redirectReq.setValue(cookies, forHTTPHeaderField: "Cookie") }
        let (redirectData, redirectRawResp) = try await session.data(for: redirectReq)
        let finalURL     = (redirectRawResp as? HTTPURLResponse)?.url?.absoluteString ?? redirectURL.absoluteString
        let redirectHTML = String(data: redirectData, encoding: .utf8) ?? ""

        // 5. Inspect landing page — same logic as rtreinus.
        if finalURL.contains("LoginSelectTeam") {
            let teams = try parseTeams(from: redirectHTML)
            guard let wantedId = teamId ?? teams.first?.id else {
                throw TreinusError.teamSelectionRequired(teams)
            }
            try await selectTeam(wantedId, html: redirectHTML,
                                 referer: redirectURL, cookies: cookies)
        } else if !finalURL.contains("Default.aspx") {
            // Any non-login destination is success (R doesn't check the URL either).
            authenticatedTeamId = teamId
        } else {
            throw TreinusError.loginFailed("Login failed — credentials may be incorrect")
        }
    }

    private func selectTeam(_ teamId: Int, html: String, referer: URL, cookies: String) async throws {
        let doc          = try SwiftSoup.parse(html)
        let viewstate    = try doc.select("input[name='__VIEWSTATE']").attr("value")
        let viewstateGen = try doc.select("input[name='__VIEWSTATEGENERATOR']").attr("value")

        let pfx = "ctl00$ctl00$ctl00$FormContent$FormContent$body$"
        let url = baseURL.appending(path: "LoginSelectTeam.aspx")
        var req = URLRequest(url: url)
        req.httpMethod = "POST"
        req.setValue("application/x-www-form-urlencoded", forHTTPHeaderField: "Content-Type")
        // Explicit Cookie header (mirrors R's req_headers(Cookie = cookies, ...))
        if !cookies.isEmpty { req.setValue(cookies, forHTTPHeaderField: "Cookie") }
        req.setValue(referer.absoluteString, forHTTPHeaderField: "Referer")
        req.setValue(baseURL.absoluteString,  forHTTPHeaderField: "Origin")
        req.httpBody = formEncode([
            "__EVENTTARGET"        : "",
            "__EVENTARGUMENT"      : "",
            "__VIEWSTATE"          : viewstate,
            "__VIEWSTATEGENERATOR" : viewstateGen,
            "\(pfx)rblTeam"        : "\(teamId)",
            "\(pfx)LoginButton"    : "Entrar",
        ])

        let (_, resp) = try await session.data(for: req)
        let final = (resp as? HTTPURLResponse)?.url?.absoluteString ?? ""
        // R doesn't check the final URL at all; we only fail if still on an auth page.
        let stillOnAuthPage = final.contains("Default.aspx") || final.contains("LoginSelectTeam")
        guard !stillOnAuthPage else {
            throw TreinusError.loginFailed("Team selection with id \(teamId) failed. URL: \(final)")
        }
        authenticatedTeamId = teamId
        self.cookieString = cookies
    }

    // MARK: - Cookie helpers (mirrors R's vapply + paste(collapse="; "))

    /// Extracts `name=value` pairs from the Set-Cookie response header.
    /// `allHeaderFields` collapses multiple Set-Cookie values with ", " — safe for
    /// ASP.NET cookies whose values (Base64/hex) never contain commas.
    private func extractCookieString(from response: HTTPURLResponse) -> String {
        guard let combined = response.allHeaderFields["Set-Cookie"] as? String,
              !combined.isEmpty
        else {
            // Fall back to anything the session's cookie jar already has.
            return session.configuration.httpCookieStorage?
                .cookies(for: response.url ?? baseURL)?
                .map { "\($0.name)=\($0.value)" }
                .joined(separator: "; ") ?? ""
        }
        // Each "Set-Cookie: name=value; attr1; attr2" separated by ", " between cookies.
        return combined
            .components(separatedBy: ", ")
            .map { $0.components(separatedBy: ";")[0].trimmingCharacters(in: .whitespaces) }
            .filter { $0.contains("=") }
            .joined(separator: "; ")
    }

    /// Resolves a relative or absolute Location header to a full URL.
    private func resolveURL(_ location: String, base: URL) -> URL {
        if location.hasPrefix("http") { return URL(string: location) ?? base }
        let path = location.hasPrefix("/") ? String(location.dropFirst()) : location
        return base.appending(path: path)
    }

    private func parseTeams(from html: String) throws -> [TreinusTeam] {
        let doc    = try SwiftSoup.parse(html)
        let radios = try doc.select("input[name$='rblTeam']").array()
        let labels = try doc.select("label[for^='FormContent_FormContent_body_rblTeam']").array()
        return radios.enumerated().compactMap { i, radio in
            guard let id = Int((try? radio.attr("value")) ?? "") else { return nil }
            let name = (try? labels[safe: i]?.text()) ?? ""
            return TreinusTeam(id: id, name: name)
        }
    }

    // MARK: - Auth helper

    private func applyAuth(to req: inout URLRequest) {
        if !cookieString.isEmpty {
            req.setValue(cookieString, forHTTPHeaderField: "Cookie")
        }
    }

    // MARK: - Exercises

    /// Returns exercises for one athlete in the team.
    /// An empty result means the athlete ID is not in the team (useful for probing).
    func getExercises(athleteId: Int, teamId: Int) async throws -> [TreinusExercise] {
        let url = baseURL.appending(path: "Athlete/Exercise/GetLastExerciseDone")
        var comps = URLComponents(url: url, resolvingAgainstBaseURL: true)!
        comps.queryItems = [
            URLQueryItem(name: "idAthlete", value: "\(athleteId)"),
            URLQueryItem(name: "idTeam",    value: "\(teamId)"),
        ]
        var req = URLRequest(url: comps.url!)
        req.setValue("application/json", forHTTPHeaderField: "Accept")
        applyAuth(to: &req)

        let (data, _) = try await session.data(for: req)
        guard let json = try JSONSerialization.jsonObject(with: data) as? [String: Any],
              let arr  = json["data"] as? [[String: Any]] else { return [] }
        return arr.compactMap { TreinusExercise(json: $0, athleteId: athleteId, teamId: teamId) }
    }

    // MARK: - Exercise analysis

    func getExerciseAnalysis(exerciseId: Int, athleteId: Int, teamId: Int) async throws -> TreinusAnalysis {
        let url = baseURL.appending(path: "Athlete/ExerciseSheet/Done/ExerciseAnalysis")
        var comps = URLComponents(url: url, resolvingAgainstBaseURL: true)!
        comps.queryItems = [
            URLQueryItem(name: "idTeam",         value: "\(teamId)"),
            URLQueryItem(name: "idAthlete",      value: "\(athleteId)"),
            URLQueryItem(name: "idExerciseDone", value: "\(exerciseId)"),
        ]
        var req = URLRequest(url: comps.url!)
        req.setValue("application/json", forHTTPHeaderField: "Accept")
        applyAuth(to: &req)

        let (data, _) = try await session.data(for: req)
        guard let json = try JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            throw TreinusError.unexpectedResponse("Cannot parse analysis response")
        }
        if let success = json["success"] as? Bool, !success {
            throw TreinusError.apiError((json["message"] as? String) ?? "Unknown error")
        }
        guard let dataObj  = json["data"]       as? [String: Any],
              let analysis = dataObj["Analysis"] as? [String: Any] else {
            throw TreinusError.unexpectedResponse("Missing data.Analysis in response")
        }
        return TreinusAnalysis(json: analysis, exerciseId: exerciseId)
    }

    // MARK: - Athlete discovery

    /// Probe IDs 1…`maxId` in concurrent batches to find team members.
    /// Stops after `maxConsecutiveMisses` consecutive empty results.
    func probeAthletes(
        teamId: Int,
        maxId: Int = 300,
        batchSize: Int = 20,
        maxConsecutiveMisses: Int = 10
    ) async throws -> [Int] {
        var found: [Int] = []
        var consecutiveMisses = 0
        var currentId = 1

        probeLoop: while currentId <= maxId, consecutiveMisses < maxConsecutiveMisses {
            let batchEnd = min(currentId + batchSize - 1, maxId)
            let batchIDs = Array(currentId...batchEnd)

            // Fire requests concurrently. Actor suspension during each
            // `await session.data(for:)` allows true network parallelism.
            let results = try await withThrowingTaskGroup(of: (Int, Bool).self) { group in
                for id in batchIDs {
                    group.addTask {
                        let exs = try await self.getExercises(athleteId: id, teamId: teamId)
                        return (id, !exs.isEmpty)
                    }
                }
                var pairs: [(Int, Bool)] = []
                for try await pair in group { pairs.append(pair) }
                return pairs.sorted { $0.0 < $1.0 }
            }

            for (id, exists) in results {
                if exists { found.append(id); consecutiveMisses = 0 }
                else       { consecutiveMisses += 1 }
                if consecutiveMisses >= maxConsecutiveMisses { break probeLoop }
            }
            currentId = batchEnd + 1
        }
        return found
    }

    // MARK: - Private helpers

    /// URL-encodes a form body. `$` is kept unencoded in keys (browser convention;
    /// ASP.NET accepts both `$` and `%24`).
    private func formEncode(_ params: [String: String]) -> Data {
        let keySet: CharacterSet = .alphanumerics.union(.init(charactersIn: "-._~$"))
        let valSet: CharacterSet = .alphanumerics.union(.init(charactersIn: "-._~"))
        let body = params.map { k, v -> String in
            let ek = k.addingPercentEncoding(withAllowedCharacters: keySet) ?? k
            let ev = v.addingPercentEncoding(withAllowedCharacters: valSet) ?? v
            return "\(ek)=\(ev)"
        }.joined(separator: "&")
        return Data(body.utf8)
    }
}

// MARK: - Stop-redirect delegate (mirrors R's req_options(followlocation = FALSE))

/// Task-level delegate that cancels the automatic redirect so the caller can
/// capture the 302 response and extract its Set-Cookie headers manually.
private final class StopRedirectDelegate: NSObject, URLSessionTaskDelegate, @unchecked Sendable {
    func urlSession(
        _ session: URLSession,
        task: URLSessionTask,
        willPerformHTTPRedirection response: HTTPURLResponse,
        newRequest request: URLRequest,
        completionHandler: @escaping (URLRequest?) -> Void
    ) {
        completionHandler(nil) // cancel redirect, return the 302 response body
    }
}

// MARK: - JSON field helpers (probe multiple key variants for mixed-case API)

private extension TreinusExercise {
    static func intField(_ j: [String: Any], _ keys: String...) -> Int? {
        for k in keys {
            if let v = j[k] as? Int    { return v }
            if let v = j[k] as? String { return Int(v) }
        }
        return nil
    }
    static func strField(_ j: [String: Any], _ keys: String...) -> String? {
        keys.compactMap { j[$0] as? String }.first
    }
    static func dblField(_ j: [String: Any], _ keys: String...) -> Double? {
        for k in keys {
            if let v = j[k] as? Double { return v }
            if let v = j[k] as? Int    { return Double(v) }
        }
        return nil
    }
}

// MARK: - Safe array subscript

private extension Array {
    subscript(safe index: Int) -> Element? {
        indices.contains(index) ? self[index] : nil
    }
}
