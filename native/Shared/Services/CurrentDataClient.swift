import Foundation

enum CurrentDataError: LocalizedError {
    case notFound(dateStr: String, statusCode: Int)
    case parseError(String)

    var errorDescription: String? {
        switch self {
        case .notFound(let d, let code):
            return "GeoJSON para \(d) não encontrado (HTTP \(code))"
        case .parseError(let msg):
            return "Erro ao decodificar GeoJSON: \(msg)"
        }
    }
}

/// Downloads UV GeoJSON files from GitHub releases and caches them on disk.
actor CurrentDataClient {
    static let shared = CurrentDataClient()

    private let session = URLSession.shared

    // Release tag for the GeoJSON assets (separate from the GRIB release)
    private static let releaseTag = "geojson\(AppConfig.geojsonResolutionCode)"

    // MARK: - Public API

    /// Returns the current grid for `date` (Bahia calendar day), using disk cache.
    /// Throws `CurrentDataError.notFound` if the file doesn't exist on GitHub yet.
    func fetchGrid(for date: Date) async throws -> CurrentGrid {
        let dateStr = bahiaDateString(date)
        let cacheURL = FileCache.currentGridURL(date: dateStr)

        // Return cached file if present
        if FileManager.default.fileExists(atPath: cacheURL.path) {
            let data = try Data(contentsOf: cacheURL)
            return try decode(data: data, for: date)
        }

        // Download from GitHub releases
        let url = Self.releaseURL(for: dateStr)
        let (data, response) = try await session.data(from: url)
        let statusCode = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard statusCode == 200 else {
            throw CurrentDataError.notFound(dateStr: dateStr, statusCode: statusCode)
        }

        // Persist to disk
        try FileCache.prepare()
        try data.write(to: cacheURL, options: .atomic)

        return try decode(data: data, for: date)
    }

    // MARK: - URL

    private static func releaseURL(for dateStr: String) -> URL {
        let filename = "\(AppConfig.siscorarArea)_\(dateStr)_\(AppConfig.geojsonResolutionCode).geojson"
        return URL(string: "https://github.com/leoniedu/siscorar_gribs/releases/download/\(releaseTag)/\(filename)")!
    }

    // MARK: - Decoding

    private func decode(data: Data, for date: Date) throws -> CurrentGrid {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            throw CurrentDataError.parseError("root is not an object")
        }
        let hoursUTC  = (root["hours_utc"] as? [Int]) ?? Array(0..<24)
        let features  = (root["features"]  as? [[String: Any]]) ?? []

        let points: [GridPoint] = features.compactMap { feature in
            guard
                let geom   = feature["geometry"]   as? [String: Any],
                let coords = geom["coordinates"]   as? [Double], coords.count >= 2,
                let props  = feature["properties"] as? [String: Any]
            else { return nil }

            return GridPoint(
                lat: coords[1],
                lon: coords[0],
                u: readDoubleArray(props["u"]),
                v: readDoubleArray(props["v"]),
                s: readDoubleArray(props["s"]),
                d: readDoubleArray(props["d"])
            )
        }

        return CurrentGrid(date: date, hoursUTC: hoursUTC, points: points)
    }

    /// Coerces a JSON array of numbers (Int or Double) to `[Double?]`.
    private func readDoubleArray(_ value: Any?) -> [Double?] {
        guard let arr = value as? [Any] else { return [] }
        return arr.map { elem -> Double? in
            switch elem {
            case let d as Double: return d
            case let i as Int:    return Double(i)
            default:              return nil
            }
        }
    }

    // MARK: - Date formatting

    /// Date string in YYYYMMDD format using the Bahia (local) calendar day.
    private func bahiaDateString(_ date: Date) -> String {
        let fmt = DateFormatter()
        fmt.locale   = Locale(identifier: "en_US_POSIX")
        fmt.timeZone = AppConfig.timezone
        fmt.dateFormat = "yyyyMMdd"
        return fmt.string(from: date)
    }
}
