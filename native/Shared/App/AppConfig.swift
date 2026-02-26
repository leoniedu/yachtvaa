import CoreLocation

enum AppConfig {
    /// Baia de Todos os Santos bounding box (WGS84)
    static let bboxSSA = (
        minLon: -38.614, minLat: -13.007,
        maxLon: -38.463, maxLat: -12.813
    )
    /// SIMCOSTA buoy ID
    static let buoyID: Int = 515
    /// SIMCOSTA buoy 515 coordinates (WGS84)
    static let buoyLat: Double = -12.9900
    static let buoyLon: Double = -38.5415
    /// Treinus team timezone
    static let timezone = TimeZone(identifier: "America/Bahia")!
    /// UTM zone 24S EPSG code
    static let utmEPSG = 31984
    /// SISCORAR area name used in GitHub release filenames
    static let siscorarArea = "baiatos"
    /// GeoJSON resolution code used in filenames (0.005° → "005")
    static let geojsonResolutionCode = "005"
    /// GitHub repo for cached GeoJSON files
    static let siscorarGitHubRepo = "leoniedu/siscorar_gribs"
    /// Map center for initial region
    static let mapCenter = CLLocationCoordinate2D(latitude: -12.99953, longitude: -38.5308)
    /// Target distance for fastest-segment analysis (metres)
    static let analysisDistanceM: Double = 1_000
    /// Converts WGS84 degrees to Garmin semicircles (unit used in `records.position_lat/lon`).
    static func toSemicircle(_ degrees: Double) -> Int {
        Int((degrees * 2_147_483_648.0 / 180.0).rounded())
    }
}
