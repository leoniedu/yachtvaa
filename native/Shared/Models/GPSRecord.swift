import Foundation

struct GPSRecord {
    let exerciseID: Int
    let athleteID: Int
    let ts: Date
    let positionLat: Int    // Garmin semicircles
    let positionLon: Int    // Garmin semicircles
    let distanceM: Double?
    let speedMps: Double?
    let heartRate: Int?
    // Derived
    var latDeg: Double { Double(positionLat) * (180.0 / pow(2, 31)) }
    var lonDeg: Double { Double(positionLon) * (180.0 / pow(2, 31)) }
}
