import Foundation

/// One grid point from the UV GeoJSON (one per geographic location, 24-hour arrays).
struct GridPoint {
    let lat: Double
    let lon: Double
    /// Eastward velocity m/s per hour (index = UTC hour)
    let u: [Double?]
    /// Northward velocity m/s per hour
    let v: [Double?]
    /// Speed m/s per hour
    let s: [Double?]
    /// Direction degrees clockwise from North per hour
    let d: [Double?]
}

struct CurrentGrid {
    let date: Date
    let hoursUTC: [Int]
    let points: [GridPoint]
}
