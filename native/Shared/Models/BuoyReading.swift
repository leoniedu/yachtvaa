import Foundation

struct BuoyReading {
    let ts: Date
    let windSpeedMps: Double?
    let windDirectionDeg: Double?    // FROM convention
    let currentSpeedMps: Double?
    let currentDirectionDeg: Double? // TO convention
}
