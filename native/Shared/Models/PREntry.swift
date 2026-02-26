import Foundation

struct PREntry: Identifiable {
    var id: Double { distanceM }
    let distanceM: Double
    let predictedTimeSec: Double
    let predictedTimeFmt: String     // "M:SS.s"
    let avgSpeedKmh: Double
    let bearing: Double
    let sessionDate: Date
    // Condition-adjusted (nil if no buoy data for that date)
    let stwBuoyKmh: Double?
    let buoyCurrentComponentKmh: Double?
    let windComponentKmh: Double?
    // Context
    let sessionCount: Int            // sessions with data for this distance
    let previousBestSec: Double?     // 2nd-best time (for improvement display)
}
