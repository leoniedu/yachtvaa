import Foundation

struct LeagueEntry: Identifiable {
    let id: Int  // athleteID
    let rank: Int
    let athlete: Athlete
    let predictedTimeSec: Double   // raw seconds, used for sorting
    let predictedTimeFmt: String   // "MM:SS.s"
    let avgSpeedKmh: Double
    /// Along-track component (km/h). Negative = opposing, positive = assisting.
    let windComponent: Double?          // km/h
    let siscorarCurrentComponent: Double?  // km/h
    let buoyCurrentComponent: Double?   // km/h
    /// Speed through water = GPS speed − current component. km/h.
    let speedThroughWaterSiscorarKmh: Double?
    let speedThroughWaterBuoyKmh: Double?
    let bearing: Double
}
