import Foundation
import CoreLocation

struct FastestSegment {
    let athleteID: Int
    let predictedTimeSec: Double
    let bearing: Double         // degrees clockwise from North
    let avgSpeedKmh: Double
    let startCoord: CLLocationCoordinate2D
    let endCoord: CLLocationCoordinate2D
    let startTime: Date
    let endTime: Date
    let distanceM: Double
}
