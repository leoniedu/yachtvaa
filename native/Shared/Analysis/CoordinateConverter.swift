import Foundation

enum CoordinateConverter {
    /// Garmin semicircles to decimal degrees
    static func semicirclesToDegrees(_ semicircles: Int32) -> Double {
        Double(semicircles) * (180.0 / pow(2, 31))
    }

    /// Haversine great-circle distance between two WGS84 points, in meters.
    static func haversineDistance(
        lat1: Double, lon1: Double,
        lat2: Double, lon2: Double
    ) -> Double {
        let R = 6_371_000.0
        let phi1 = lat1 * .pi / 180
        let phi2 = lat2 * .pi / 180
        let dphi = (lat2 - lat1) * .pi / 180
        let dlam = (lon2 - lon1) * .pi / 180
        let a = sin(dphi / 2) * sin(dphi / 2)
              + cos(phi1) * cos(phi2) * sin(dlam / 2) * sin(dlam / 2)
        let c = 2 * atan2(sqrt(a), sqrt(1 - a))
        return R * c
    }

    /// Forward azimuth bearing from point 1 to point 2, degrees clockwise from North [0, 360).
    static func bearing(
        lat1: Double, lon1: Double,
        lat2: Double, lon2: Double
    ) -> Double {
        let phi1 = lat1 * .pi / 180
        let phi2 = lat2 * .pi / 180
        let dlam = (lon2 - lon1) * .pi / 180
        let y = sin(dlam) * cos(phi2)
        let x = cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlam)
        let b = atan2(y, x) * 180 / .pi
        return (b + 360).truncatingRemainder(dividingBy: 360)
    }
}
