import Foundation
import CoreLocation

/// Port of fastest_straight_distance() from R/fastest-distance.R.
/// For each set of records (one athlete), finds the segment with the minimum
/// predicted time to travel `distanceM` meters in a straight line.
struct FastestDistanceAnalyzer {
    let distanceM: Double
    /// Maximum gap between consecutive timestamps (seconds) before splitting the track.
    var maxGapSec: Double = 20

    func run(records: [GPSRecord]) -> FastestSegment? {
        guard records.count >= 2 else { return nil }
        let athleteID = records[0].athleteID

        // Flat-earth approximation: convert lat/lon to local meters.
        // Valid for BTS which spans ~30 km.
        let latRef = records.reduce(0.0) { $0 + $1.latDeg } / Double(records.count)
        let metersPerLatDeg = 111_111.0
        let metersPerLonDeg = 111_111.0 * cos(latRef * .pi / 180)

        struct Pt {
            let x: Double   // meters east
            let y: Double   // meters north
            let ts: Double  // Unix seconds
            let lat: Double
            let lon: Double
        }

        let pts = records.map { r in
            Pt(
                x: r.lonDeg * metersPerLonDeg,
                y: r.latDeg * metersPerLatDeg,
                ts: r.ts.timeIntervalSince1970,
                lat: r.latDeg,
                lon: r.lonDeg
            )
        }

        // Split into continuous segments by time gap.
        var segments: [[Pt]] = []
        var current: [Pt] = [pts[0]]
        for idx in 1 ..< pts.count {
            if pts[idx].ts - pts[idx - 1].ts > maxGapSec {
                segments.append(current)
                current = []
            }
            current.append(pts[idx])
        }
        segments.append(current)

        // For each start i, find the first j where straight-line distance >= distanceM,
        // compute predicted time for exactly distanceM, keep the minimum.
        // Mirrors .fastest_distance_vectorized() in R.
        let distSqThreshold = distanceM * distanceM
        var bestTPred = Double.infinity
        var bestStart: Pt?
        var bestEnd: Pt?

        for seg in segments {
            let n = seg.count
            guard n >= 2 else { continue }
            for i in 0 ..< (n - 1) {
                let pi = seg[i]
                for j in (i + 1) ..< n {
                    let pj = seg[j]
                    let dx = pj.x - pi.x
                    let dy = pj.y - pi.y
                    let dSq = dx * dx + dy * dy
                    guard dSq >= distSqThreshold else { continue }
                    let d = sqrt(dSq)
                    let dtIJ = pj.ts - pi.ts
                    guard dtIJ > 0 else { break }
                    let tPred = (distanceM * dtIJ) / d
                    if tPred < bestTPred {
                        bestTPred = tPred
                        bestStart = pi
                        bestEnd = pj
                    }
                    break // first j that clears the threshold
                }
            }
        }

        guard let start = bestStart, let end = bestEnd, !bestTPred.isInfinite else {
            return nil
        }

        let dx = end.x - start.x
        let dy = end.y - start.y
        let d = sqrt(dx * dx + dy * dy)
        let dtIJ = end.ts - start.ts

        return FastestSegment(
            athleteID: athleteID,
            predictedTimeSec: bestTPred,
            bearing: CoordinateConverter.bearing(
                lat1: start.lat, lon1: start.lon,
                lat2: end.lat,   lon2: end.lon
            ),
            avgSpeedKmh: (d / dtIJ) * 3.6,
            startCoord: CLLocationCoordinate2D(latitude: start.lat, longitude: start.lon),
            endCoord:   CLLocationCoordinate2D(latitude: end.lat,   longitude: end.lon),
            startTime:  Date(timeIntervalSince1970: start.ts),
            endTime:    Date(timeIntervalSince1970: end.ts),
            distanceM:  distanceM
        )
    }
}
