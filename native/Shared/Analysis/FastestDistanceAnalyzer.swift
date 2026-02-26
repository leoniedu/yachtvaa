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

        struct Pt {
            let lat: Double
            let lon: Double
            let ts: Double  // Unix seconds
        }

        let pts = records.map { r in
            Pt(lat: r.latDeg, lon: r.lonDeg, ts: r.ts.timeIntervalSince1970)
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

        // For each start i, find the first j where haversine distance >= distanceM,
        // compute predicted time for exactly distanceM, keep the minimum.
        // Mirrors .fastest_distance_vectorized() in R.
        var bestTPred = Double.infinity
        var bestStart: Pt?
        var bestEnd: Pt?
        var bestDist = 0.0

        for seg in segments {
            let n = seg.count
            guard n >= 2 else { continue }
            for i in 0 ..< (n - 1) {
                let pi = seg[i]
                for j in (i + 1) ..< n {
                    let pj = seg[j]
                    let d = CoordinateConverter.haversineDistance(
                        lat1: pi.lat, lon1: pi.lon,
                        lat2: pj.lat, lon2: pj.lon
                    )
                    guard d >= distanceM else { continue }
                    let dtIJ = pj.ts - pi.ts
                    guard dtIJ > 0 else { break }
                    let tPred = (distanceM * dtIJ) / d
                    if tPred < bestTPred {
                        bestTPred = tPred
                        bestStart = pi
                        bestEnd = pj
                        bestDist = d
                    }
                    break // first j that clears the threshold
                }
            }
        }

        guard let start = bestStart, let end = bestEnd, !bestTPred.isInfinite else {
            return nil
        }

        let dtIJ = end.ts - start.ts

        return FastestSegment(
            athleteID: athleteID,
            predictedTimeSec: bestTPred,
            bearing: CoordinateConverter.bearing(
                lat1: start.lat, lon1: start.lon,
                lat2: end.lat,   lon2: end.lon
            ),
            avgSpeedKmh: (bestDist / dtIJ) * 3.6,
            startCoord: CLLocationCoordinate2D(latitude: start.lat, longitude: start.lon),
            endCoord:   CLLocationCoordinate2D(latitude: end.lat,   longitude: end.lon),
            startTime:  Date(timeIntervalSince1970: start.ts),
            endTime:    Date(timeIntervalSince1970: end.ts),
            distanceM:  distanceM
        )
    }
}
