import Foundation

/// Matches FastestSegments with environmental data from SISCORAR grid and SIMCOSTA buoy.
enum EnvironmentalMatcher {

    // MARK: - SISCORAR current (inverse-distance-weighted from 4 surrounding points)

    /// Interpolates u/v from the grid at each segment's midpoint and nearest UTC hour.
    static func matchCurrent(
        segments: [FastestSegment],
        grid: CurrentGrid
    ) -> [ConditionedSegment] {
        let step = gridStep()
        var lookup: [LatLonKey: GridPoint] = [:]
        for pt in grid.points {
            let key = LatLonKey(lat: quantize(pt.lat, step: step),
                                lon: quantize(pt.lon, step: step))
            lookup[key] = pt
        }

        return segments.map { seg in
            let midLat = (seg.startCoord.latitude  + seg.endCoord.latitude)  / 2
            let midLon = (seg.startCoord.longitude + seg.endCoord.longitude) / 2
            let hIdx   = nearestHourIndex(time: seg.startTime, hoursUTC: grid.hoursUTC)

            var u: Double?
            var v: Double?
            if let h = hIdx {
                (u, v) = interpolateUV(lat: midLat, lon: midLon,
                                       step: step, hIdx: h, lookup: lookup)
            }

            var speed: Double?
            var dir:   Double?
            if let uv = u, let vv = v {
                speed = sqrt(uv * uv + vv * vv)
                dir   = (atan2(uv, vv) * 180 / .pi + 360).truncatingRemainder(dividingBy: 360)
            }

            return ConditionedSegment(
                segment: seg,
                currentU: u, currentV: v,
                currentSpeedMps: speed, currentDirDeg: dir,
                buoyWindSpeedMps: nil, buoyWindDirDeg: nil,
                buoyCurrentSpeedMps: nil, buoyCurrentDirDeg: nil,
                siscorarCurrentComponent: nil,
                buoyCurrentComponent: nil, windComponent: nil
            )
        }
    }

    // MARK: - SIMCOSTA buoy (nearest-time join, tolerance ≤ 2 h)

    static func matchBuoy(
        segments: [ConditionedSegment],
        readings: [BuoyReading]
    ) -> [ConditionedSegment] {
        let maxGap: TimeInterval = 2 * 3_600
        guard !readings.isEmpty else { return segments }
        let sorted = readings.sorted { $0.ts < $1.ts }

        return segments.map { seg in
            let t = seg.segment.startTime
            let nearest = sorted.min { abs($0.ts.timeIntervalSince(t)) < abs($1.ts.timeIntervalSince(t)) }
            guard let r = nearest, abs(r.ts.timeIntervalSince(t)) <= maxGap else { return seg }
            return ConditionedSegment(
                segment: seg.segment,
                currentU: seg.currentU, currentV: seg.currentV,
                currentSpeedMps: seg.currentSpeedMps, currentDirDeg: seg.currentDirDeg,
                buoyWindSpeedMps:    r.windSpeedMps,
                buoyWindDirDeg:      r.windDirectionDeg,
                buoyCurrentSpeedMps: r.currentSpeedMps,
                buoyCurrentDirDeg:   r.currentDirectionDeg,
                siscorarCurrentComponent: seg.siscorarCurrentComponent,
                buoyCurrentComponent: seg.buoyCurrentComponent,
                windComponent: seg.windComponent
            )
        }
    }

    // MARK: - Conditions table helper

    /// Returns (speedMps, dirDeg) keyed by UTC hour at a single lat/lon point.
    static func hourlyCurrentAtPoint(
        lat: Double, lon: Double, grid: CurrentGrid
    ) -> [Int: (speedMps: Double, dirDeg: Double)] {
        let step = gridStep()
        var lookup: [LatLonKey: GridPoint] = [:]
        for pt in grid.points {
            let key = LatLonKey(lat: quantize(pt.lat, step: step),
                                lon: quantize(pt.lon, step: step))
            lookup[key] = pt
        }
        var result: [Int: (speedMps: Double, dirDeg: Double)] = [:]
        for (idx, hour) in grid.hoursUTC.enumerated() {
            let (u, v) = interpolateUV(lat: lat, lon: lon, step: step, hIdx: idx, lookup: lookup)
            guard let uv = u, let vv = v else { continue }
            let speed = sqrt(uv * uv + vv * vv)
            let dir   = (atan2(uv, vv) * 180 / .pi + 360).truncatingRemainder(dividingBy: 360)
            result[hour] = (speed, dir)
        }
        return result
    }

    // MARK: - Helpers

    private struct LatLonKey: Hashable {
        let lat: Double
        let lon: Double
    }

    private static func quantize(_ v: Double, step: Double) -> Double {
        (v / step).rounded() * step
    }

    /// SISCORAR grid step in degrees, derived from AppConfig.geojsonResolutionCode.
    /// "005" → 0.005°
    private static func gridStep() -> Double {
        let code = AppConfig.geojsonResolutionCode
        if let n = Double(code), code.count > 0 {
            return n / pow(10.0, Double(code.count - 1))
        }
        return 0.005
    }

    /// Inverse-distance–weighted interpolation from the 4 surrounding grid cells.
    private static func interpolateUV(
        lat: Double, lon: Double,
        step: Double, hIdx: Int,
        lookup: [LatLonKey: GridPoint]
    ) -> (u: Double?, v: Double?) {
        let latL = floor(lat / step) * step
        let lonL = floor(lon / step) * step

        let corners: [(Double, Double)] = [
            (latL,        lonL),
            (latL,        lonL + step),
            (latL + step, lonL),
            (latL + step, lonL + step)
        ]

        var uVals: [Double] = []
        var vVals: [Double] = []
        var wts:   [Double] = []

        for (cLat, cLon) in corners {
            let key = LatLonKey(lat: quantize(cLat, step: step),
                                lon: quantize(cLon, step: step))
            guard let pt = lookup[key],
                  hIdx < pt.u.count, hIdx < pt.v.count,
                  let u = pt.u[hIdx], let v = pt.v[hIdx]
            else { continue }
            let dist = max(1e-9, sqrt(pow(lat - cLat, 2) + pow(lon - cLon, 2)))
            uVals.append(u); vVals.append(v); wts.append(1 / dist)
        }

        guard !uVals.isEmpty else { return (nil, nil) }
        let wSum = wts.reduce(0, +)
        let uOut = zip(uVals, wts).reduce(0.0) { $0 + $1.0 * $1.1 } / wSum
        let vOut = zip(vVals, wts).reduce(0.0) { $0 + $1.0 * $1.1 } / wSum
        return (uOut, vOut)
    }

    private static func nearestHourIndex(time: Date, hoursUTC: [Int]) -> Int? {
        guard !hoursUTC.isEmpty else { return nil }
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "UTC")!
        let hour = cal.component(.hour, from: time)
        return hoursUTC.indices.min { abs(hoursUTC[$0] - hour) < abs(hoursUTC[$1] - hour) }
    }
}

