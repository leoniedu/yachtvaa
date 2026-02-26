import Foundation

// Step 8: Port of wind-current.R trig
// See /Users/eleon/github/yachtvaa/R/wind-current.R
enum ApparentConditions {
    /// Angle between vessel bearing and wind FROM direction (degrees).
    static func relativeAngle(vesselBearing: Double, sourceDirFrom: Double) -> Double {
        var angle = sourceDirFrom - vesselBearing
        while angle > 180  { angle -= 360 }
        while angle < -180 { angle += 360 }
        return angle
    }

    /// Angle between vessel bearing and current TO direction.
    static func relativeCurrentAngle(vesselBearing: Double, currentDirTo: Double) -> Double {
        relativeAngle(vesselBearing: vesselBearing, sourceDirFrom: currentDirTo)
    }

    /// Wind component along vessel axis (negative = headwind).
    static func windComponent(speedMps: Double, angle: Double) -> Double {
        -speedMps * cos(angle * .pi / 180)
    }

    /// Current component along vessel axis (positive = following).
    static func currentComponent(speedMps: Double, angle: Double) -> Double {
        speedMps * cos(angle * .pi / 180)
    }

    /// Fills all apparent-condition fields on a ConditionedSegment.
    static func apply(to seg: ConditionedSegment) -> ConditionedSegment {
        let bearing = seg.segment.bearing

        var siscorarComp: Double?
        if let speed = seg.currentSpeedMps, let dir = seg.currentDirDeg {
            let angle = relativeCurrentAngle(vesselBearing: bearing, currentDirTo: dir)
            siscorarComp = currentComponent(speedMps: speed, angle: angle)
        }

        var buoyCurrentComp: Double?
        if let speed = seg.buoyCurrentSpeedMps, let dir = seg.buoyCurrentDirDeg {
            let angle = relativeCurrentAngle(vesselBearing: bearing, currentDirTo: dir)
            buoyCurrentComp = currentComponent(speedMps: speed, angle: angle)
        }

        var windComp: Double?
        if let speed = seg.buoyWindSpeedMps, let dir = seg.buoyWindDirDeg {
            let angle = relativeAngle(vesselBearing: bearing, sourceDirFrom: dir)
            windComp = windComponent(speedMps: speed, angle: angle)
        }

        return ConditionedSegment(
            segment: seg.segment,
            currentU: seg.currentU, currentV: seg.currentV,
            currentSpeedMps: seg.currentSpeedMps, currentDirDeg: seg.currentDirDeg,
            buoyWindSpeedMps: seg.buoyWindSpeedMps, buoyWindDirDeg: seg.buoyWindDirDeg,
            buoyCurrentSpeedMps: seg.buoyCurrentSpeedMps, buoyCurrentDirDeg: seg.buoyCurrentDirDeg,
            siscorarCurrentComponent: siscorarComp,
            buoyCurrentComponent: buoyCurrentComp,
            windComponent: windComp
        )
    }
}
