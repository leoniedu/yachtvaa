import Foundation

struct ConditionedSegment {
    let segment: FastestSegment
    // SISCORAR current at segment midpoint
    let currentU: Double?
    let currentV: Double?
    let currentSpeedMps: Double?
    let currentDirDeg: Double?
    // SIMCOSTA buoy (nearest time)
    let buoyWindSpeedMps: Double?
    let buoyWindDirDeg: Double?
    let buoyCurrentSpeedMps: Double?
    let buoyCurrentDirDeg: Double?
    // Apparent components (positive = assisting)
    let siscorarCurrentComponent: Double?
    let buoyCurrentComponent: Double?
    let windComponent: Double?
}
