import Foundation

// Step: Sort, rank, format MM:SS.s
enum LeagueBuilder {
    static func build(segments: [ConditionedSegment], athletes: [Int: Athlete]) -> [LeagueEntry] {
        let sorted = segments.sorted { $0.segment.predictedTimeSec < $1.segment.predictedTimeSec }
        return sorted.enumerated().compactMap { idx, cs in
            guard let athlete = athletes[cs.segment.athleteID] else { return nil }
            let kmh = cs.segment.avgSpeedKmh
            // Components come from ApparentConditions in m/s → convert to km/h
            let windKmh     = cs.windComponent.map          { $0 * 3.6 }
            let siscorarKmh = cs.siscorarCurrentComponent.map { $0 * 3.6 }
            let buoyKmh     = cs.buoyCurrentComponent.map   { $0 * 3.6 }
            // Speed through water: GPS speed minus the along-track current component.
            // If the tide is following (+), STW < GPS speed; if opposing (−), STW > GPS speed.
            let stwSiscorar = siscorarKmh.map { kmh - $0 }
            let stwBuoy     = buoyKmh.map     { kmh - $0 }
            return LeagueEntry(
                id:    cs.segment.athleteID,
                rank:  idx + 1,
                athlete: athlete,
                predictedTimeSec:               cs.segment.predictedTimeSec,
                predictedTimeFmt:               formatTime(cs.segment.predictedTimeSec),
                avgSpeedKmh:                    kmh,
                windComponent:                  windKmh,
                siscorarCurrentComponent:       siscorarKmh,
                buoyCurrentComponent:           buoyKmh,
                speedThroughWaterSiscorarKmh:   stwSiscorar,
                speedThroughWaterBuoyKmh:       stwBuoy,
                bearing:                        cs.segment.bearing,
                startTime:                      cs.segment.startTime
            )
        }
    }

    static func formatTime(_ seconds: Double) -> String {
        let m = Int(seconds) / 60
        let s = seconds - Double(m * 60)
        return String(format: "%d:%04.1f", m, s)
    }
}
