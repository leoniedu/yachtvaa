import Foundation

/// Computes all-time personal records at standard distances for one athlete.
enum PRBoardComputer {
    static let distances: [Double] = [200, 500, 1000, 2000]

    /// Loads all GPS data for `athleteId`, runs FastestDistanceAnalyzer at each distance
    /// for each exercise, matches buoy data where available, and returns the best per distance.
    static func compute(
        athleteId: Int,
        exerciseStore: ExerciseStore,
        buoyStore: BuoyStore
    ) async throws -> [PREntry] {
        // 1. Load all GPS records in BTS bbox and group by exercise
        let rows = try await exerciseStore.fetchAllRecordsInBbox(
            athleteId: athleteId,
            latMin: AppConfig.toSemicircle(AppConfig.bboxSSA.minLat),
            latMax: AppConfig.toSemicircle(AppConfig.bboxSSA.maxLat),
            lonMin: AppConfig.toSemicircle(AppConfig.bboxSSA.minLon),
            lonMax: AppConfig.toSemicircle(AppConfig.bboxSSA.maxLon)
        )
        guard !rows.isEmpty else { return [] }

        var byExercise: [Int: [GPSRecord]] = [:]
        for row in rows {
            guard let lat = row.positionLat, let lon = row.positionLon else { continue }
            let record = GPSRecord(
                exerciseID: row.exerciseId, athleteID: row.athleteId,
                ts: Date(timeIntervalSince1970: Double(row.ts)),
                positionLat: lat, positionLon: lon,
                distanceM: row.distanceM, speedMps: row.speedMps, heartRate: row.heartRate
            )
            byExercise[row.exerciseId, default: []].append(record)
        }

        // 2. For each exercise × distance, find fastest segment
        struct SegmentResult {
            let segment: FastestSegment
            let exerciseId: Int
        }

        var resultsByDistance: [Double: [SegmentResult]] = [:]
        for distance in distances {
            resultsByDistance[distance] = []
        }

        let analyzer = { (d: Double) in FastestDistanceAnalyzer(distanceM: d) }
        for (exerciseId, records) in byExercise {
            guard records.count >= 2 else { continue }
            for distance in distances {
                if let seg = analyzer(distance).run(records: records) {
                    resultsByDistance[distance, default: []].append(
                        SegmentResult(segment: seg, exerciseId: exerciseId)
                    )
                }
            }
        }

        // 3. For each distance, find best and 2nd-best, match buoy data
        var entries: [PREntry] = []

        for distance in distances {
            guard var results = resultsByDistance[distance], !results.isEmpty else { continue }
            results.sort { $0.segment.predictedTimeSec < $1.segment.predictedTimeSec }

            let best = results[0]
            let previousBest: Double? = results.count > 1 ? results[1].segment.predictedTimeSec : nil

            // Match buoy data for the best segment
            let segTs = Int(best.segment.startTime.timeIntervalSince1970)
            let buoyObs = (try? await buoyStore.fetchObservationsNear(ts: segTs)) ?? []
            let readings = pivotObservations(buoyObs)

            var stwKmh: Double?
            var buoyCurrentKmh: Double?
            var windKmh: Double?

            if !readings.isEmpty {
                let nearest = binaryNearestReading(readings, to: best.segment.startTime)
                if let r = nearest, abs(r.ts.timeIntervalSince(best.segment.startTime)) <= 7200 {
                    let bearing = best.segment.bearing

                    if let cSpeed = r.currentSpeedMps, let cDir = r.currentDirectionDeg {
                        let angle = ApparentConditions.relativeCurrentAngle(
                            vesselBearing: bearing, currentDirTo: cDir
                        )
                        let comp = ApparentConditions.currentComponent(speedMps: cSpeed, angle: angle)
                        buoyCurrentKmh = comp * 3.6
                        stwKmh = best.segment.avgSpeedKmh - comp * 3.6
                    }

                    if let wSpeed = r.windSpeedMps, let wDir = r.windDirectionDeg {
                        let angle = ApparentConditions.relativeAngle(
                            vesselBearing: bearing, sourceDirFrom: wDir
                        )
                        let comp = ApparentConditions.windComponent(speedMps: wSpeed, angle: angle)
                        windKmh = comp * 3.6
                    }
                }
            }

            entries.append(PREntry(
                distanceM: distance,
                predictedTimeSec: best.segment.predictedTimeSec,
                predictedTimeFmt: LeagueBuilder.formatTime(best.segment.predictedTimeSec),
                avgSpeedKmh: best.segment.avgSpeedKmh,
                bearing: best.segment.bearing,
                sessionDate: best.segment.startTime,
                stwBuoyKmh: stwKmh,
                buoyCurrentComponentKmh: buoyCurrentKmh,
                windComponentKmh: windKmh,
                sessionCount: results.count,
                previousBestSec: previousBest
            ))
        }

        return entries
    }

    // MARK: - Buoy helpers (mirrors SimcostaClient.pivot, which is private)

    private static func pivotObservations(_ obs: [BuoyObservation]) -> [BuoyReading] {
        var byTs: [Int: [String: Double]] = [:]
        for o in obs {
            guard let v = o.value else { continue }
            byTs[o.ts, default: [:]][o.variable] = v
        }
        return byTs.keys.sorted().map { ts in
            let v = byTs[ts]!
            return BuoyReading(
                ts: Date(timeIntervalSince1970: TimeInterval(ts)),
                windSpeedMps: v["Avg_Wnd_Sp"],
                windDirectionDeg: v["Avg_Wnd_Dir_N"],
                currentSpeedMps: v["C_Avg_Spd"].map { $0 * 0.001 },
                currentDirectionDeg: v["C_Avg_Dir_N"]
            )
        }
    }

    private static func binaryNearestReading(_ sorted: [BuoyReading], to t: Date) -> BuoyReading? {
        guard !sorted.isEmpty else { return nil }
        var lo = 0, hi = sorted.count - 1
        while lo < hi {
            let mid = (lo + hi) / 2
            if sorted[mid].ts < t { lo = mid + 1 } else { hi = mid }
        }
        if lo == 0 { return sorted[0] }
        if lo == sorted.count { return sorted[sorted.count - 1] }
        let dBefore = abs(sorted[lo - 1].ts.timeIntervalSince(t))
        let dAfter  = abs(sorted[lo].ts.timeIntervalSince(t))
        return dBefore <= dAfter ? sorted[lo - 1] : sorted[lo]
    }
}
