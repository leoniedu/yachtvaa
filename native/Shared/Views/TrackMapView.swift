import SwiftUI
import MapKit

struct TrackMapView: View {
    @EnvironmentObject private var session: AppSession

    private let trackColors: [Color] = [.blue, .red, .green, .orange, .purple, .cyan, .pink]

    /// Seconds since gpsStart; defaults to totalSeconds (show all).
    @State private var sliderPos: Double = 0
    @State private var isAnimating = false
    @State private var animationTask: Task<Void, Never>?
    @State private var showCurrents = true

    // MARK: - Computed from session

    private var gpsStart: Date? { session.gpsStart }
    private var gpsEnd:   Date? { session.gpsEnd }
    private var hasGPSRange: Bool { gpsStart != nil && gpsEnd != nil }

    private var totalSeconds: Double {
        guard let s = gpsStart, let e = gpsEnd else { return 1 }
        return max(1, e.timeIntervalSince(s))
    }

    private var currentMapTime: Date {
        guard let s = gpsStart else { return Date() }
        return s.addingTimeInterval(sliderPos)
    }

    private var hoursUTC: [Int] { session.currentGrid?.hoursUTC ?? [] }

    // MARK: - Body

    var body: some View {
        Map(initialPosition: .region(
            MKCoordinateRegion(
                center: AppConfig.mapCenter,
                span: MKCoordinateSpan(latitudeDelta: 0.135, longitudeDelta: 0.135)
            )
        )) {
            // GPS tracks trimmed to currentMapTime
            let sortedIds = session.mapGpsRecordsByAthlete.keys.sorted()
            let athleteNames = Dictionary(uniqueKeysWithValues: session.athletes.map { ($0.id, $0.fullName) })
            ForEach(Array(sortedIds.enumerated()), id: \.element) { idx, athleteId in
                let coords = filteredCoords(for: athleteId)
                if !coords.isEmpty {
                    MapPolyline(coordinates: coords)
                        .stroke(trackColors[idx % trackColors.count], lineWidth: 2)
                    Annotation("", coordinate: coords.last!, anchor: .bottom) {
                        Text(athleteNames[athleteId] ?? "")
                            .font(.system(size: 9, weight: .semibold))
                            .foregroundStyle(.white)
                            .shadow(color: .black.opacity(0.8), radius: 2, x: 0, y: 0)
                    }
                }
            }

            // SISCORAR current arrows at the nearest UTC hour to currentMapTime
            if showCurrents {
                ForEach(currentArrows(hIdx: nearestSiscorarHourIdx), id: \.id) { arrow in
                    Annotation("", coordinate: arrow.coordinate, anchor: .center) {
                        Image(systemName: "arrow.up")
                            .rotationEffect(.degrees(arrow.dirDeg))
                            .font(.system(size: 11, weight: .semibold))
                            .foregroundStyle(colorForSpeed(arrow.speedMps))
                    }
                }
            }
        }
        .overlay(alignment: .bottomTrailing) {
            Button {
                showCurrents.toggle()
            } label: {
                Image(systemName: showCurrents ? "water.waves" : "water.waves.slash")
                    .font(.system(size: 15, weight: .semibold))
                    .foregroundStyle(showCurrents ? Color.accentColor : Color.secondary)
                    .frame(width: 36, height: 36)
                    .background(.regularMaterial, in: Circle())
            }
            .buttonStyle(.plain)
            .padding(.trailing, 16)
            .padding(.bottom, hasGPSRange ? 76 : 16)
        }
        .overlay(alignment: .bottom) {
            VStack(spacing: 6) {
                if hasGPSRange {
                    timeSliderBar
                }
                if session.gpsRecordsByAthlete.isEmpty {
                    Label("Toque Carregar para ver as rotas", systemImage: "map")
                        .font(.caption)
                        .padding(.horizontal, 12)
                        .padding(.vertical, 6)
                        .background(.regularMaterial, in: Capsule())
                }
            }
            .padding(.bottom, 16)
        }
        .onAppear { onGPSRangeUpdated() }
        .onChange(of: session.gpsEnd) { _ in onGPSRangeUpdated() }
    }

    // MARK: - GPS time filter

    private func filteredCoords(for athleteId: Int) -> [CLLocationCoordinate2D] {
        guard let records = session.mapGpsRecordsByAthlete[athleteId] else { return [] }
        guard hasGPSRange else {
            return records.map { CLLocationCoordinate2D(latitude: $0.latDeg, longitude: $0.lonDeg) }
        }
        let cutoff = currentMapTime
        return records
            .filter { $0.ts <= cutoff }
            .map { CLLocationCoordinate2D(latitude: $0.latDeg, longitude: $0.lonDeg) }
    }

    // MARK: - SISCORAR hour selection

    private var nearestSiscorarHourIdx: Int {
        guard !hoursUTC.isEmpty else { return 0 }
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "UTC")!
        let utcHour = cal.component(.hour, from: currentMapTime)
        return hoursUTC.indices.min {
            abs(hoursUTC[$0] - utcHour) < abs(hoursUTC[$1] - utcHour)
        } ?? 0
    }

    // MARK: - Time slider bar

    private var timeSliderBar: some View {
        HStack(spacing: 8) {
            Button {
                toggleAnimation()
            } label: {
                Image(systemName: isAnimating ? "stop.fill" : "play.fill")
                    .frame(width: 24)
            }
            .buttonStyle(.plain)

            Slider(value: $sliderPos, in: 0...totalSeconds)

            Text(formatLocalTime(currentMapTime))
                .font(.caption.monospacedDigit())
                .frame(width: 44)
        }
        .padding(.horizontal, 14)
        .padding(.vertical, 8)
        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 12))
        .padding(.horizontal, 16)
    }

    private func formatLocalTime(_ date: Date) -> String {
        let fmt = DateFormatter()
        fmt.timeZone = AppConfig.timezone   // Bahia local time
        fmt.dateFormat = "HH:mm"
        return fmt.string(from: date)
    }

    // MARK: - Animation (1-minute steps, loops by default)

    private func toggleAnimation() {
        if isAnimating {
            animationTask?.cancel()
            animationTask = nil
            isAnimating = false
        } else {
            isAnimating = true
            sliderPos = 0   // rewind before playing
            animationTask = Task {
                let step = 60.0   // 1 minute per frame
                while !Task.isCancelled {
                    try? await Task.sleep(nanoseconds: 200_000_000)
                    guard !Task.isCancelled else { break }
                    await MainActor.run {
                        let next = sliderPos + step
                        if next > totalSeconds {
                            sliderPos = 0
                        } else {
                            sliderPos = next
                        }
                    }
                }
            }
        }
    }

    /// Called on appear and whenever gpsEnd changes.
    /// Shows all tracks immediately; user taps ▶ to animate.
    private func onGPSRangeUpdated() {
        animationTask?.cancel()
        animationTask = nil
        isAnimating = false
        guard hasGPSRange else { sliderPos = 0; return }
        sliderPos = totalSeconds   // show all records immediately
    }

    // MARK: - SISCORAR arrows (no bbox filter, 0.02° grid)

    private struct CurrentArrow {
        let id: String
        let coordinate: CLLocationCoordinate2D
        let dirDeg: Double
        let speedMps: Double
    }

    private func currentArrows(hIdx: Int) -> [CurrentArrow] {
        guard let grid = session.currentGrid, !grid.points.isEmpty,
              hIdx < grid.hoursUTC.count else { return [] }

        let subsampleStep = 0.02
        var arrows: [CurrentArrow] = []

        for pt in grid.points {
            guard hIdx < pt.d.count, hIdx < pt.s.count,
                  let dir = pt.d[hIdx], let spd = pt.s[hIdx]
            else { continue }

            let latSnap = (pt.lat / subsampleStep).rounded() * subsampleStep
            let lonSnap = (pt.lon / subsampleStep).rounded() * subsampleStep
            guard abs(pt.lat - latSnap) < 0.003, abs(pt.lon - lonSnap) < 0.003 else { continue }

            arrows.append(CurrentArrow(
                id: "\(pt.lat),\(pt.lon)",
                coordinate: CLLocationCoordinate2D(latitude: pt.lat, longitude: pt.lon),
                dirDeg: dir,
                speedMps: spd
            ))
        }
        return arrows
    }

    // MARK: - Speed color scale

    private func colorForSpeed(_ mps: Double) -> Color {
        switch mps {
        case ..<0.10: return .cyan.opacity(0.55)
        case 0.10..<0.25: return .cyan
        case 0.25..<0.40: return .green
        case 0.40..<0.55: return .yellow
        case 0.55..<0.75: return .orange
        default:          return .red
        }
    }
}
