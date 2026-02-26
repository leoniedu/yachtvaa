import SwiftUI

struct AthletesView: View {
    @EnvironmentObject private var session: AppSession

    var body: some View {
        Group {
            if session.athletes.isEmpty {
                emptyState
            } else {
                athleteList
            }
        }
    }

    private var emptyState: some View {
        VStack(spacing: 16) {
            Image(systemName: "person.3")
                .font(.system(size: 52))
                .foregroundStyle(.secondary)
            Text("Nenhum atleta carregado")
                .font(.headline)
            Text("Vá em Configurações → Atualizar Atletas para buscar os membros da equipe.")
                .font(.subheadline)
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal, 32)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private var displayedAthletes: [Athlete] {
        let spatialIds = session.allGpsAthleteIds
        return session.athletes
            .filter { spatialIds.contains($0.id) }
            .sorted { $0.fullName < $1.fullName }
    }

    // MARK: - Summary stats

    private var athleteCount: Int { session.allGpsAthleteIds.count }

    private var avgDurationMin: Double? {
        let recs = session.allGpsRecords
        guard !recs.isEmpty else { return nil }
        let durations = recs.values.compactMap { records -> Double? in
            guard records.count >= 2 else { return nil }
            return records.last!.ts.timeIntervalSince(records.first!.ts) / 60
        }
        guard !durations.isEmpty else { return nil }
        return durations.reduce(0, +) / Double(durations.count)
    }

    private var fastestEntry: LeagueEntry? { session.leagueEntries.first }

    private var topSpeedEntry: LeagueEntry? {
        session.leagueEntries.max { $0.avgSpeedKmh < $1.avgSpeedKmh }
    }

    // MARK: - Per-athlete stats

    private struct AthleteStats {
        let startTime: Date
        let durationMin: Double
        let distanceKm: Double
        let speedKmh: Double
    }

    private func stats(for id: Int) -> AthleteStats? {
        guard let records = session.allGpsRecords[id], records.count >= 2 else { return nil }
        let first = records.first!, last = records.last!
        let durMin = last.ts.timeIntervalSince(first.ts) / 60
        let distKm: Double
        if let d0 = first.distanceM, let d1 = last.distanceM, d1 > d0 {
            distKm = (d1 - d0) / 1000.0
        } else {
            distKm = 0
        }
        let speed = durMin > 0 ? distKm / (durMin / 60) : 0
        return AthleteStats(startTime: first.ts, durationMin: durMin, distanceKm: distKm, speedKmh: speed)
    }

    // MARK: - Body

    private var athleteList: some View {
        List {
            summarySection
            timeFilterSection
            Section("Atletas") {
                ForEach(displayedAthletes) { athlete in
                    athleteRow(athlete)
                }
            }
        }
        .listStyle(.insetGrouped)
    }

    // MARK: - Summary section

    private var summarySection: some View {
        Section {
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: 10) {
                    statChip("Na água", "\(athleteCount)", "person.3", .blue)
                    if let d = avgDurationMin {
                        statChip("Dur. média", formatDuration(d), "clock", .cyan)
                    }
                    if let e = fastestEntry {
                        statChip("Mais rápido",
                                 "\(e.athlete.fullName.components(separatedBy: " ").first ?? "") \(e.predictedTimeFmt)",
                                 "trophy", .green)
                    }
                    if let e = topSpeedEntry {
                        statChip("Maior vel.", String(format: "%.1f km/h", e.avgSpeedKmh), "speedometer", .orange)
                    }
                }
                .padding(.vertical, 4)
            }
        }
        .listRowInsets(EdgeInsets(top: 0, leading: 16, bottom: 0, trailing: 16))
    }

    private func statChip(_ label: String, _ value: String, _ icon: String, _ color: Color) -> some View {
        VStack(alignment: .leading, spacing: 2) {
            Label(label, systemImage: icon)
                .font(.caption2)
                .foregroundStyle(color)
            Text(value)
                .font(.caption.bold())
                .lineLimit(1)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 6)
        .background(color.opacity(0.1), in: RoundedRectangle(cornerRadius: 8))
    }

    // MARK: - Athlete row

    private func athleteRow(_ athlete: Athlete) -> some View {
        HStack(alignment: .center) {
            Image(systemName: "person.circle")
                .foregroundStyle(.tint)
            VStack(alignment: .leading, spacing: 2) {
                Text(athlete.fullName)
                    .font(.body)
                if let s = stats(for: athlete.id) {
                    Text(athleteStatsLine(s))
                        .font(.caption.monospacedDigit())
                        .foregroundStyle(.secondary)
                }
            }
            Spacer()
            if session.selectedAthleteIDs.contains(athlete.id) {
                Image(systemName: "checkmark")
                    .foregroundStyle(.tint)
            }
        }
        .contentShape(Rectangle())
        .onTapGesture { toggleSelection(athlete.id) }
    }

    private func athleteStatsLine(_ s: AthleteStats) -> String {
        let fmt = DateFormatter()
        fmt.timeZone = AppConfig.timezone
        fmt.dateFormat = "HH:mm"
        let start = fmt.string(from: s.startTime)
        let dur = formatDuration(s.durationMin)
        if s.distanceKm > 0.01 {
            return "\(start) · \(dur) · \(String(format: "%.1f km · %.1f km/h", s.distanceKm, s.speedKmh))"
        }
        return "\(start) · \(dur)"
    }

    private func formatDuration(_ min: Double) -> String {
        let h = Int(min) / 60
        let m = Int(min) % 60
        return h > 0 ? String(format: "%d:%02d", h, m) : String(format: "%d min", m)
    }

    // MARK: - Time filter

    private var timeFilterSection: some View {
        Section("Filtro de horário") {
            HStack {
                Text("Início")
                Spacer()
                if let h = session.filterStartHour {
                    Text(String(format: "%02d:00", h))
                        .foregroundStyle(.tint)
                    Button("Limpar") { session.filterStartHour = nil }
                        .font(.caption)
                } else {
                    Text("—").foregroundStyle(.secondary)
                }
                Stepper("", value: Binding(
                    get: { session.filterStartHour ?? 0 },
                    set: { session.filterStartHour = $0 }
                ), in: 0...23)
                .labelsHidden()
                .frame(width: 94)
            }
            HStack {
                Text("Fim")
                Spacer()
                if let h = session.filterEndHour {
                    Text(String(format: "%02d:00", h))
                        .foregroundStyle(.tint)
                    Button("Limpar") { session.filterEndHour = nil }
                        .font(.caption)
                } else {
                    Text("—").foregroundStyle(.secondary)
                }
                Stepper("", value: Binding(
                    get: { session.filterEndHour ?? 23 },
                    set: { session.filterEndHour = $0 }
                ), in: 0...23)
                .labelsHidden()
                .frame(width: 94)
            }
        }
    }

    private func toggleSelection(_ id: Int) {
        if session.selectedAthleteIDs.contains(id) {
            session.selectedAthleteIDs.remove(id)
        } else {
            session.selectedAthleteIDs.insert(id)
        }
    }
}
