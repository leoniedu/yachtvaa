import SwiftUI

struct PRBoardView: View {
    let athlete: Athlete
    let exerciseStore: ExerciseStore
    let buoyStore: BuoyStore

    @State private var entries: [PREntry] = []
    @State private var isLoading = true
    @State private var errorMessage: String?

    var body: some View {
        Group {
            if isLoading {
                VStack(spacing: 12) {
                    ProgressView()
                    Text("Calculando recordes…")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else if entries.isEmpty {
                PlaceholderView(
                    icon: "trophy",
                    title: "Sem dados",
                    subtitle: "Nenhum GPS encontrado para \(athlete.fullName)."
                )
            } else {
                ScrollView {
                    LazyVStack(spacing: 12) {
                        ForEach(entries) { entry in
                            PRCard(entry: entry)
                        }
                        // Show placeholder cards for distances with no data
                        ForEach(missingDistances, id: \.self) { d in
                            EmptyDistanceCard(distanceM: d)
                        }
                    }
                    .padding()
                }
                .background(Color(.systemGroupedBackground))
            }
        }
        .navigationTitle(athlete.fullName)
        .navigationBarTitleDisplayMode(.inline)
        .task {
            do {
                entries = try await PRBoardComputer.compute(
                    athleteId: athlete.id,
                    exerciseStore: exerciseStore,
                    buoyStore: buoyStore
                )
            } catch {
                errorMessage = error.localizedDescription
            }
            isLoading = false
        }
    }

    private var missingDistances: [Double] {
        let present = Set(entries.map(\.distanceM))
        return PRBoardComputer.distances.filter { !present.contains($0) }
    }
}

// MARK: - PR Card

private struct PRCard: View {
    let entry: PREntry

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            // Header: distance + trophy
            HStack {
                Text(formatDistance(entry.distanceM))
                    .font(.headline)
                Spacer()
                Image(systemName: "trophy.fill")
                    .foregroundStyle(.yellow)
            }

            // Time + speed
            HStack(alignment: .firstTextBaseline) {
                Text(entry.predictedTimeFmt)
                    .font(.title2.monospacedDigit().bold())
                Spacer()
                Text(String(format: "%.1f km/h", entry.avgSpeedKmh))
                    .font(.callout.monospacedDigit())
                    .foregroundStyle(.secondary)
            }

            // Date + bearing
            HStack(spacing: 6) {
                Text(formatDate(entry.sessionDate))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Text("·")
                    .foregroundStyle(.tertiary)
                Text(String(format: "%.0f°", entry.bearing))
                    .font(.caption.monospacedDigit())
                    .foregroundStyle(.secondary)
            }

            // STW + components
            if entry.stwBuoyKmh != nil || entry.windComponentKmh != nil {
                HStack(spacing: 8) {
                    if let stw = entry.stwBuoyKmh {
                        Text(String(format: "STW %.1f km/h", stw))
                            .font(.caption.monospacedDigit())
                            .foregroundStyle(.blue)
                    }
                    if let w = entry.windComponentKmh {
                        compTag("↗", w)
                    }
                    if let b = entry.buoyCurrentComponentKmh {
                        compTag("~B", b)
                    }
                }
            }

            // Improvement from previous best
            if let prev = entry.previousBestSec {
                let delta = prev - entry.predictedTimeSec
                HStack(spacing: 4) {
                    Image(systemName: "arrow.down.right")
                        .font(.caption2)
                        .foregroundStyle(.green)
                    Text(String(format: "%.1fs mais rápido que o anterior", delta))
                        .font(.caption2)
                        .foregroundStyle(.green)
                }
            }

            // Session count
            Text("\(entry.sessionCount) sessões")
                .font(.caption2)
                .foregroundStyle(.tertiary)
        }
        .padding()
        .background(Color(.secondarySystemGroupedBackground), in: RoundedRectangle(cornerRadius: 12))
    }

    @ViewBuilder
    private func compTag(_ prefix: String, _ kmh: Double) -> some View {
        let sign = kmh >= 0 ? "+" : ""
        Text("\(prefix)\(sign)\(String(format: "%.1f", kmh))")
            .font(.caption.monospacedDigit())
            .foregroundStyle(kmh >= 0 ? Color.green : Color.red)
    }

    private func formatDistance(_ m: Double) -> String {
        if m >= 1000 {
            return String(format: "%.0f km", m / 1000)
        }
        return String(format: "%.0f m", m)
    }

    private func formatDate(_ date: Date) -> String {
        let fmt = DateFormatter()
        fmt.locale = Locale(identifier: "pt_BR")
        fmt.timeZone = AppConfig.timezone
        fmt.dateStyle = .medium
        return fmt.string(from: date)
    }
}

// MARK: - Empty distance placeholder

private struct EmptyDistanceCard: View {
    let distanceM: Double

    var body: some View {
        HStack {
            Text(distanceM >= 1000
                 ? String(format: "%.0f km", distanceM / 1000)
                 : String(format: "%.0f m", distanceM))
                .font(.headline)
                .foregroundStyle(.secondary)
            Spacer()
            Text("Sem dados")
                .font(.caption)
                .foregroundStyle(.tertiary)
        }
        .padding()
        .background(Color(.secondarySystemGroupedBackground).opacity(0.5),
                     in: RoundedRectangle(cornerRadius: 12))
    }
}
