import SwiftUI

// MARK: - Column geometry
// Line 1: [rank] [name · · · · ] [time]
// Line 2: [    ] [STW·S · · · ] [STW·B]  (same widths → auto-alignment)
private enum Col {
    static let rank: CGFloat = 26
    static let right: CGFloat = 70   // time & STW·B
    static let gap: CGFloat  =  8
    static let pad: CGFloat  = 16
    static var indent: CGFloat { rank + gap }  // left edge of name / STW·S
}

// MARK: - Sort state

private enum SortKey { case name, time, stwSiscorar, stwBuoy }

// MARK: - View

struct RankingsView: View {
    @EnvironmentObject private var session: AppSession
    @State private var sortKey   = SortKey.time
    @State private var ascending = true

    private var hasBuoy: Bool {
        session.leagueEntries.contains { $0.speedThroughWaterBuoyKmh != nil }
    }

    private var sorted: [LeagueEntry] {
        session.leagueEntries.sorted { sortBefore($0, $1) }
    }

    private func sortBefore(_ a: LeagueEntry, _ b: LeagueEntry) -> Bool {
        switch sortKey {
        case .name:
            let lt = a.athlete.fullName < b.athlete.fullName
            return ascending ? lt : !lt
        case .time:
            let lt = a.predictedTimeSec < b.predictedTimeSec
            return ascending ? lt : !lt
        case .stwSiscorar:
            return stwLess(a.speedThroughWaterSiscorarKmh, b.speedThroughWaterSiscorarKmh)
        case .stwBuoy:
            return stwLess(a.speedThroughWaterBuoyKmh, b.speedThroughWaterBuoyKmh)
        }
    }

    // nil always last; non-nil ordered by ascending flag
    private func stwLess(_ a: Double?, _ b: Double?) -> Bool {
        switch (a, b) {
        case (nil, nil): return false
        case (nil,   _): return false
        case (  _, nil): return true
        case (let x?, let y?): return ascending ? x < y : x > y
        }
    }

    private func tap(_ key: SortKey) {
        if sortKey == key {
            ascending.toggle()
        } else {
            sortKey  = key
            // fastest-first default for speed columns; A-Z for name; fastest-first for time
            ascending = key == .name
        }
    }

    var body: some View {
        if session.leagueEntries.isEmpty {
            PlaceholderView(
                icon: "list.number",
                title: "Sem classificação",
                subtitle: "Toque Carregar para calcular a classificação da sessão."
            )
        } else {
            ScrollView {
                LazyVStack(spacing: 0, pinnedViews: .sectionHeaders) {
                    Section {
                        ForEach(sorted) { entry in
                            LeagueRow(entry: entry, hasBuoy: hasBuoy)
                            Divider().padding(.leading, Col.pad + Col.indent)
                        }
                    } header: {
                        TableHeader(
                            sortKey: sortKey, ascending: ascending,
                            hasBuoy: hasBuoy, onTap: tap
                        )
                        .background(.bar)
                    }
                }
            }
            .background(Color(.systemGroupedBackground))
        }
    }
}

// MARK: - Sticky header

private struct TableHeader: View {
    let sortKey: SortKey
    let ascending: Bool
    let hasBuoy: Bool
    let onTap: (SortKey) -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 1) {
            // Line 1: Atleta  |  Tempo
            rowOf {
                rankGap
                colBtn("Atleta", .name, trailing: false)
                colBtn("Tempo",  .time, trailing: true).frame(width: Col.right)
            }
            // Line 2: STW·S  |  STW·B
            rowOf {
                Spacer().frame(width: Col.indent)
                colBtn("STW·S (SISCORAR)", .stwSiscorar, trailing: false)
                if hasBuoy {
                    colBtn("STW·B (Boia)", .stwBuoy, trailing: true).frame(width: Col.right)
                }
            }
        }
        .font(.caption.bold())
        .buttonStyle(.plain)
        .padding(.horizontal, Col.pad)
        .padding(.vertical, 5)
        .overlay(alignment: .bottom) { Divider() }
    }

    @ViewBuilder
    private func rowOf<Content: View>(@ViewBuilder _ content: () -> Content) -> some View {
        HStack(spacing: Col.gap, content: content)
    }

    private var rankGap: some View {
        Spacer().frame(width: Col.rank)
    }

    @ViewBuilder
    private func colBtn(_ label: String, _ key: SortKey, trailing: Bool) -> some View {
        Button { onTap(key) } label: {
            HStack(spacing: 2) {
                if trailing { Spacer(minLength: 0) }
                Text(label)
                Image(systemName: sortKey == key
                      ? (ascending ? "chevron.up" : "chevron.down")
                      : "chevron.up.chevron.down")
                    .imageScale(.small)
                    .opacity(sortKey == key ? 1 : 0.3)
                if !trailing { Spacer(minLength: 0) }
            }
            .foregroundStyle(sortKey == key ? Color.accentColor : Color.secondary)
        }
    }
}

// MARK: - Row

private struct LeagueRow: View {
    let entry: LeagueEntry
    let hasBuoy: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 3) {
            // Line 1: rank  name  time
            HStack(spacing: Col.gap) {
                Text("\(entry.rank)")
                    .font(.subheadline.monospacedDigit())
                    .frame(width: Col.rank, alignment: .trailing)
                    .foregroundStyle(entry.rank <= 3 ? Color.primary : Color.secondary)

                Text(entry.athlete.fullName)
                    .font(.subheadline)
                    .lineLimit(1)
                    .frame(maxWidth: .infinity, alignment: .leading)

                Text(entry.predictedTimeFmt)
                    .font(.subheadline.monospacedDigit())
                    .frame(width: Col.right, alignment: .trailing)
            }

            // Line 2: STW·S  STW·B  (aligned under name & time respectively)
            HStack(spacing: Col.gap) {
                Spacer().frame(width: Col.indent)

                stwCell(entry.speedThroughWaterSiscorarKmh)
                    .frame(maxWidth: .infinity, alignment: .leading)

                if hasBuoy {
                    stwCell(entry.speedThroughWaterBuoyKmh)
                        .frame(width: Col.right, alignment: .trailing)
                }
            }

            // Line 3: GPS speed · bearing · components
            HStack(spacing: 6) {
                Spacer().frame(width: Col.indent)
                Text(String(format: "%.1f km/h · %.0f°", entry.avgSpeedKmh, entry.bearing))
                if let w = entry.windComponent     { compTag("↗", w) }
                if let s = entry.siscorarCurrentComponent { compTag("~S", s) }
                if let b = entry.buoyCurrentComponent     { compTag("~B", b) }
            }
            .font(.caption2)
            .foregroundStyle(.tertiary)
        }
        .padding(.horizontal, Col.pad)
        .padding(.vertical, 6)
        .background(Color(.secondarySystemGroupedBackground))
    }

    @ViewBuilder
    private func stwCell(_ value: Double?) -> some View {
        if let v = value {
            Text(String(format: "%.1f km/h", v))
                .font(.callout.monospacedDigit())
                .foregroundStyle(.primary)
        } else {
            Text("—")
                .font(.callout)
                .foregroundStyle(.tertiary)
        }
    }

    @ViewBuilder
    private func compTag(_ prefix: String, _ kmh: Double) -> some View {
        let sign = kmh >= 0 ? "+" : ""
        Text("\(prefix)\(sign)\(String(format: "%.1f", kmh))")
            .foregroundStyle(kmh >= 0 ? Color.green : Color.red)
    }
}
