import SwiftUI

struct HourlyMapView: View {
    @EnvironmentObject private var session: AppSession

    private let columns = Array(repeating: GridItem(.flexible(), spacing: 8), count: 3)

    var body: some View {
        if session.athletes.isEmpty {
            PlaceholderView(
                icon: "clock",
                title: "Sem mapas horários",
                subtitle: "Toque Carregar para ver o grid de correntes hora a hora."
            )
        } else {
            ScrollView {
                LazyVGrid(columns: columns, spacing: 8) {
                    // TODO Step 10: MKMapSnapshotter images with current arrows per hour
                    ForEach(0..<24, id: \.self) { hour in
                        RoundedRectangle(cornerRadius: 8)
                            .fill(Color.secondary.opacity(0.12))
                            .aspectRatio(1, contentMode: .fit)
                            .overlay(
                                Text(String(format: "%02d:00", hour))
                                    .font(.caption.monospacedDigit())
                                    .foregroundStyle(.secondary)
                            )
                    }
                }
                .padding()
            }
        }
    }
}
