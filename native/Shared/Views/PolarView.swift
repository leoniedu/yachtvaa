import SwiftUI

struct PolarView: View {
    @EnvironmentObject private var session: AppSession

    var body: some View {
        if session.athletes.isEmpty {
            PlaceholderView(
                icon: "chart.pie",
                title: "Sem dados polares",
                subtitle: "Toque Carregar para calcular os polares de velocidade."
            )
        } else {
            ScrollView {
                VStack(spacing: 24) {
                    // TODO Step 10: 3 Canvas-based polar charts
                    // (SISCORAR current, buoy current, wind)
                    ForEach(["Corrente SISCORAR", "Corrente Boia", "Vento"], id: \.self) { label in
                        VStack {
                            Text(label).font(.headline)
                            Circle()
                                .stroke(Color.secondary.opacity(0.3), lineWidth: 1)
                                .frame(width: 240, height: 240)
                                .overlay(
                                    Text("Polar")
                                        .foregroundStyle(.secondary)
                                )
                        }
                        .padding()
                        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 12))
                    }
                }
                .padding()
            }
        }
    }
}
