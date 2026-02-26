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
        return session.athletes.filter { spatialIds.contains($0.id) }
    }

    private var athleteList: some View {
        List {
            timeFilterSection
            Section("Atletas") {
                ForEach(displayedAthletes) { athlete in
                    HStack {
                        Image(systemName: "person.circle")
                            .foregroundStyle(.tint)
                        Text(athlete.fullName)
                        Spacer()
                        if session.selectedAthleteIDs.contains(athlete.id) {
                            Image(systemName: "checkmark")
                                .foregroundStyle(.tint)
                        }
                    }
                    .contentShape(Rectangle())
                    .onTapGesture { toggleSelection(athlete.id) }
                }
            }
        }
        .listStyle(.insetGrouped)
    }

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
