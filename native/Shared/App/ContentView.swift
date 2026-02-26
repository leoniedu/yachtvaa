import SwiftUI

struct ContentView: View {
    @StateObject private var session = AppSession()

    var body: some View {
        if session.isLoggedIn {
            mainTabView
                .task {
                    await session.autoLogin()
                    session.loadFromCache()
                }
        } else {
            LoginView()
                .environmentObject(session)
        }
    }

    // MARK: - Main layout

    private var mainTabView: some View {
        NavigationStack {
            ZStack {
                TabView {
                    AthletesView()
                        .tabItem { Label("Atletas", systemImage: "person.3") }

                    TrackMapView()
                        .tabItem { Label("Mapa", systemImage: "map") }

                    ConditionsView()
                        .tabItem { Label("Condições", systemImage: "wind") }

                    RankingsView()
                        .tabItem { Label("Classif.", systemImage: "list.number") }
                }

                if session.isLoading {
                    loadingOverlay
                }
            }
            .navigationBarTitleDisplayMode(.inline)
            .onChange(of: session.selectedDate) { _ in session.loadFromCache() }
            .toolbar {
                ToolbarItem(placement: .principal) {
                    HStack(spacing: 4) {
                        Button {
                            session.selectedDate = Calendar.current.date(
                                byAdding: .day, value: -1, to: session.selectedDate
                            )!
                        } label: {
                            Image(systemName: "chevron.left")
                        }
                        .disabled(session.isLoading)

                        DatePicker(
                            "",
                            selection: $session.selectedDate,
                            in: ...Date(),
                            displayedComponents: .date
                        )
                        .labelsHidden()

                        Button {
                            session.selectedDate = Calendar.current.date(
                                byAdding: .day, value: 1, to: session.selectedDate
                            )!
                        } label: {
                            Image(systemName: "chevron.right")
                        }
                        .disabled(session.isLoading || Calendar.current.isDateInToday(session.selectedDate))
                    }
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button {
                        session.syncWithTreinus()
                    } label: {
                        Image(systemName: "arrow.clockwise.icloud")
                    }
                    .disabled(session.isLoading)
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    NavigationLink {
                        SettingsView()
                            .environmentObject(session)
                    } label: {
                        Image(systemName: "gear")
                    }
                }
            }
        }
        .environmentObject(session)
    }

    private var loadingOverlay: some View {
        VStack(spacing: 12) {
            ProgressView()
            Text(session.loadingMessage)
                .font(.caption)
                .foregroundStyle(.secondary)
        }
        .padding(20)
        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 14))
    }
}
