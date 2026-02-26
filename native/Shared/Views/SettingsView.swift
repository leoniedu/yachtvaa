import SwiftUI

struct SettingsView: View {
    @EnvironmentObject private var session: AppSession

    @State private var email = ""
    @State private var password = ""
    @State private var teamIdText = ""
    @State private var showResetConfirm = false
    @AppStorage("maxAthleteId") private var maxAthleteId = 10

    var body: some View {
        Form {
            Section("Credenciais Treinus") {
                TextField("E-mail", text: $email)
                    .keyboardType(.emailAddress)
                    .textContentType(.emailAddress)
                    .autocorrectionDisabled()
                    .textInputAutocapitalization(.never)
                SecureField("Senha", text: $password)
                    .textContentType(.password)
                TextField("ID da Equipe (opcional)", text: $teamIdText)
                    .keyboardType(.numberPad)
                Button("Salvar e Reconectar") {
                    Task {
                        await session.login(
                            email: email,
                            password: password,
                            teamId: Int(teamIdText)
                        )
                    }
                }
                .disabled(email.isEmpty || password.isEmpty || session.isLoading)
            }

            Section("Análise") {
                HStack {
                    Text("Distância de referência")
                    Spacer()
                    TextField("metros", value: $session.analysisDistanceM, format: .number)
                        .keyboardType(.decimalPad)
                        .multilineTextAlignment(.trailing)
                        .frame(width: 80)
                        .onChange(of: session.analysisDistanceM) { _ in session.rerunAnalysis() }
                    Text("m")
                        .foregroundStyle(.secondary)
                }
            }

            Section("Atletas") {
                HStack {
                    Text("Atletas carregados")
                    Spacer()
                    Text("\(session.athletes.count)")
                        .foregroundStyle(.secondary)
                }
                if session.backfillPending > 0 {
                    HStack(spacing: 8) {
                        ProgressView()
                            .controlSize(.small)
                        Text("Baixando histórico…")
                            .foregroundStyle(.secondary)
                        Spacer()
                        Text("~\(session.backfillPending) restantes")
                            .font(.caption.monospacedDigit())
                            .foregroundStyle(.secondary)
                    }
                }
                Stepper("Sondar IDs 1…\(maxAthleteId)", value: $maxAthleteId, in: 10...500, step: 10)
                Button("Atualizar Atletas") {
                    Task { await session.refreshAthletes() }
                }
                .disabled(session.isLoading)
            }

            if let error = session.errorMessage {
                Section {
                    Text(error).foregroundStyle(.red)
                }
            }

            Section("Limpar Dados") {
                Button("Limpar cache de análise e SISCORAR") {
                    Task { await session.clearAnalysisCache() }
                }
                .foregroundStyle(.orange)
                Button("Limpar dados de boia") {
                    Task { await session.clearBuoyData() }
                }
                .foregroundStyle(.orange)
                Button("Limpar tudo / Começar do zero", role: .destructive) {
                    showResetConfirm = true
                }
            }

            Section {
                Button("Sair", role: .destructive) {
                    session.logout()
                }
            }
        }
        .navigationTitle("Configurações")
        .onAppear { prefillFromSaved() }
        .confirmationDialog(
            "Apagar todos os dados locais?",
            isPresented: $showResetConfirm,
            titleVisibility: .visible
        ) {
            Button("Limpar tudo", role: .destructive) {
                Task { await session.clearAll() }
            }
            Button("Cancelar", role: .cancel) {}
        }
    }

    private func prefillFromSaved() {
        guard let creds = CredentialManager.load() else { return }
        email    = creds.email
        password = creds.password
        if let tid = creds.teamId { teamIdText = "\(tid)" }
    }
}
