import SwiftUI

struct LoginView: View {
    @EnvironmentObject private var session: AppSession

    @State private var email = ""
    @State private var password = ""
    @State private var teamIdText = ""

    var body: some View {
        NavigationStack {
            Form {
                Section("Conta Treinus") {
                    TextField("E-mail", text: $email)
                        .keyboardType(.emailAddress)
                        .textContentType(.emailAddress)
                        .autocorrectionDisabled()
                        .textInputAutocapitalization(.never)
                    SecureField("Senha", text: $password)
                        .textContentType(.password)
                }

                Section {
                    TextField("ID da Equipe (opcional)", text: $teamIdText)
                        .keyboardType(.numberPad)
                } footer: {
                    Text("Necessário apenas se sua conta pertence a mais de uma equipe.")
                }

                if let error = session.errorMessage {
                    Section {
                        Text(error)
                            .foregroundStyle(.red)
                    }
                }

                Section {
                    Button(action: loginAction) {
                        if session.isLoading {
                            HStack {
                                ProgressView()
                                Text(session.loadingMessage)
                            }
                        } else {
                            Text("Entrar")
                                .frame(maxWidth: .infinity, alignment: .center)
                        }
                    }
                    .disabled(session.isLoading || email.isEmpty || password.isEmpty)
                }
            }
            .navigationTitle("YachtVAA")
        }
        .onAppear { prefillFromSaved() }
    }

    private func loginAction() {
        Task { await session.login(email: email, password: password, teamId: Int(teamIdText)) }
    }

    private func prefillFromSaved() {
        guard let creds = CredentialManager.load() else { return }
        email    = creds.email
        password = creds.password
        if let tid = creds.teamId { teamIdText = "\(tid)" }
    }
}
