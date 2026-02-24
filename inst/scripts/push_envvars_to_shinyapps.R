#!/usr/bin/env Rscript
# Push local environment variables to a hosted R server account.
# Usage:
#   Rscript inst/scripts/push_envvars_to_shinyapps.R                         # defaults to Connect Cloud
#   Rscript inst/scripts/push_envvars_to_shinyapps.R shinyapps.io eduardo-leoni

readRenviron("~/.Renviron")

args <- commandArgs(trailingOnly = TRUE)
server <- if (length(args) >= 1) args[[1]] else "connect.posit.cloud"
account <- if (length(args) >= 2) args[[2]] else "leoni"

var_names <- c(
  "AWS_ACCESS_KEY_ID",
  "AWS_SECRET_ACCESS_KEY",
  "AWS_DEFAULT_REGION",
  "AWS_S3_BUCKET",
  "TREINUS_EMAIL",
  "TREINUS_PASSWORD",
  "TREINUS_TEAM_ID",
  "GITHUB_PAT"
)

vals <- Sys.getenv(var_names)
missing <- var_names[nchar(vals) == 0]

if (length(missing) > 0) {
  warning(
    "The following env vars are not set locally and will be skipped:\n  ",
    paste(missing, collapse = "\n  ")
  )
  vals <- vals[nchar(vals) > 0]
}

if (length(vals) == 0) {
  stop("No env vars to push.")
}

cat("Pushing to", server, "account:", account, "\n")
for (nm in names(vals)) {
  cat(" ", nm, "=", if (grepl("SECRET|KEY|PAT|PASSWORD|TOKEN", nm)) "***" else vals[[nm]], "\n")
}

if (rsconnect:::isPositConnectCloudServer(server)) {
  # Connect Cloud: secrets are set via PATCH /contents/{id}
  # updateContent reads values from Sys.getenv(), so they must be set in the environment
  guid <- if (length(args) >= 3) args[[3]] else "019c8fe3-c953-680f-161d-e89891729db1"
  cat("Content GUID:", guid, "\n")
  account_details <- rsconnect:::accountInfo(account, server)
  client <- rsconnect:::clientForAccount(account_details)
  result <- client$updateContent(
    contentId   = guid,
    envVars     = names(vals),
    newBundle   = FALSE,
    primaryFile = "app.R",
    appMode     = "shiny"
  )
  cat("\nDone. State:", result$state, "\n")
} else {
  rsconnect:::updateAccountEnvVars(
    envVars = vals,
    account = account,
    server  = server
  )
  cat("\nVerifying...\n")
  current <- rsconnect:::listAccountEnvVars(account = account, server = server)
  print(current)
}
