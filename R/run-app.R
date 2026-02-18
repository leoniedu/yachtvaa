# ------------------------------------------------------------------
# Shiny app launcher
# ------------------------------------------------------------------

#' Launch the Session Explorer Shiny app
#'
#' Starts an interactive Shiny application for exploring paddler training
#' sessions with environmental conditions (SISCORAR ocean currents,
#' SIMCOSTA buoy data, wind).
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `port`,
#'   `host`, or `launch.browser`.
#' @return Called for its side effect of launching the Shiny app.
#' @export
run_session_explorer <- function(...) {
  rlang::check_installed(
    c("shiny", "bslib", "leaflet", "reactable"),
    reason = "to run the Session Explorer app"
  )

  app_dir <- system.file(
    "shiny", "session-explorer",
    package = "yachtvaa", mustWork = TRUE
  )

  shiny::runApp(app_dir, ...)
}
