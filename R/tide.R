# ------------------------------------------------------------------
# Tide prediction stub
# ------------------------------------------------------------------

#' Predict tide levels (stub)
#'
#' Placeholder for future tide prediction integration. Currently returns
#' NULL with an informational message.
#'
#' @param date Date or POSIXct for the prediction.
#' @param location Character location identifier. Default "salvador".
#' @return NULL (invisibly). Future versions will return tide predictions.
#' @export
tide_prediction <- function(date = Sys.Date(), location = "salvador") {
  cli::cli_inform(c(
    "i" = "Tide prediction is not yet implemented.",
    "i" = "Future versions will integrate tide API data for {location}."
  ))
  invisible(NULL)
}
