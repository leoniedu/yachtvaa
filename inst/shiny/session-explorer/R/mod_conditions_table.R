# Module: Conditions Table
# Hourly environmental conditions (wind, SISCORAR current, buoy current)

mod_conditions_table_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Condi\u00e7\u00f5es ambientais por hora"),
    bslib::card_body(
      reactable::reactableOutput(ns("conditions_tbl"))
    )
  )
}

mod_conditions_table_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$conditions_tbl <- reactable::renderReactable({
      req(rv$grib_cropped, rv$buoy_ip, rv$records_sf)

      grib_hourly <- grib_hourly_summary(
        rv$grib_cropped$u_study,
        rv$grib_cropped$v_study,
        rv$grib_data$grib_times
      )

      records_time_range <- range(rv$records_sf$timestamp)

      conditions_wide <- build_conditions_table(
        grib_hourly, rv$buoy_ip, records_time_range
      )

      # Build column definitions
      col_defs <- list(
        metric = reactable::colDef(name = "", minWidth = 140, sticky = "left")
      )
      for (col_name in setdiff(names(conditions_wide), "metric")) {
        col_defs[[col_name]] <- reactable::colDef(
          name = col_name, align = "center", minWidth = 60
        )
      }

      reactable::reactable(
        conditions_wide,
        columns = col_defs,
        compact = TRUE,
        striped = TRUE,
        bordered = TRUE,
        defaultPageSize = 10
      )
    })
  })
}
