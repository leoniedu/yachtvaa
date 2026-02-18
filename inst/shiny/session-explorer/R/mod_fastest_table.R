# Module: Fastest X meters league table
# Sortable table with environmental condition components

mod_fastest_table_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(shiny::textOutput(ns("table_title"))),
    bslib::card_body(
      reactable::reactableOutput(ns("league_tbl"))
    ),
    bslib::card_footer(
      shiny::em(
        "Componentes (vento/corrente) = Velocidade \u00d7 cos(\u00e2ngulo canoa vs corrente/vento). ",
        "Medem a intensidade na dire\u00e7\u00e3o da canoa."
      )
    )
  )
}

mod_fastest_table_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$table_title <- shiny::renderText({
      req(rv$league_fmt)
      dist <- unique(rv$league_fmt$distance_m_target)
      if (length(dist) == 1) {
        sprintf("Classifica\u00e7\u00e3o: %sm mais r\u00e1pidos", dist)
      } else {
        "Classifica\u00e7\u00e3o"
      }
    })

    output$league_tbl <- reactable::renderReactable({
      req(rv$league_fmt, rv$conditions)

      wind_class_pt <- c(
        calm = "calmo", headwind = "contra", tailwind = "a favor",
        crosswind_right = "lateral dir.", crosswind_left = "lateral esq."
      )
      current_class_pt <- c(
        following = "a favor", opposing = "contra",
        cross_right = "lateral dir.", cross_left = "lateral esq."
      )

      tbl <- rv$league_fmt |>
        dplyr::mutate(
          inicio = format(
            lubridate::with_tz(datetime, "America/Bahia"), "%H:%M"
          ),
          tempo = sprintf(
            "%d:%02d",
            floor(predicted_time_sec / 60),
            round(predicted_time_sec %% 60)
          ),
          avg_speed_kmh = round(avg_speed_kmh, 1),
          vento = paste0(
            wind_class_pt[wind_class], " (",
            round(wind_component_kmh, 1), " km/h)"
          ),
          corrente_siscorar = paste0(
            current_class_pt[current_class], " (",
            round(current_component_kmh, 1), " km/h)"
          )
        )

      cols <- list(
        rank              = reactable::colDef(name = "#", maxWidth = 40),
        fullname_athlete  = reactable::colDef(
          name = "Atleta", minWidth = 180
        ),
        inicio            = reactable::colDef(name = "In\u00edcio", maxWidth = 70),
        tempo             = reactable::colDef(name = "Tempo", maxWidth = 70),
        avg_speed_kmh     = reactable::colDef(
          name = "Vel. (km/h)", maxWidth = 90
        ),
        vento             = reactable::colDef(
          name = "Comp. Vento", minWidth = 130
        ),
        corrente_siscorar = reactable::colDef(
          name = "Comp. Corrente (SISCORAR)", minWidth = 160
        )
      )

      sel_cols <- c(
        "rank", "fullname_athlete", "inicio", "tempo",
        "avg_speed_kmh", "vento", "corrente_siscorar"
      )

      # Add buoy current column if available
      if (rv$has_buoy_current &&
          "buoy_current_class" %in% names(tbl)) {
        tbl <- tbl |>
          dplyr::mutate(
            corrente_boia = paste0(
              current_class_pt[buoy_current_class], " (",
              round(buoy_current_component_kmh, 1), " km/h)"
            )
          )
        sel_cols <- c(sel_cols, "corrente_boia")
        cols$corrente_boia <- reactable::colDef(
          name = "Comp. Corrente (Boia)", minWidth = 150
        )
      }

      tbl <- tbl |> dplyr::select(dplyr::all_of(sel_cols))

      reactable::reactable(
        tbl,
        columns = cols,
        defaultPageSize = 20,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultSorted = "rank"
      )
    })
  })
}
