# Module: Polar Plots
# SISCORAR current, buoy current, and wind relative to athlete bearing

mod_polar_plots_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::card(
      bslib::card_header("Corrente (SISCORAR)"),
      bslib::card_body(shiny::plotOutput(ns("polar_siscorar"), height = "450px"))
    ),
    bslib::card(
      bslib::card_header("Corrente (Boia)"),
      bslib::card_body(shiny::plotOutput(ns("polar_buoy"), height = "450px"))
    ),
    bslib::card(
      bslib::card_header("Vento (Boia)"),
      bslib::card_body(shiny::plotOutput(ns("polar_wind"), height = "450px"))
    )
  )
}

mod_polar_plots_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$polar_siscorar <- shiny::renderPlot({
      req(rv$conditions)
      rlang::check_installed("ggplot2")
      rlang::check_installed("ggrepel")

      conds <- rv$conditions |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(current_speed_kmh)) |>
        dplyr::mutate(
          current_relative_deg = (current_direction_deg - bearing_deg) %% 360
        )

      if (nrow(conds) == 0) return(NULL)

      r_max <- max(5, max(conds$current_speed_kmh, na.rm = TRUE))

      ggplot2::ggplot(
        conds,
        ggplot2::aes(x = current_relative_deg, y = current_speed_kmh)
      ) +
        ggplot2::geom_point(
          ggplot2::aes(colour = avg_speed_kmh), size = 3
        ) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = fullname_athlete),
          size = 3, max.overlaps = 20
        ) +
        ggplot2::coord_polar(start = 0, direction = -1) +
        ggplot2::scale_x_continuous(
          limits = c(0, 360), breaks = c(0, 90, 180, 270),
          labels = c("A favor", "90\u00b0", "Contra", "270\u00b0")
        ) +
        ggplot2::scale_y_continuous(limits = c(0, r_max)) +
        ggplot2::scale_colour_viridis_c(
          option = "plasma", name = "Vel. (km/h)"
        ) +
        ggplot2::labs(x = NULL, y = "Vel. corrente (km/h)") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })

    output$polar_buoy <- shiny::renderPlot({
      req(rv$conditions)
      rlang::check_installed("ggplot2")
      rlang::check_installed("ggrepel")

      if (!rv$has_buoy_current ||
          !"buoy_current_speed_kmh" %in% names(rv$conditions) ||
          all(is.na(rv$conditions$buoy_current_speed_kmh))) {
        return(NULL)
      }

      conds <- rv$conditions |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(buoy_current_speed_kmh)) |>
        dplyr::mutate(
          buoy_relative_deg = (buoy_current_direction_deg - bearing_deg) %% 360
        )

      if (nrow(conds) == 0) return(NULL)

      r_max <- max(5, max(conds$buoy_current_speed_kmh, na.rm = TRUE))

      ggplot2::ggplot(
        conds,
        ggplot2::aes(x = buoy_relative_deg, y = buoy_current_speed_kmh)
      ) +
        ggplot2::geom_point(
          ggplot2::aes(colour = avg_speed_kmh), size = 3
        ) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = fullname_athlete),
          size = 3, max.overlaps = 20
        ) +
        ggplot2::coord_polar(start = 0, direction = -1) +
        ggplot2::scale_x_continuous(
          limits = c(0, 360), breaks = c(0, 90, 180, 270),
          labels = c("A favor", "90\u00b0", "Contra", "270\u00b0")
        ) +
        ggplot2::scale_y_continuous(limits = c(0, r_max)) +
        ggplot2::scale_colour_viridis_c(
          option = "plasma", name = "Vel. (km/h)"
        ) +
        ggplot2::labs(x = NULL, y = "Vel. corrente (km/h)") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })

    output$polar_wind <- shiny::renderPlot({
      req(rv$conditions)
      rlang::check_installed("ggplot2")
      rlang::check_installed("ggrepel")

      if (!rv$has_wind ||
          !"wind_speed_kmh" %in% names(rv$conditions) ||
          all(is.na(rv$conditions$wind_speed_kmh))) {
        return(NULL)
      }

      conds <- rv$conditions |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(wind_speed_kmh)) |>
        dplyr::mutate(
          wind_relative_deg = (wind_direction_deg + 180 - bearing_deg) %% 360
        )

      if (nrow(conds) == 0) return(NULL)

      r_max <- max(30, max(conds$wind_speed_kmh, na.rm = TRUE))

      ggplot2::ggplot(
        conds,
        ggplot2::aes(x = wind_relative_deg, y = wind_speed_kmh)
      ) +
        ggplot2::geom_point(
          ggplot2::aes(colour = avg_speed_kmh), size = 3
        ) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = fullname_athlete),
          size = 3, max.overlaps = 20
        ) +
        ggplot2::coord_polar(start = 0, direction = -1) +
        ggplot2::scale_x_continuous(
          limits = c(0, 360), breaks = c(0, 90, 180, 270),
          labels = c("A favor", "90\u00b0", "Contra", "270\u00b0")
        ) +
        ggplot2::scale_y_continuous(limits = c(0, r_max)) +
        ggplot2::scale_colour_viridis_c(
          option = "plasma", name = "Vel. (km/h)"
        ) +
        ggplot2::labs(x = NULL, y = "Vel. vento (km/h)") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })
  })
}
