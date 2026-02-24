# YachtVAA Session Explorer
# Interactive analysis of paddler performance vs. environmental conditions

# ===== CRITICAL: Ensure cache directory exists BEFORE loading libraries =====
# Memoise marks cache as "destroyed" if directory doesn't exist when it initializes
# MUST happen BEFORE rtreinus is loaded through yachtvaa
tryCatch({
  cache_base <- file.path(
    Sys.getenv("HOME"), "Library/Caches/org.R-project.R/R"
  )
  rtreinus_cache <- file.path(cache_base, "rtreinus")
  memoise_cache <- file.path(rtreinus_cache, "memoise")

  # Ensure directories exist before libraries load
  if (!dir.exists(memoise_cache)) {
    dir.create(memoise_cache, recursive = TRUE, showWarnings = FALSE)
  }
}, error = function(e) {
  warning("Could not pre-create cache directory: ", conditionMessage(e))
})

library(shiny)
library(bslib)
library(yachtvaa)
library(dplyr)
library(sf)
library(leaflet)
library(reactable)

# Source Shiny modules
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "YachtVAA \u2014 Explorador de Treinos",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    "nav-link-padding-y" = "0.3rem"
  ),
  fillable = TRUE,

  sidebar = sidebar(
    width = 280,
    title = "Configura\u00e7\u00e3o",

    dateInput(
      "date", "Data do treino",
      value = Sys.Date(),
      format = "dd/mm/yyyy",
      language = "pt-BR"
    ),
    sliderInput(
      "time_range", "Hor\u00e1rio",
      min = 0, max = 23, value = c(0, 23),
      step = 1, post = "h"
    ),
    checkboxInput(
      "force_refresh", "For\u00e7ar atualiza\u00e7\u00e3o (login)",
      value = FALSE
    ),
    actionButton("load_btn", "Carregar Dados",
                 class = "btn-primary w-100 mb-3"),

    # Phase 2: shown after data loads
    conditionalPanel(
      condition = "output.data_loaded",
      hr(),
      uiOutput("athlete_checkboxes"),
      numericInput(
        "distance_m", "Dist\u00e2ncia (m)",
        value = 1000, min = 100, max = 5000, step = 100
      )
    )
  ),

  # Tab panels
  nav_panel(
    title = "Atletas",
    icon = icon("users"),
    mod_summary_ui("summary")
  ),
  nav_panel(
    title = "Mapa",
    icon = icon("map"),
    mod_track_map_ui("track_map")
  ),
  nav_panel(
    title = "Condi\u00e7\u00f5es",
    icon = icon("cloud-sun"),
    mod_conditions_table_ui("conditions")
  ),

  nav_panel(
    title = "Polares",
    icon = icon("compass"),
    mod_polar_plots_ui("polars")
  ),
  nav_panel(
    title = "Classifica\u00e7\u00e3o",
    icon = icon("trophy"),
    mod_fastest_table_ui("fastest")
  ),
  nav_panel(
    title = "Mapa Hor\u00e1rio",
    icon = icon("clock"),
    mod_hourly_map_ui("hourly_map")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Shared reactive values
  rv <- reactiveValues(
    # Phase 1
    raw          = NULL,
    grib_data    = NULL,
    records_sf   = NULL,
    records_sf_bbox = NULL,
    buoy_ip      = NULL,
    paddlers     = NULL,
    grib_cropped = NULL,
    study_area   = NULL,
    # Phase 2
    fastx        = NULL,
    matched      = NULL,
    conditions   = NULL,
    league       = NULL,
    league_fmt   = NULL,
    grib_hourly  = NULL,
    # Flags
    has_wind         = FALSE,
    has_buoy_current = FALSE
  )

  # Signal to conditionalPanel that data is loaded
  output$data_loaded <- reactive(!is.null(rv$paddlers))
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # ---------------------------
  # Phase 1: Load Data
  # ---------------------------
  mod_data_loader_server("loader", input, rv)

  # ---------------------------
  # Athlete checkboxes
  # ---------------------------
  output$athlete_checkboxes <- renderUI({
    req(rv$paddlers)
    choices <- setNames(
      rv$paddlers$id_athlete,
      rv$paddlers$fullname_athlete
    )
    checkboxGroupInput(
      "selected_athletes", "Atletas",
      choices = choices,
      selected = choices
    )
  })

  # ---------------------------
  # Analysis trigger: auto-fires on data load, athlete change, or distance change
  # ---------------------------
  analysis_trigger <- reactiveVal(0)

  # ---------------------------
  # Phase 2: Auto-analysis triggers
  # ---------------------------
  # Auto-trigger whenever data loads or reloads
  observeEvent(rv$paddlers, {
    req(rv$paddlers)
    analysis_trigger(analysis_trigger() + 1)
  })

  # Auto-trigger when athlete selection changes
  observeEvent(input$selected_athletes, {
    req(rv$paddlers)
    analysis_trigger(analysis_trigger() + 1)
  }, ignoreInit = TRUE)

  # Auto-trigger when distance parameter changes
  observeEvent(input$distance_m, {
    req(rv$paddlers)
    analysis_trigger(analysis_trigger() + 1)
  }, ignoreInit = TRUE)

  # Auto-trigger when time range changes
  observeEvent(input$time_range, {
    req(rv$paddlers)
    analysis_trigger(analysis_trigger() + 1)
  }, ignoreInit = TRUE)

  # ---------------------------
  # Phase 2: Analysis pipeline
  # ---------------------------
  observeEvent(analysis_trigger(), {
    req(analysis_trigger() > 0)
    req(rv$records_sf_bbox, rv$buoy_ip, rv$grib_cropped)

    sel_ids <- as.integer(input$selected_athletes)
    req(length(sel_ids) > 0)

    withProgress(message = "Analisando...", value = 0, {

      # Filter records to selected athletes and time range
      start_t <- as.POSIXct(
        sprintf("%s %02d:00:00", rv$the_date, input$time_range[1]),
        tz = "America/Bahia"
      )
      end_t <- as.POSIXct(
        sprintf("%s %02d:59:59", rv$the_date, input$time_range[2]),
        tz = "America/Bahia"
      )
      recs <- rv$records_sf_bbox |>
        dplyr::filter(
          id_athlete %in% sel_ids,
          timestamp >= start_t,
          timestamp <= end_t
        )

      # Fastest X
      incProgress(0.2, detail = "Calculando trechos mais r\u00e1pidos...")
      rv$fastx <- fastest_straight_distance(
        recs,
        athlete_col = "id_athlete",
        time_col    = "timestamp",
        distance_m  = input$distance_m
      )

      if (nrow(rv$fastx) > 0) {
        # Spatial filter: both endpoints inside study area
        start_pts <- sf::st_as_sf(
          rv$fastx, coords = c("start_x", "start_y"),
          crs = sf::st_crs(rv$records_sf)
        )
        end_pts <- sf::st_as_sf(
          rv$fastx, coords = c("end_x", "end_y"),
          crs = sf::st_crs(rv$records_sf)
        )
        in_bbox <- lengths(sf::st_intersects(start_pts, rv$study_area)) > 0 &
          lengths(sf::st_intersects(end_pts, rv$study_area)) > 0
        rv$fastx <- rv$fastx[in_bbox, ]
      }

      # Join athlete names
      rv$fastx <- rv$fastx |>
        dplyr::left_join(
          rv$paddlers |> dplyr::select(id_athlete, fullname_athlete),
          by = "id_athlete"
        )

      # Match GRIB current
      incProgress(0.3, detail = "Combinando com dados ambientais...")
      rv$fastx <- match_grib_to_segments(
        rv$fastx,
        rv$grib_cropped$u_study,
        rv$grib_cropped$v_study,
        rv$grib_data$grib_times,
        sf::st_crs(rv$records_sf)
      )

      # Match buoy data (wind + buoy current)
      buoy_for_match <- rv$buoy_ip |>
        dplyr::transmute(
          datetime,
          wind_direction_deg = if ("wind_direction" %in% names(rv$buoy_ip))
            wind_direction else NA_real_,
          wind_speed_kmh = if ("wind_speed" %in% names(rv$buoy_ip))
            wind_speed * 3.6 else NA_real_,
          buoy_current_direction_deg = if ("current_direction" %in% names(rv$buoy_ip))
            current_direction else NA_real_,
          buoy_current_speed_kmh = if ("current_speed_kmh" %in% names(rv$buoy_ip))
            current_speed_kmh else NA_real_
        )
      rv$matched <- match_buoy_to_segments(rv$fastx, buoy_for_match)

      # Apparent conditions
      incProgress(0.2, detail = "Calculando condi\u00e7\u00f5es aparentes...")
      has_wind_in_segments <- "wind_speed_kmh" %in% names(rv$matched) &&
        any(!is.na(rv$matched$wind_speed_kmh))
      rv$conditions <- apparent_conditions(
        rv$matched, impute_missing_wind = !has_wind_in_segments
      )

      # Buoy current classification
      if (rv$has_buoy_current &&
          "buoy_current_speed_kmh" %in% names(rv$conditions) &&
          !all(is.na(rv$conditions$buoy_current_speed_kmh))) {
        buoy_rel <- relative_current(
          rv$conditions$bearing_deg,
          rv$conditions$buoy_current_direction_deg
        )
        buoy_angle_rad <- buoy_rel$current_angle_deg * pi / 180
        rv$conditions$buoy_current_angle_deg     <- buoy_rel$current_angle_deg
        rv$conditions$buoy_current_class          <- buoy_rel$current_class
        rv$conditions$buoy_current_component_kmh <-
          rv$conditions$buoy_current_speed_kmh * cos(buoy_angle_rad)
      }

      # League table
      incProgress(0.2, detail = "Construindo classifica\u00e7\u00e3o...")
      rv$league     <- build_league_table(rv$conditions,
                                          athlete_col = "fullname_athlete")
      rv$league_fmt <- format_league(rv$league, top_n = 15)

      # GRIB hourly summary
      rv$grib_hourly <- grib_hourly_summary(
        rv$grib_cropped$u_study,
        rv$grib_cropped$v_study,
        rv$grib_data$grib_times
      )
    })
  })

  # ---------------------------
  # Module servers
  # ---------------------------
  mod_summary_server("summary", rv, reactive(input$selected_athletes))
  mod_track_map_server("track_map", rv, reactive(input$selected_athletes))
  mod_conditions_table_server("conditions", rv)
  mod_polar_plots_server("polars", rv)
  mod_fastest_table_server("fastest", rv)
  mod_hourly_map_server("hourly_map", rv)
}

shinyApp(ui, server)
