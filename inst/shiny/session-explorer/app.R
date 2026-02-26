# YachtVAA Session Explorer
# Interactive analysis of paddler performance vs. environmental conditions

# ===== CRITICAL: Ensure cache directory exists BEFORE loading libraries =====
# Memoise marks cache as "destroyed" if directory doesn't exist when it initializes
# MUST happen BEFORE rtreinus is loaded through yachtvaa
tryCatch({
  memoise_cache <- file.path(tools::R_user_dir("rtreinus", "cache"), "memoise")
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

.app_password <- "vaa2222"

# Source Shiny modules
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "YachtVAA",
  window_title = "YachtVAA \u2014 Explorador de Treinos",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    "nav-link-padding-y" = "0.3rem",
    "font-size-base" = "0.9rem"
  ),
  fillable = TRUE,
  header = tagList(
    tags$head(
      tags$meta(name = "viewport",
                content = "width=device-width, initial-scale=1, maximum-scale=1"),
      tags$style(HTML("
        /* ---- action bar: JS moves it between navbar and sidebar layout
           so bslib's flex-column page handles visibility on all tabs  ---- */
        #action-bar {
          flex-shrink: 0;
          z-index: 1019;
        }
        #action-bar .form-group,
        #action-bar .shiny-input-container { margin-bottom: 0; }
        #action-bar .control-label { display: none; }
        #action-bar input[type='text'] {
          padding: 0.2rem 0.45rem;
          font-size: 0.85rem;
          height: auto;
        }

        /* ---- compact value boxes ---- */
        .bslib-value-box { min-height: 54px !important; }
        .bslib-value-box .card-body { padding: 0.2rem 0.4rem !important; }
        .bslib-value-box .value-box-title {
          font-size: 0.6rem !important;
          margin-bottom: 0.05rem !important;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .bslib-value-box .value-box-value {
          font-size: 0.85rem !important;
          line-height: 1.1 !important;
        }
        /* ---- nav tabs: icons only on narrow screens ---- */
        @media (max-width: 480px) {
          .navbar-nav .nav-link span.nav-text { display: none; }
          .navbar-nav .nav-link { padding-left: 0.5rem; padding-right: 0.5rem; }
        }
        /* ---- touch targets ---- */
        .bslib-sidebar-layout > .collapse-toggle { min-width: 44px; min-height: 44px; }
        .btn { min-height: 38px; }
        /* ---- sidebar inputs ---- */
        .sidebar .shiny-input-container { max-width: 100%; }
      "))
    ),
    # JS: move action bar out of tab-content into the page-level flex layout.
    # position: sticky breaks inside bslib's overflow:hidden fillable panels;
    # inserting it as a flex sibling of the sidebar layout keeps it visible
    # on every tab without needing position:fixed or padding compensation.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var bar = document.getElementById('action-bar');
        var nav = document.querySelector('nav.navbar');
        if (bar && nav && nav.parentNode) {
          nav.parentNode.insertBefore(bar, nav.nextSibling);
        }
      });
    ")),
    # Action bar: date picker + primary action buttons always visible
    tags$div(
      id = "action-bar",
      class = "bg-body-secondary border-bottom py-1 px-2",
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$div(
          class = "flex-grow-1",
          dateInput("date", NULL, value = Sys.Date(),
                    format = "dd/mm/yyyy", language = "pt-BR")
        ),
        actionButton(
          "load_btn",
          tagList(icon("play"),
                  tags$span("Carregar", class = "d-none d-sm-inline ms-1")),
          class = "btn-primary btn-sm flex-shrink-0",
          title = "Carregar Dados"
        ),
        actionButton(
          "refresh_exercises_btn",
          tagList(icon("rotate"),
                  tags$span("Atualizar", class = "d-none d-sm-inline ms-1")),
          class = "btn-outline-secondary btn-sm flex-shrink-0",
          title = "Atualizar lista de treinos"
        )
      )
    )
  ),

  sidebar = sidebar(
    width = 260,
    open = list(desktop = "open", mobile = "closed"),
    title = "Configura\u00e7\u00e3o",

    sliderInput(
      "time_range", "Hor\u00e1rio",
      min = 0, max = 23, value = c(0, 23),
      step = 1, post = "h"
    ),

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

  # Tab panels — wrap label text in span.nav-text so CSS can hide it on phones
  nav_panel(
    title = tagList(icon("users"), tags$span("Atletas", class = "nav-text ms-1")),
    value = "tab_atletas",
    mod_summary_ui("summary")
  ),
  nav_panel(
    title = tagList(icon("map"), tags$span("Mapa", class = "nav-text ms-1")),
    value = "tab_mapa",
    mod_track_map_ui("track_map")
  ),
  nav_panel(
    title = tagList(icon("cloud-sun"), tags$span("Condi\u00e7\u00f5es", class = "nav-text ms-1")),
    value = "tab_cond",
    mod_conditions_table_ui("conditions")
  ),
  nav_panel(
    title = tagList(icon("compass"), tags$span("Polares", class = "nav-text ms-1")),
    value = "tab_polares",
    mod_polar_plots_ui("polars")
  ),
  nav_panel(
    title = tagList(icon("trophy"), tags$span("Class.", class = "nav-text ms-1")),
    value = "tab_class",
    mod_fastest_table_ui("fastest")
  ),
  nav_panel(
    title = tagList(icon("clock"), tags$span("Hor\u00e1rio", class = "nav-text ms-1")),
    value = "tab_hourly",
    mod_hourly_map_ui("hourly_map")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # iOS Safari suspends WebSocket connections when the screen locks or the
  # tab goes to background. "force" reconnects the session automatically
  # instead of showing the grey disconnected overlay.
  session$allowReconnect("force")

  showModal(modalDialog(
    title = "YachtVAA",
    passwordInput("app_password", "Senha:"),
    footer = actionButton("app_login", "Entrar", class = "btn-primary w-100"),
    easyClose = FALSE
  ))

  observeEvent(input$app_login, {
    if (isTRUE(input$app_password == .app_password)) {
      removeModal()
    } else {
      showNotification("Senha incorreta.", type = "error")
    }
  })

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
    sorted <- rv$paddlers[order(rv$paddlers$fullname_athlete), ]
    choices <- setNames(sorted$id_athlete, sorted$fullname_athlete)
    checkboxGroupInput(
      "selected_athletes", "Atletas",
      choices = choices,
      selected = choices
    )
  })
  outputOptions(output, "athlete_checkboxes", suspendWhenHidden = FALSE)

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
    req(rv$records_sf, rv$buoy_ip, rv$grib_cropped, rv$grib_data, rv$the_date)

    sel_ids <- as.integer(input$selected_athletes)
    req(length(sel_ids) > 0)

    withProgress(message = "Analisando...", value = 0, {
      tryCatch({

      # Filter records to selected athletes and time range
      start_t <- as.POSIXct(
        sprintf("%s %02d:00:00", rv$the_date, input$time_range[1]),
        tz = "America/Bahia"
      )
      end_t <- as.POSIXct(
        sprintf("%s %02d:59:59", rv$the_date, input$time_range[2]),
        tz = "America/Bahia"
      )
      recs <- rv$records_sf |>
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
      }, error = function(e) {
        showNotification(
          paste("Erro na análise:", conditionMessage(e)),
          type = "error", duration = 15
        )
      })
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
