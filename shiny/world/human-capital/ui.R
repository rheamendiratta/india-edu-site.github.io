# ui.R — World > Human Capital & Development Shiny App

source("global.R")

inter_font <- tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css")

custom_css <- tags$style(HTML("
  * { font-family: 'Inter', sans-serif; }
  body, .bslib-page-sidebar { background-color: #fdfaf6; }
  .bslib-sidebar-layout > .sidebar { background-color: #1a1a1a; color: #fdfaf6; }
  .bslib-sidebar-layout > .sidebar label,
  .bslib-sidebar-layout > .sidebar .control-label { color: #fdfaf6; }
  .bslib-sidebar-layout > .sidebar select,
  .bslib-sidebar-layout > .sidebar .form-control {
    background-color: #2a2a2a; color: #fdfaf6; border-color: #e8ad4a;
  }
  .irs--shiny .irs-bar, .irs--shiny .irs-handle {
    background-color: #e8ad4a !important; border-color: #e8ad4a !important;
  }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background-color: #e8ad4a !important;
  }
  .india-callout {
    background-color: #2a2a2a; border-left: 3px solid #e8ad4a;
    border-radius: 4px; padding: 10px 12px; margin-top: 8px;
    color: #fdfaf6; font-size: 0.85rem;
  }
  .card { background-color: #fdfaf6; border: 1px solid #e8e4dc; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #333333; }

  .gender-btn {
    background-color: #2a2a2a; color: #fdfaf6;
    border: 1px solid #333333; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    transition: background-color 0.15s;
  }
  .gender-btn.active, .gender-btn:hover {
    background-color: #e8ad4a; border-color: #e8ad4a; color: #1a1a1a;
  }

  .gender-legend {
    display: none; margin-bottom: 12px;
    background-color: #2a2a2a; border-radius: 4px; padding: 8px 12px;
  }
  .gender-legend.visible { display: block; }
  .legend-item {
    display: flex; align-items: center; gap: 8px;
    font-size: 0.8rem; color: #fdfaf6; margin-bottom: 4px;
  }
  .legend-item:last-child { margin-bottom: 0; }
  .legend-swatch {
    width: 24px; height: 3px; border-radius: 2px; flex-shrink: 0;
  }

  .view-toggle-sidebar { display: flex; gap: 6px; margin-bottom: 12px; }
  .view-btn {
    background-color: #2a2a2a; color: #fdfaf6;
    border: 1px solid #333333; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    flex: 1; text-align: center;
    transition: background-color 0.15s;
  }
  .view-btn.active {
    background-color: #e8ad4a; border-color: #e8ad4a; color: #1a1a1a;
  }

  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e4dc;
    font-size: 0.72rem; color: #999993; line-height: 1.7;
  }
  .sources-footer a { color: #999993; text-decoration: underline; }
  .sources-footer a:hover { color: #1a1a1a; }

  .card > .card-header {
    background-color: #fdfaf6; border-bottom: 1px solid #e8e4dc;
    font-size: 13px; font-weight: 500; color: #1a1a1a; padding: 8px 12px;
  }

  .nav-pills .nav-link { font-size: 0.82rem; color: #1a1a1a; }
  .nav-pills .nav-link.active {
    background-color: #1a1a1a !important; color: #fdfaf6 !important;
  }

  /* Chart description subtitle */
  .plot-desc {
    font-size: 0.70rem; color: #888; padding: 2px 12px 8px; line-height: 1.4;
  }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    bg         = "#fdfaf6",
    fg         = "#1a1a1a",
    primary    = "#e8ad4a"
  ),
  tags$head(inter_font, custom_css),
  useShinyjs(),

  # ── Sidebar ──────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 260,

    tags$label("Gender", style = "color: #fdfaf6; font-size: 0.85rem;"),
    div(
      style = "display: flex; gap: 6px; margin-bottom: 4px;",
      actionButton("gender_total", "Total",    class = "gender-btn active", style = "flex: 1;"),
      actionButton("gender_split", "By gender", class = "gender-btn",       style = "flex: 1;")
    ),
    tags$small(
      style = "color: #888880; font-size: 0.68rem; display: block; margin-bottom: 10px;",
      "Gender split available for HCI only"
    ),

    div(id = "gender_legend", class = "gender-legend",
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #d4687a;"), span("Female")),
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #4a5899;"), span("Male"))
    ),

    tags$label("View", style = "color: #fdfaf6; font-size: 0.85rem;"),
    div(
      class = "view-toggle-sidebar",
      actionButton("view_bar",  "Country ranking", class = "view-btn"),
      actionButton("view_time", "Over time",       class = "view-btn active")
    ),

    uiOutput("year_slider_ui"),

    hr(),

    uiOutput("indicator_description")
  ),

  # ── Main content ──────────────────────────────────────────────────────────────
  navset_pill(
    id = "active_tab",

    # ── Tab 1: Human Capital Index ────────────────────────────────────────────
    nav_panel("Human Capital Index",
      layout_column_wrap(
        width = 1/2, gap = "12px",

        card(
          card_header("Human Capital Index (Overall)"),
          plotlyOutput("plot_hci", height = "420px"),
          div(class = "plot-desc", PLOT_META[["HCI"]]$description)
        ),

        card(
          card_header("Harmonized Test Scores"),
          plotlyOutput("plot_hts", height = "420px"),
          div(class = "plot-desc", PLOT_META[["HTS"]]$description)
        ),

        card(
          card_header("Learning-Adjusted Years of Schooling (LAYS)"),
          plotlyOutput("plot_lays", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LAYS"]]$description)
        ),

        card(
          card_header("Expected Years of Schooling"),
          plotlyOutput("plot_eys", height = "420px"),
          div(class = "plot-desc", PLOT_META[["EYS"]]$description)
        ),

        card(
          card_header("HCI: Under-5 Stunting Rate"),
          plotlyOutput("plot_hci_stnt", height = "420px"),
          div(class = "plot-desc", PLOT_META[["HCI_STNT"]]$description)
        ),

        card(
          card_header("HCI: Adult Survival Rate (15 to 60)"),
          plotlyOutput("plot_hci_amrt", height = "420px"),
          div(class = "plot-desc", PLOT_META[["HCI_AMRT"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "),
          tags$span("World Bank Human Capital Index (HCI); "),
          tags$a("worldbank.org/hci",
                 href = "https://www.worldbank.org/en/publication/human-capital",
                 target = "_blank"), tags$span(". "),
          tags$span("World Bank World Development Indicators; "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
          tags$span(".")
      )
    ),

    # ── Tab 2: Human Development Index ───────────────────────────────────────
    nav_panel("Human Development Index",
      layout_column_wrap(
        width = 1/2, gap = "12px",

        card(
          card_header("Human Development Index (HDI)"),
          plotlyOutput("plot_hdi", height = "420px"),
          div(class = "plot-desc", PLOT_META[["HDI"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "),
          tags$span("UNDP Human Development Reports, compiled by World Bank SSGD; "),
          tags$a("hdr.undp.org", href = "https://hdr.undp.org", target = "_blank"),
          tags$span(".")
      )
    )
  )
)
