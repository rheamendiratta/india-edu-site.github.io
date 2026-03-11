# Load Inter font from rsms.me
inter_font <- tags$link(
  rel  = "stylesheet",
  href = "https://rsms.me/inter/inter.css"
)

custom_css <- tags$style(HTML("
  /* Font */
  * { font-family: 'Inter', sans-serif; }

  /* Body background */
  body, .bslib-page-sidebar {
    background-color: #fafaf8;
  }

  /* Sidebar */
  .bslib-sidebar-layout > .sidebar {
    background-color: #2e3250;
    color: #f0eeea;
  }

  .bslib-sidebar-layout > .sidebar label,
  .bslib-sidebar-layout > .sidebar .control-label {
    color: #f0eeea;
  }

  /* Sidebar inputs */
  .bslib-sidebar-layout > .sidebar select,
  .bslib-sidebar-layout > .sidebar .form-control {
    background-color: #3d4268;
    color: #f0eeea;
    border-color: #c9a0a0;
  }

  /* Slider accent */
  .irs--shiny .irs-bar,
  .irs--shiny .irs-handle {
    background-color: #c9a0a0 !important;
    border-color: #c9a0a0 !important;
  }

  .irs--shiny .irs-from,
  .irs--shiny .irs-to,
  .irs--shiny .irs-single {
    background-color: #c9a0a0 !important;
  }

  /* India callout */
  .india-callout {
    background-color: #3d4268;
    border-left: 3px solid #c9a0a0;
    border-radius: 4px;
    padding: 10px 12px;
    margin-top: 8px;
    color: #f0eeea;
    font-size: 0.85rem;
  }

  .india-callout .value {
    font-size: 1.3rem;
    font-weight: 600;
    color: #c9a0a0;
  }

  /* Nav tabs */
  .nav-underline .nav-link {
    color: #2e3250;
    font-size: 0.9rem;
  }

  .nav-underline .nav-link.active {
    color: #c9a0a0;
    border-bottom-color: #c9a0a0;
  }

  /* Card */
  .card {
    background-color: #fafaf8;
    border: 1px solid #e8e6e2;
  }

  /* HR in sidebar */
  .bslib-sidebar-layout > .sidebar hr {
    border-color: #4a5080;
  }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch  = "flatly",
    base_font   = font_google("Inter"),
    bg          = "#fafaf8",
    fg          = "#2e3250",
    primary     = "#c9a0a0"
  ),
  tags$head(inter_font, custom_css),
  
  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 260,
    
    selectInput(
      "indicator",
      "Indicator",
      choices  = indicator_choices,
      selected = DEFAULT_INDICATOR
    ),
    
    sliderInput(
      "year",
      "Year",
      min   = 1970,
      max   = 2024,
      value = DEFAULT_YEAR,
      step  = 1,
      sep   = ""
    ),
    
    hr(),
    
    uiOutput("india_callout")
  ),
  
  # ── Main: side by side ────────────────────────────────────────────────────
  card(
    fluidRow(
      column(
        width = 8,  # 70%
        plotlyOutput("choropleth", height = "520px")
      ),
      column(
        width = 4,  # 30%
        plotlyOutput("coverage_chart", height = "520px")
      )
    )
  )
)