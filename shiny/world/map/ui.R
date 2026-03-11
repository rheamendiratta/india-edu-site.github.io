inter_font <- tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css")

custom_css <- tags$style(HTML("
  * { font-family: 'Inter', sans-serif; }
  body, .bslib-page-sidebar { background-color: #fafaf8; }
  .bslib-sidebar-layout > .sidebar { background-color: #2e3250; color: #f0eeea; }
  .bslib-sidebar-layout > .sidebar label,
  .bslib-sidebar-layout > .sidebar .control-label { color: #f0eeea; }
  .bslib-sidebar-layout > .sidebar select,
  .bslib-sidebar-layout > .sidebar .form-control {
    background-color: #3d4268; color: #f0eeea; border-color: #c9a0a0;
  }
  .irs--shiny .irs-bar, .irs--shiny .irs-handle {
    background-color: #c9a0a0 !important; border-color: #c9a0a0 !important;
  }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background-color: #c9a0a0 !important;
  }
  .india-callout {
    background-color: #3d4268; border-left: 3px solid #c9a0a0;
    border-radius: 4px; padding: 10px 12px; margin-top: 8px;
    color: #f0eeea; font-size: 0.85rem;
  }
  .india-callout .value { font-size: 1.3rem; font-weight: 600; color: #c9a0a0; }
  .card { background-color: #fafaf8; border: 1px solid #e8e6e2; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #4a5080; }
  .leaflet-container { background: #fafaf8 !important; }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(bootswatch = "flatly", bg = "#fafaf8", fg = "#2e3250", primary = "#c9a0a0"),
  tags$head(inter_font, custom_css),
  
  sidebar = sidebar(
    width = 260,
    selectInput("indicator", "Indicator", choices = indicator_choices, selected = DEFAULT_INDICATOR),
    sliderInput("year", "Year", min = 1970, max = 2024, value = DEFAULT_YEAR, step = 1, sep = ""),
    hr(),
    uiOutput("india_callout"),
    hr(),
    uiOutput("indicator_description")
  ),
  
  tagList(
    card(
      div(
        style = "padding: 8px 12px 0 12px; font-size: 13px; font-weight: 500; color: #2e3250;",
        textOutput("map_title")
      ),
      leafletOutput("choropleth", height = "520px")
    ),
    card(
      style = "margin-top: 12px;",
      plotlyOutput("coverage_chart", height = "280px")
    )
  )
)