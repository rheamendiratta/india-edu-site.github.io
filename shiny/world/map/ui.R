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
    padding-right: 1.8rem; text-overflow: ellipsis; overflow: hidden;
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
  .india-callout .value { font-size: 1.3rem; font-weight: 600; color: #e8ad4a; }
  .card { background-color: #fdfaf6; border: 1px solid #e8e4dc; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #333333; }
  .leaflet-container { background: #fdfaf6 !important; }
  
  /* Sources footer */
  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e4dc;
    font-size: 0.72rem; color: #999993; line-height: 1.7;
  }
  .sources-footer a { color: #999993; text-decoration: underline; }
  .sources-footer a:hover { color: #1a1a1a; }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(bootswatch = "flatly", bg = "#fdfaf6", fg = "#1a1a1a", primary = "#e8ad4a"),
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
        style = "padding: 8px 12px 0 12px; font-size: 13px; font-weight: 500; color: #1a1a1a;",
        textOutput("map_title")
      ),
      leafletOutput("choropleth", height = "620px")
    ),
    card(
      style = "margin-top: 12px;",
      plotlyOutput("coverage_chart", height = "280px")
    ),
    # Sources footer
    div(class = "sources-footer",
        tags$b("Sources: "),
        tags$span("World Bank World Development Indicators (WDI); "),
        tags$a("data.worldbank.org", href = "https://data.worldbank.org",
               target = "_blank"), tags$span(". ")
    )
  )
)