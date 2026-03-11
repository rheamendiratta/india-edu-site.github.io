# ui.R — World > Students Shiny App

source("global.R")

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

  /* Gender toggle buttons */
  .gender-btn {
    background-color: #3d4268; color: #f0eeea;
    border: 1px solid #4a5080; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    transition: background-color 0.15s;
  }
  .gender-btn.active, .gender-btn:hover {
    background-color: #c9a0a0; border-color: #c9a0a0; color: white;
  }

  /* Gender legend in sidebar */
  .gender-legend {
    display: none; margin-bottom: 12px;
    background-color: #3d4268; border-radius: 4px; padding: 8px 12px;
  }
  .gender-legend.visible { display: block; }
  .legend-item {
    display: flex; align-items: center; gap: 8px;
    font-size: 0.8rem; color: #f0eeea; margin-bottom: 4px;
  }
  .legend-item:last-child { margin-bottom: 0; }
  .legend-swatch {
    width: 24px; height: 3px; border-radius: 2px; flex-shrink: 0;
  }

  /* View toggle in sidebar */
  .view-toggle-sidebar { display: flex; gap: 6px; margin-bottom: 12px; }
  .view-btn {
    background-color: #3d4268; color: #f0eeea;
    border: 1px solid #4a5080; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    flex: 1; text-align: center;
    transition: background-color 0.15s;
  }
  .view-btn.active {
    background-color: #c9a0a0; border-color: #c9a0a0; color: white;
  }

  /* Sources footer */
  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e6e2;
    font-size: 0.72rem; color: #A0A8C0; line-height: 1.7;
  }
  .sources-footer a { color: #A0A8C0; text-decoration: underline; }
  .sources-footer a:hover { color: #2e3250; }

  /* Card header */
  .card > .card-header {
    background-color: #fafaf8; border-bottom: 1px solid #e8e6e2;
    font-size: 13px; font-weight: 500; color: #2e3250; padding: 8px 12px;
  }

  /* Tab pills */
  .nav-pills .nav-link { font-size: 0.82rem; color: #2e3250; }
  .nav-pills .nav-link.active {
    background-color: #2e3250 !important; color: #f0eeea !important;
  }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    bootswatch = "flatly",
    bg         = "#fafaf8",
    fg         = "#2e3250",
    primary    = "#c9a0a0"
  ),
  tags$head(inter_font, custom_css),
  useShinyjs(),
  
  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 260,
    
    # Gender toggle — Total vs By gender
    tags$label("Gender", style = "color: #f0eeea; font-size: 0.85rem;"),
    div(
      style = "display: flex; gap: 6px; margin-bottom: 12px;",
      actionButton("gender_total",  "Total",
                   class = "gender-btn active", style = "flex: 1;"),
      actionButton("gender_split",  "By gender",
                   class = "gender-btn", style = "flex: 1;")
    ),
    
    # Gender legend — visible only in "by gender" mode
    div(id = "gender_legend", class = "gender-legend",
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #d4787a;"),
            span("Female")
        ),
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #5b8fa6;"),
            span("Male")
        )
    ),
    
    # View toggle — shared across all plots
    tags$label("View", style = "color: #f0eeea; font-size: 0.85rem;"),
    div(
      class = "view-toggle-sidebar",
      actionButton("view_bar",  "Country ranking", class = "view-btn active"),
      actionButton("view_time", "Over time",       class = "view-btn")
    ),
    
    # Year slider — only shown in ranking view
    uiOutput("year_slider_ui"),
    
    hr(),
    
    # Plain-language description
    uiOutput("indicator_description")
  ),
  
  # ── Main content ─────────────────────────────────────────────────────────────
  navset_pill(
    id = "active_tab",
    
    # ── Tab 1: Enrolment & Access ─────────────────────────────────────────────
    nav_panel("Enrolment & Access",
              layout_column_wrap(
                width = 1/2, gap = "12px",
                
                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Gross Enrolment Ratio (GER)"),
                      selectInput("ger_level", label = NULL,
                                  choices  = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
                                  selected = "Primary",
                                  width    = "130px"
                      )
                    )
                  ),
                  plotlyOutput("plot_ger", height = "420px")
                ),
                
                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Net Enrolment Rate (NER)"),
                      selectInput("ner_level", label = NULL,
                                  choices  = c("Primary", "Secondary"),
                                  selected = "Primary",
                                  width    = "110px"
                      )
                    )
                  ),
                  plotlyOutput("plot_ner", height = "420px")
                )
              ), # end layout_column_wrap
              
              # Sources footer
              div(class = "sources-footer",
                  tags$b("Sources: "),
                  tags$span("World Bank World Development Indicators (WDI); "),
                  tags$a("data.worldbank.org", href = "https://data.worldbank.org",
                         target = "_blank"), tags$span(". "),
                  tags$span("Unified District Information System for Education Plus (UDISE+), "),
                  tags$span("Ministry of Education, Government of India; "),
                  tags$a("udiseplus.gov.in", href = "https://udiseplus.gov.in",
                         target = "_blank"), tags$span(".")
              )
    ),
    
    # ── Tab 2: Completion, Persistence & Flow ─────────────────────────────────
    nav_panel("Completion, Persistence & Flow",
              layout_column_wrap(
                width = 1/2, gap = "12px",
                
                card(
                  card_header("Repetition Rate, Primary"),
                  plotlyOutput("plot_rep", height = "420px")
                ),
                
                card(
                  card_header("Persistence to Last Grade of Primary"),
                  plotlyOutput("plot_pers", height = "420px")
                )
              ), # end layout_column_wrap
              
              # Sources footer
              div(class = "sources-footer",
                  tags$b("Sources: "),
                  tags$span("World Bank World Development Indicators (WDI); "),
                  tags$a("data.worldbank.org", href = "https://data.worldbank.org",
                         target = "_blank"), tags$span(". "),
                  tags$span("Unified District Information System for Education Plus (UDISE+), "),
                  tags$span("Ministry of Education, Government of India; "),
                  tags$a("udiseplus.gov.in", href = "https://udiseplus.gov.in",
                         target = "_blank"), tags$span(".")
              )
    )
  )
)