# ui.R — World > Schools Shiny App

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
  .card { background-color: #fafaf8; border: 1px solid #e8e6e2; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #4a5080; }
  .gender-btn {
    background-color: #3d4268; color: #f0eeea;
    border: 1px solid #4a5080; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    transition: background-color 0.15s;
  }
  .gender-btn.active, .gender-btn:hover {
    background-color: #c9a0a0; border-color: #c9a0a0; color: white;
  }
  .view-toggle-sidebar { display: flex; gap: 6px; margin-bottom: 12px; }
  .view-btn {
    background-color: #3d4268; color: #f0eeea;
    border: 1px solid #4a5080; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    flex: 1; text-align: center; transition: background-color 0.15s;
  }
  .view-btn.active { background-color: #c9a0a0; border-color: #c9a0a0; color: white; }
  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e6e2;
    font-size: 0.72rem; color: #A0A8C0; line-height: 1.7;
  }
  .sources-footer a { color: #A0A8C0; text-decoration: underline; }
  .card > .card-header {
    background-color: #fafaf8; border-bottom: 1px solid #e8e6e2;
    font-size: 13px; font-weight: 500; color: #2e3250; padding: 8px 12px;
  }
  .nav-pills .nav-link { font-size: 0.82rem; color: #2e3250; }
  .nav-pills .nav-link.active {
    background-color: #2e3250 !important; color: #f0eeea !important;
  }
  .pisa-note {
    font-size: 0.72rem; color: #A0A8C0; margin-top: 6px; padding-left: 4px;
  }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(bootswatch = "flatly", bg = "#fafaf8",
                   fg = "#2e3250", primary = "#c9a0a0"),
  tags$head(inter_font, custom_css),
  useShinyjs(),

  sidebar = sidebar(
    width = 260,

    # View toggle (no gender toggle — no gender breakdown in this data)
    tags$label("View", style = "color: #f0eeea; font-size: 0.85rem;"),
    div(class = "view-toggle-sidebar",
        actionButton("view_bar",  "Country ranking", class = "view-btn active"),
        actionButton("view_time", "Over time",        class = "view-btn")
    ),

    uiOutput("year_slider_ui"),
    hr(),
    uiOutput("indicator_description")
  ),

  navset_pill(
    id = "active_tab",

    # ── Tab 1: System Structure ─────────────────────────────────────────────────
    nav_panel("System Structure",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Education System Duration"),
            selectInput("dur_level", NULL,
                        choices  = c("Pre-primary", "Primary", "Secondary", "Compulsory"),
                        selected = "Primary", width = "130px")
          )),
          plotlyOutput("plot_dur", height = "420px")
        ),

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Private Enrolment Share"),
            selectInput("priv_level", NULL,
                        choices  = c("Primary", "Secondary"),
                        selected = "Primary", width = "110px")
          )),
          plotlyOutput("plot_priv", height = "420px")
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
          tags$span(". Based on UNESCO Institute for Statistics data. "),
          tags$span("UDISE+, Ministry of Education, Government of India; "),
          tags$a("udiseplus.gov.in", href = "https://udiseplus.gov.in", target = "_blank"),
          tags$span("."))
    ),

    # ── Tab 2: School Infrastructure ───────────────────────────────────────────
    nav_panel("School Infrastructure",

      card(
        card_header("India: Schools by Management Type (UDISE)"),
        plotlyOutput("plot_mgmt", height = "420px")
      ),

      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("UDISE+, Ministry of Education, Government of India; "),
          tags$a("udiseplus.gov.in", href = "https://udiseplus.gov.in", target = "_blank"),
          tags$span("."))
    ),

    # ── Tab 3: PISA School Insights ─────────────────────────────────────────────
    nav_panel("PISA School Insights",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header("Student-Teacher Ratio (PISA School Questionnaire)"),
          plotlyOutput("plot_str", height = "380px"),
          div(class = "pisa-note", "India is not in PISA; India line is blank.")
        ),

        card(
          card_header("% Public Schools (PISA)"),
          plotlyOutput("plot_pct_public", height = "380px"),
          div(class = "pisa-note", "India is not in PISA; India line is blank.")
        ),

        card(
          card_header("Government Funding Share (PISA)"),
          plotlyOutput("plot_fund_gov", height = "380px"),
          div(class = "pisa-note", "India is not in PISA; India line is blank.")
        ),

        card(
          card_header("Staff Shortage Index (PISA)"),
          plotlyOutput("plot_staff_short", height = "380px"),
          div(class = "pisa-note", "India is not in PISA; India line is blank.")
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("PISA (Programme for International Student Assessment), OECD; "),
          tags$a("oecd.org/pisa", href = "https://www.oecd.org/pisa/", target = "_blank"),
          tags$span("."))
    )
  )
)
