# ui.R — World > Schools Shiny App

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
  .view-toggle-sidebar { display: flex; gap: 6px; margin-bottom: 12px; }
  .view-btn {
    background-color: #2a2a2a; color: #fdfaf6;
    border: 1px solid #333333; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    flex: 1; text-align: center; transition: background-color 0.15s;
  }
  .view-btn.active { background-color: #e8ad4a; border-color: #e8ad4a; color: #1a1a1a; }
  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e4dc;
    font-size: 0.72rem; color: #999993; line-height: 1.7;
  }
  .sources-footer a { color: #999993; text-decoration: underline; }
  .card > .card-header {
    background-color: #fdfaf6; border-bottom: 1px solid #e8e4dc;
    font-size: 13px; font-weight: 500; color: #1a1a1a; padding: 8px 12px;
  }
  .nav-pills .nav-link { font-size: 0.82rem; color: #1a1a1a; }
  .nav-pills .nav-link.active {
    background-color: #1a1a1a !important; color: #fdfaf6 !important;
  }
  .pisa-note {
    font-size: 0.72rem; color: #999993; margin-top: 6px; padding-left: 4px;
  }
  .plot-desc {
    font-size: 0.70rem; color: #888880; padding: 2px 12px 8px; line-height: 1.4;
  }
"))

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(bootswatch = "flatly", bg = "#fdfaf6",
                   fg = "#1a1a1a", primary = "#e8ad4a"),
  tags$head(inter_font, custom_css),
  useShinyjs(),

  sidebar = sidebar(
    width = 260,

    # View toggle (no gender toggle — no gender breakdown in this data)
    tags$label("View", style = "color: #fdfaf6; font-size: 0.85rem;"),
    div(class = "view-toggle-sidebar",
        actionButton("view_bar",  "Country ranking", class = "view-btn"),
        actionButton("view_time", "Over time",        class = "view-btn active")
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
          plotlyOutput("plot_dur", height = "420px"),
          div(class = "plot-desc", PLOT_META[["SCHED_DUR"]]$description)
        ),

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Private Enrolment Share"),
            selectInput("priv_level", NULL,
                        choices  = c("Primary", "Secondary"),
                        selected = "Primary", width = "110px")
          )),
          plotlyOutput("plot_priv", height = "420px"),
          div(class = "plot-desc", PLOT_META[["PRIV"]]$description)
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
