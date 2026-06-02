# ui.R — World > Finance & Expenditure Shiny App

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
  .card { background-color: #fdfaf6; border: 1px solid #e8e4dc; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #333333; }

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

    # ── Tab 1: Overall Spending ───────────────────────────────────────────────
    nav_panel("Overall Spending",
      layout_column_wrap(
        width = 1/2, gap = "12px",

        card(
          card_header("Government Education Expenditure (% of GDP)"),
          plotlyOutput("plot_exp_gdp", height = "420px"),
          div(class = "plot-desc", PLOT_META[["EXP_GDP"]]$description)
        ),

        card(
          card_header("Government Education Expenditure (% of Total Spending)"),
          plotlyOutput("plot_exp_govtbdg", height = "420px"),
          div(class = "plot-desc", PLOT_META[["EXP_GOVTBDG"]]$description)
        ),

        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Annual Expenditure per Student — OECD (USD PPP)"),
              selectInput("oecd_exp_level", label = NULL,
                          choices  = c("Primary", "Lower Secondary", "Upper Secondary", "Tertiary"),
                          selected = "Primary",
                          width    = "170px")
            )
          ),
          plotlyOutput("plot_oecd_exp", height = "420px"),
          div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
              "India included (government sector only). OECD + partner countries.")
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "),
          tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org",
                 target = "_blank"), tags$span(". "),
          tags$span("OECD Education at a Glance (EAG); "),
          tags$a("oecd.org/education/education-at-a-glance",
                 href = "https://www.oecd.org/education/education-at-a-glance/", target = "_blank"),
          tags$span(".")
      )
    ),

    # ── Tab 2: Spending by Level ───────────────────────────────────────────────
    nav_panel("Spending by Level",
      layout_column_wrap(
        width = 1/2, gap = "12px",

        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Expenditure per Student (% of GDP per Capita)"),
              selectInput("per_stu_level", label = NULL,
                          choices  = c("Primary", "Secondary", "Tertiary"),
                          selected = "Primary",
                          width    = "120px")
            )
          ),
          plotlyOutput("plot_per_stu", height = "420px"),
          div(class = "plot-desc", PLOT_META[["EXP_PER_STU"]]$description)
        ),

        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Share of Education Budget by Level"),
              selectInput("share_level", label = NULL,
                          choices  = c("Primary", "Secondary", "Tertiary"),
                          selected = "Primary",
                          width    = "120px")
            )
          ),
          plotlyOutput("plot_share", height = "420px"),
          div(class = "plot-desc", PLOT_META[["EXP_SHARE"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "),
          tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org",
                 target = "_blank"), tags$span(". "),
          tags$span("UNESCO Institute for Statistics via WDI.")
      )
    )
  )
)
