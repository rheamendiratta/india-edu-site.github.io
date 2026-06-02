# ui.R — World > Teachers Shiny App

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
  .legend-item { display: flex; align-items: center; gap: 8px;
    font-size: 0.8rem; color: #fdfaf6; margin-bottom: 4px; }
  .legend-item:last-child { margin-bottom: 0; }
  .legend-swatch { width: 24px; height: 3px; border-radius: 2px; flex-shrink: 0; }
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

  /* Chart description subtitle */
  .plot-desc {
    font-size: 0.70rem; color: #888; padding: 2px 12px 8px; line-height: 1.4;
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

    tags$label("Gender", style = "color: #fdfaf6; font-size: 0.85rem;"),
    div(style = "display: flex; gap: 6px; margin-bottom: 12px;",
        actionButton("gender_total", "Total",     class = "gender-btn active", style = "flex: 1;"),
        actionButton("gender_split", "By gender", class = "gender-btn",        style = "flex: 1;")
    ),
    div(id = "gender_legend", class = "gender-legend",
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #d4687a;"), span("Female")),
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #4a5899;"), span("Male"))
    ),

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

    # ── Tab 1: Teacher Workforce ────────────────────────────────────────────────
    nav_panel("Teacher Workforce",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Teachers, Number"),
            selectInput("num_level", NULL,
                        choices = c("Primary", "Secondary"), selected = "Primary", width = "110px")
          )),
          plotlyOutput("plot_num", height = "420px"),
          div(class = "plot-desc", PLOT_META[["TCHR_NUM"]]$description)
        ),

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Teachers, % Female"),
            selectInput("fem_level", NULL,
                        choices = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
                        selected = "Primary", width = "120px")
          )),
          plotlyOutput("plot_fem", height = "420px"),
          div(class = "plot-desc", PLOT_META[["TCHR_FEM"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
          tags$span(". Based on UNESCO Institute for Statistics data."))
    ),

    # ── Tab 2: Teacher Training ─────────────────────────────────────────────────
    nav_panel("Teacher Training",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Trained Teachers (%)"),
            selectInput("trn_level", NULL,
                        choices = c("Pre-primary", "Primary", "Secondary"),
                        selected = "Primary", width = "120px")
          )),
          plotlyOutput("plot_trn", height = "420px"),
          div(class = "plot-desc", PLOT_META[["TCHR_TRN"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
          tags$span("."))
    ),

    # ── Tab 3: Pupil-Teacher Ratio ──────────────────────────────────────────────
    nav_panel("Pupil-Teacher Ratio",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Pupil-Teacher Ratio"),
            selectInput("ptr_level", NULL,
                        choices = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
                        selected = "Primary", width = "120px")
          )),
          plotlyOutput("plot_ptr", height = "420px"),
          div(class = "plot-desc", PLOT_META[["TCHR_PTR"]]$description)
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("World Bank World Development Indicators (WDI); "),
          tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
          tags$span(". "),
          tags$span("UDISE+, Ministry of Education, Government of India; "),
          tags$a("udiseplus.gov.in", href = "https://udiseplus.gov.in", target = "_blank"),
          tags$span("."))
    ),

    # ── Tab 4: PISA Teacher Insights (OECD) ────────────────────────────────────
    nav_panel("OECD Insights",
      layout_column_wrap(width = 1/2, gap = "12px",

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Teacher Salaries Relative to Similar Workers"),
            selectInput("sal_level", NULL,
                        choices  = c("Primary", "Lower Secondary", "Upper Secondary"),
                        selected = "Primary", width = "170px")
          )),
          plotlyOutput("plot_sal", height = "420px"),
          div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
              "Ratio of teacher salary to earnings of full-time workers with similar education. India absent from OECD survey.")
        ),

        card(
          card_header(div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("Intended Instruction Time (hours/year)"),
            selectInput("instr_level", NULL,
                        choices  = c("Primary", "Lower Secondary", "Upper Secondary"),
                        selected = "Primary", width = "170px")
          )),
          plotlyOutput("plot_instr", height = "420px"),
          div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
              "Intended hours of instruction per year for students. India absent from OECD survey.")
        )
      ),
      div(class = "sources-footer",
          tags$b("Sources: "), tags$span("OECD Education at a Glance (EAG); "),
          tags$a("oecd.org/education/education-at-a-glance",
                 href = "https://www.oecd.org/education/education-at-a-glance/", target = "_blank"),
          tags$span("."))
    )
  )
)
