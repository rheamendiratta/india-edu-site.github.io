# ui.R — World > Students Shiny App

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
  .india-callout .value { font-size: 1.3rem; font-weight: 600; color: #e8ad4a; }
  .card { background-color: #fdfaf6; border: 1px solid #e8e4dc; }
  .bslib-sidebar-layout > .sidebar hr { border-color: #333333; }

  /* Gender toggle buttons */
  .gender-btn {
    background-color: #2a2a2a; color: #fdfaf6;
    border: 1px solid #333333; border-radius: 4px;
    padding: 4px 10px; font-size: 0.8rem; cursor: pointer;
    transition: background-color 0.15s;
  }
  .gender-btn.active, .gender-btn:hover {
    background-color: #e8ad4a; border-color: #e8ad4a; color: #1a1a1a;
  }

  /* Gender legend in sidebar */
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

  /* View toggle in sidebar */
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

  /* Sources footer */
  .sources-footer {
    margin-top: 16px; padding: 10px 14px;
    border-top: 1px solid #e8e4dc;
    font-size: 0.72rem; color: #999993; line-height: 1.7;
  }
  .sources-footer a { color: #999993; text-decoration: underline; }
  .sources-footer a:hover { color: #1a1a1a; }

  /* Card header */
  .card > .card-header {
    background-color: #fdfaf6; border-bottom: 1px solid #e8e4dc;
    font-size: 13px; font-weight: 500; color: #1a1a1a; padding: 8px 12px;
  }

  /* Tab pills */
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
  
  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 260,
    
    # Gender toggle — Total vs By gender
    tags$label("Gender", style = "color: #fdfaf6; font-size: 0.85rem;"),
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
            div(class = "legend-swatch", style = "background-color: #d4687a;"),
            span("Female")
        ),
        div(class = "legend-item",
            div(class = "legend-swatch", style = "background-color: #4a5899;"),
            span("Male")
        )
    ),
    
    # View toggle — shared across all plots
    tags$label("View", style = "color: #fdfaf6; font-size: 0.85rem;"),
    div(
      class = "view-toggle-sidebar",
      actionButton("view_bar",  "Country ranking", class = "view-btn"),
      actionButton("view_time", "Over time", class = "view-btn active")
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
                  plotlyOutput("plot_ger", height = "420px"),
          div(class = "plot-desc", PLOT_META[["GER"]]$description)
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
                  plotlyOutput("plot_ner", height = "420px"),
          div(class = "plot-desc", PLOT_META[["NER"]]$description)
                ),

                card(
                  card_header("Net Intake Rate (NIR), Primary"),
                  plotlyOutput("plot_nir", height = "420px"),
          div(class = "plot-desc", PLOT_META[["NIR"]]$description)
                ),

                card(
                  card_header("Gross Intake Ratio (GIR), Primary"),
                  plotlyOutput("plot_gir", height = "420px"),
          div(class = "plot-desc", PLOT_META[["GIR"]]$description)
                ),

                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Gender Parity Index (GPI of GER)"),
                      selectInput("gpi_level", label = NULL,
                                  choices  = c("Primary", "Secondary", "Tertiary"),
                                  selected = "Primary",
                                  width    = "110px"
                      )
                    )
                  ),
                  plotlyOutput("plot_gpi", height = "420px"),
          div(class = "plot-desc", PLOT_META[["GPI"]]$description)
                ),

                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Out-of-School Rate"),
                      selectInput("oos_level", label = NULL,
                                  choices  = c("Primary", "Lower Secondary"),
                                  selected = "Primary",
                                  width    = "150px"
                      )
                    )
                  ),
                  plotlyOutput("plot_oos", height = "420px"),
          div(class = "plot-desc", PLOT_META[["OOS"]]$description)
                ),

                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Private School Enrolment Share"),
                      selectInput("priv_level", label = NULL,
                                  choices  = c("Primary", "Secondary"),
                                  selected = "Primary",
                                  width    = "110px"
                      )
                    )
                  ),
                  plotlyOutput("plot_priv", height = "420px"),
          div(class = "plot-desc", PLOT_META[["PRIV"]]$description)
                ),

                card(
                  card_header("Over-Age Enrolment, Primary"),
                  plotlyOutput("plot_overage", height = "420px"),
          div(class = "plot-desc", PLOT_META[["OVERAGE"]]$description)
                ),

                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Enrolment (Number of Students)"),
                      selectInput("enrl_num_level", label = NULL,
                                  choices  = c("Primary", "Secondary", "Secondary General"),
                                  selected = "Primary",
                                  width    = "170px")
                    )
                  ),
                  plotlyOutput("plot_enrl_num", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ENRL_NUM"]]$description)
                ),

                card(
                  card_header(
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      span("Enrolment, % Female"),
                      selectInput("enrl_pctf_level", label = NULL,
                                  choices  = c("Primary", "Secondary"),
                                  selected = "Primary",
                                  width    = "110px")
                    )
                  ),
                  plotlyOutput("plot_enrl_pctf", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ENRL_PCT_F"]]$description)
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
    
    # ── Tab 3: Learning Outcomes ──────────────────────────────────────────────
    nav_panel("Learning Outcomes",
              layout_column_wrap(
                width = 1/2, gap = "12px",

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
                  card_header("Learning Poverty, Primary"),
                  plotlyOutput("plot_lp", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LP"]]$description)
                ),

                card(
                  card_header("Learning Deprivation, Primary"),
                  plotlyOutput("plot_ld", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LD"]]$description)
                ),

                card(
                  card_header("School Deprivation, Primary"),
                  plotlyOutput("plot_sd", height = "420px"),
          div(class = "plot-desc", PLOT_META[["SD"]]$description)
                ),

                card(
                  card_header("PISA: Mean Mathematics Score"),
                  plotlyOutput("plot_pisa_math", height = "420px"),
                  div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
                      "India is not in PISA; no India line shown.")
                ),

                card(
                  card_header("PISA: Mean Reading Score"),
                  plotlyOutput("plot_pisa_read", height = "420px"),
                  div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
                      "India is not in PISA; no India line shown.")
                ),

                card(
                  card_header("PISA: Mean Science Score"),
                  plotlyOutput("plot_pisa_sci", height = "420px"),
                  div(style = "font-size:0.72rem;color:#999993;padding:4px 12px 8px;",
                      "India is not in PISA; no India line shown.")
                )
              ),
              div(class = "sources-footer",
                  tags$b("Sources: "),
                  tags$span("World Bank Human Capital Index (HCI); "),
                  tags$a("worldbank.org/hci", href = "https://www.worldbank.org/en/publication/human-capital",
                         target = "_blank"), tags$span(". "),
                  tags$span("World Bank World Development Indicators (WDI), Learning Poverty; "),
                  tags$a("data.worldbank.org", href = "https://data.worldbank.org",
                         target = "_blank"), tags$span(".")
              )
    ),

    # ── Tab 5: Literacy ───────────────────────────────────────────────────────
    nav_panel("Literacy",
              layout_column_wrap(
                width = 1/2, gap = "12px",

                card(
                  card_header("Adult Literacy Rate (15+)"),
                  plotlyOutput("plot_lit_adt", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LIT_ADT"]]$description)
                ),

                card(
                  card_header("Youth Literacy Rate (15–24)"),
                  plotlyOutput("plot_lit_yth", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LIT_YTH"]]$description)
                ),

                card(
                  card_header("Youth Literacy Gender Parity Index"),
                  plotlyOutput("plot_lit_yth_gpi", height = "420px"),
          div(class = "plot-desc", PLOT_META[["LIT_YTH_GPI"]]$description)
                )
              ),
              div(class = "sources-footer",
                  tags$b("Sources: "),
                  tags$span("World Bank World Development Indicators (WDI); "),
                  tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
                  tags$span(". Based on UNESCO Institute for Statistics (UIS) data.")
              )
    ),

    # ── Tab 4: Educational Attainment ─────────────────────────────────────────
    nav_panel("Educational Attainment",
              layout_column_wrap(
                width = 1/2, gap = "12px",

                card(
                  card_header("Primary Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_prm", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_PRM"]]$description)
                ),

                card(
                  card_header("Lower Secondary Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_lsec", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_LSEC"]]$description)
                ),

                card(
                  card_header("Upper Secondary Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_usec", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_USEC"]]$description)
                ),

                card(
                  card_header("Tertiary Attainment (Bachelor's+), Adults 25+"),
                  plotlyOutput("plot_attain_ter", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_TER"]]$description)
                ),

                card(
                  card_header("Short-Cycle Tertiary Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_st", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_ST"]]$description)
                ),

                card(
                  card_header("Master's Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_ms", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_MS"]]$description)
                ),

                card(
                  card_header("Doctoral Attainment, Adults 25+"),
                  plotlyOutput("plot_attain_do", height = "420px"),
          div(class = "plot-desc", PLOT_META[["ATTAIN_DO"]]$description)
                )
              ),
              div(class = "sources-footer",
                  tags$b("Sources: "),
                  tags$span("World Bank World Development Indicators (WDI), Educational Attainment; "),
                  tags$a("data.worldbank.org", href = "https://data.worldbank.org", target = "_blank"),
                  tags$span(". Based on UNESCO Institute for Statistics (UIS) data.")
              )
    ),

    # ── Tab 2: Completion, Persistence & Flow ─────────────────────────────────
    nav_panel("Completion, Persistence & Flow",
              layout_column_wrap(
                width = 1/2, gap = "12px",

                card(
                  card_header("Primary Completion Rate"),
                  plotlyOutput("plot_compl_prm", height = "420px"),
          div(class = "plot-desc", PLOT_META[["COMPL_PRM"]]$description)
                ),

                card(
                  card_header("Lower Secondary Completion Rate"),
                  plotlyOutput("plot_compl_lsec", height = "420px"),
          div(class = "plot-desc", PLOT_META[["COMPL_LSEC"]]$description)
                ),

                card(
                  card_header("Persistence to Grade 5"),
                  plotlyOutput("plot_prs5", height = "420px"),
          div(class = "plot-desc", PLOT_META[["PRS5"]]$description)
                ),

                card(
                  card_header("Persistence to Last Grade of Primary"),
                  plotlyOutput("plot_pers", height = "420px"),
          div(class = "plot-desc", PLOT_META[["PERS"]]$description)
                ),

                card(
                  card_header("Progression to Secondary"),
                  plotlyOutput("plot_prog", height = "420px"),
          div(class = "plot-desc", PLOT_META[["PROG"]]$description)
                ),

                card(
                  card_header("Repetition Rate, Primary"),
                  plotlyOutput("plot_rep", height = "420px"),
          div(class = "plot-desc", PLOT_META[["REP"]]$description)
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