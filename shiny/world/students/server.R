server <- function(input, output, session) {
  
  # ── Gender mode ──────────────────────────────────────────────────────────────
  gender_mode <- reactiveVal("total")
  
  observeEvent(input$gender_total, {
    gender_mode("total")
    shinyjs::addClass("gender_total",     "active")
    shinyjs::removeClass("gender_split",  "active")
    shinyjs::removeClass("gender_legend", "visible")
  })
  observeEvent(input$gender_split, {
    gender_mode("split")
    shinyjs::addClass("gender_split",    "active")
    shinyjs::removeClass("gender_total", "active")
    shinyjs::addClass("gender_legend",   "visible")
  })
  
  # ── View toggle ───────────────────────────────────────────────────────────────
  selected_view <- reactiveVal("bar")
  
  observeEvent(input$view_bar, {
    selected_view("bar")
    shinyjs::addClass("view_bar",     "active")
    shinyjs::removeClass("view_time", "active")
  })
  observeEvent(input$view_time, {
    selected_view("time")
    shinyjs::addClass("view_time",   "active")
    shinyjs::removeClass("view_bar", "active")
  })
  
  # ── Current context (drives year-slider range) ───────────────────────────────
  current_context <- reactive({
    tab <- input$active_tab %||% "Enrolment & Access"
    if      (tab == "Enrolment & Access")          list(plot = "GER",  level = input$ger_level %||% "Primary")
    else if (tab == "Completion, Persistence & Flow") list(plot = "REP",  level = "Primary")
    else if (tab == "Learning Outcomes")            list(plot = "LP",          level = "Primary")
    else if (tab == "Educational Attainment")       list(plot = "ATTAIN_USEC", level = "Primary")
    else if (tab == "Literacy")                     list(plot = "LIT_ADT",    level = "Primary")
    else                                            list(plot = "GER",         level = "Primary")
  })
  
  # ── Year slider ───────────────────────────────────────────────────────────────
  output$year_slider_ui <- renderUI({
    if (selected_view() != "bar") return(NULL)
    ctx <- current_context()
    years <- students_wb |>
      filter(plot == ctx$plot, level == ctx$level, gender == "Total") |>
      pull(year) |> unique() |> sort()
    if (length(years) == 0) return(NULL)
    sliderInput("selected_year", "Year",
                min = min(years), max = max(years), value = max(years),
                step = 1, sep = "", ticks = FALSE, width = "100%"
    )
  })
  
  
  # ── Indicator descriptions ────────────────────────────────────────────────────
  output$indicator_description <- renderUI({
    tab          <- input$active_tab %||% "Enrolment & Access"
    is_enrolment  <- tab == "Enrolment & Access"
    is_flow       <- tab == "Completion, Persistence & Flow"
    is_learning   <- tab == "Learning Outcomes"
    is_attainment <- tab == "Educational Attainment"
    is_literacy   <- tab == "Literacy"
    gmode        <- gender_mode()
    genders      <- if (gmode == "split") c("Female", "Male") else "Total"
    
    if (is_literacy) {
      return(tagList(
        sidebar_entry("Adult Literacy Rate (15+)",
          "% of adults aged 15 and over who can read and write a short simple statement about everyday life.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.ADT.LITR.ZS",
          "LIT_ADT", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Youth Literacy Rate (15–24)",
          "% of people aged 15–24 who can read and write. A leading indicator of education system effectiveness in recent decades.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.ADT.1524.LT.ZS",
          "LIT_YTH", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Youth Literacy Gender Parity Index",
          "Female-to-male ratio of youth literacy rates. Values below 1 indicate girls are less likely to be literate; above 1 indicates the reverse.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.ADT.1524.LT.FM.ZS",
          "LIT_YTH_GPI", "Primary")
      ))
    }

    if (is_attainment) {
      return(tagList(
        sidebar_entry("Primary Attainment",
          "% of adults 25+ who have completed at least primary education. A cumulative measure — includes all higher attainment levels.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.CUAT.ZS",
          "ATTAIN_PRM", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Lower Secondary Attainment",
          "% of adults 25+ with at least lower secondary education (e.g. middle school / Grade 9). Threshold for basic economic participation.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.SEC.CUAT.LO.ZS",
          "ATTAIN_LSEC", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Upper Secondary Attainment",
          "% of adults 25+ with at least upper secondary (e.g. Grade 12 / A-levels). Often the threshold for skilled employment.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.SEC.CUAT.UP.ZS",
          "ATTAIN_USEC", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Tertiary Attainment (Bachelor's+)",
          "% of adults 25+ with at least a Bachelor's degree or equivalent. Measures the stock of higher-educated human capital.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.TER.CUAT.BA.ZS",
          "ATTAIN_TER", "Primary")
      ))
    }

    if (is_learning) {
      return(tagList(
        sidebar_entry("Harmonized Test Scores",
          "Average harmonised test score (scale 300–625) combining assessments across subjects and grades. Higher = better learning outcomes.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.HLOS",
          "HTS", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Learning-Adjusted Years of Schooling (LAYS)",
          "Expected years of school discounted by quality of learning. A child attending 12 years of poor-quality school may accumulate fewer LAYS than one in 8 years of strong schooling.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.LAYS",
          "LAYS", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Expected Years of Schooling",
          "Number of years a school-age child can expect to spend in education (unadjusted for quality). Input to LAYS and HDI education component.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.EYRS",
          "EYS", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Learning Poverty",
          "% of children who cannot read a simple text by age 10. Combines in-school learning deprivation with out-of-school status. Lower is better.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.LPV.PRIM",
          "LP", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("Learning Deprivation",
          "% of primary-school-age children in school who have not achieved minimum reading proficiency. The in-school component of Learning Poverty. Lower is better.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.LPV.PRIM.LD",
          "LD", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry("School Deprivation",
          "% of primary-school-age children who are out of school. The enrolment component of Learning Poverty. Lower is better.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.LPV.PRIM.SD",
          "SD", "Primary")
      ))
    }

    if (is_enrolment) {
      lev1  <- input$ger_level %||% "Primary"
      lev2  <- input$ner_level %||% "Primary"
      udise_lev1 <- switch(lev1,
                           "Primary"     = "Preparatory",
                           "Secondary"   = "Secondary",
                           "Pre-primary" = "Foundational",
                           NULL)
      udise_lev2 <- switch(lev2,
                           "Primary"   = "Preparatory",
                           "Secondary" = "Secondary",
                           NULL)
    } else {
      plot1 <- "REP";  plot2 <- "PERS"
      lev1  <- "Primary"; lev2 <- "Primary"
      udise_lev1 <- "Preparatory"; udise_lev2 <- "Preparatory"
      lbl1  <- "Repetition Rate"
      lbl2  <- "Persistence to Last Grade"
      desc1 <- "% of pupils enrolled in a grade who repeat that grade the following year. Lower = better internal efficiency."
      desc2 <- "% of Grade 1 entrants who reach the last grade of primary. Based on the reconstructed cohort method."
      src1  <- "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.REPT.ZS"
      src2  <- "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.PRSL.ZS"
    }
    
    desc_style  <- "font-size: 0.75rem; color: #A0A8C0; line-height: 1.5; margin-bottom: 4px;"
    label_style <- "font-size: 0.78rem; font-weight: 600; color: #c9a0a0; margin-bottom: 3px;"
    link_style  <- "color: #A0A8C0; font-size: 0.72rem; display: block; margin-bottom: 8px;"
    
    # India WB latest year for a plot/level
    wb_latest <- function(plot_code, level) {
      yr <- students_wb |>
        filter(plot == plot_code, level == level,
               gender == "Total", country_iso3 == "IND", !is.na(value)) |>
        pull(year) |> max(na.rm = TRUE)
      if (is.infinite(yr)) "No WB data" else paste0("Latest India WB: ", yr)
    }
    
    # UDISE callout box — Tab 1 indicators (udise_tab1, new schema)
    make_udise_box_t1 <- function(indicator_id, udise_level, genders) {
      if (is.null(udise_level)) return(NULL)
      rows <- lapply(genders, function(g) {
        row <- udise_tab1 |>
          filter(indicator_id == indicator_id, level == udise_level,
                 gender == g, !is.na(value)) |>
          slice_max(year, n = 1, with_ties = FALSE)
        if (nrow(row) == 0) return(NULL)
        g_col <- if (g == "Female") COL_FEMALE else if (g == "Male") COL_MALE else COL_OFFWHITE
        yr    <- row$year[1]
        div(style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 2px;",
            span(style = paste0("font-size: 0.72rem; color: ", g_col, ";"),
                 if (length(genders) > 1) g else "UDISE"),
            span(style = paste0("font-size: 1.05rem; font-weight: 600; color: ", g_col, ";"),
                 round(row$value[1], 1)),
            span(style = "font-size: 0.68rem; color: #A0A8C0;",
                 paste0(yr, "–", as.integer(yr) + 1))
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) return(NULL)
      div(style = paste0(
        "background: #3D4268; border-radius: 5px; padding: 8px 10px;",
        "border-left: 2px solid #c9a0a0; margin-bottom: 4px;"
      ),
      div(style = "font-size: 0.65rem; color: #A0A8C0; margin-bottom: 5px; letter-spacing: 0.05em;",
          "UDISE+"),
      tagList(rows)
      )
    }

    # One UDISE callout box per plot
    make_udise_box <- function(plot_code, udise_level, genders) {
      if (is.null(udise_level)) return(NULL)
      rows <- lapply(genders, function(g) {
        row <- students_udise |>
          filter(plot == plot_code, udise_level == udise_level, gender == g) |>
          slice_max(year, n = 1, with_ties = FALSE)
        if (nrow(row) == 0) return(NULL)
        g_col <- if (g == "Female") COL_FEMALE else if (g == "Male") COL_MALE else COL_OFFWHITE
        yr    <- row$year[1]
        div(style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 2px;",
            span(style = paste0("font-size: 0.72rem; color: ", g_col, ";"),
                 if (length(genders) > 1) g else "UDISE"),
            span(style = paste0("font-size: 1.05rem; font-weight: 600; color: ", g_col, ";"),
                 round(row$value[1], 1)),
            span(style = "font-size: 0.68rem; color: #A0A8C0;",
                 paste0(yr, "–", as.integer(yr) + 1))
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) return(NULL)
      div(style = paste0(
        "background: #3D4268; border-radius: 5px; padding: 8px 10px;",
        "border-left: 2px solid #c9a0a0; margin-bottom: 4px;"
      ),
      div(style = "font-size: 0.65rem; color: #A0A8C0; margin-bottom: 5px; letter-spacing: 0.05em;",
          "UDISE+"),
      tagList(rows)
      )
    }
    
    sidebar_entry <- function(label, desc, src_url, plot_code, level,
                              udise_box = NULL) {
      tagList(
        div(style = label_style, label),
        div(style = desc_style, desc),
        tags$a("Source: WDI", href = src_url, target = "_blank", style = link_style),
        div(style = "font-size: 0.72rem; color: #A0A8C0; margin-bottom: 5px;",
            wb_latest(plot_code, level)),
        udise_box
      )
    }

    if (is_enrolment) {
      tagList(
        sidebar_entry(
          "Gross Enrolment Ratio (GER)",
          "Total enrolment at a given level as % of the official school-age population. May exceed 100% due to over- and under-age students.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.ENRR",
          "GER", lev1, make_udise_box_t1("4010", udise_lev1, genders)
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Net Enrolment Rate (NER)",
          "Enrolment of the official school-age group only, as % of that population. More precisely reflects access than GER.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.NENR",
          "NER", lev2, make_udise_box_t1("4011", udise_lev2, genders)
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Net Intake Rate (NIR), Primary",
          "New entrants to Grade 1 of official school-entry age, as % of that age population.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.NINT.ZS",
          "NIR", "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Gross Intake Ratio (GIR), Primary",
          "All new Grade 1 entrants regardless of age, as % of the official school-entry age population. May exceed 100%.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.GINT.ZS",
          "GIR", "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Gender Parity Index (GPI)",
          "Female-to-male ratio of GER. Values below 1 indicate male-favoured enrolment; above 1 indicate female-favoured.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.ENR.PRIM.FM.ZS",
          "GPI", input$gpi_level %||% "Primary",
          make_udise_box_t1("4032", switch(input$gpi_level %||% "Primary",
            "Primary"="Preparatory", "Secondary"="Secondary", NULL), "Total")
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Out-of-School Rate",
          "% of children of official school-age who are not enrolled. Lower is better.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.UNER.ZS",
          "OOS", input$oos_level %||% "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Private School Enrolment Share",
          "% of students enrolled in private institutions. WB source; UDISE 2024-25 point estimate shown.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.PRIV.ZS",
          "PRIV", input$priv_level %||% "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Over-Age Enrolment, Primary",
          "% of primary pupils who are more than 2 years above the official age for their grade.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.OENR.ZS",
          "OVERAGE", "Primary"
        )
      )
    } else {
      tagList(
        sidebar_entry(
          "Primary Completion Rate",
          "% of children who complete primary education. Measures whether children who start primary also finish it.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.CMPT.ZS",
          "COMPL_PRM", "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Lower Secondary Completion Rate",
          "% of children who complete lower secondary (Grades 6-9). Key milestone for further education.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.SEC.CMPT.LO.ZS",
          "COMPL_LSEC", "Lower Secondary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Persistence to Grade 5",
          "% of Grade 1 pupils who reach Grade 5. Tracks early-grade retention using the reconstructed cohort method.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.PRS5.ZS",
          "PRS5", "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Persistence to Last Grade of Primary",
          "% of Grade 1 pupils who reach the final grade of primary. A broader retention measure than Grade 5.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.PRSL.ZS",
          "PERS", "Primary",
          make_udise_box("PERS", "Preparatory", genders)
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Progression to Secondary",
          "% of pupils completing primary who enrol in Grade 6. Captures the transition efficiency between cycles.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.PRSC.ZS",
          "PROG", "Primary"
        ),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        sidebar_entry(
          "Repetition Rate, Primary",
          "% of primary pupils who repeat their grade. Lower values indicate better internal efficiency.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.REPT.ZS",
          "REP", "Primary",
          make_udise_box("REP", "Preparatory", genders)
        )
      )
    }
  })
  
  # ── Plot layout helpers ───────────────────────────────────────────────────────
  
  # Bar chart layout — title handled separately, not via layout(title=)
  bar_layout <- function(p, y_label, x_range = NULL) {
    p |>
      layout(
        paper_bgcolor = COL_NEARWHITE,
        plot_bgcolor  = COL_NEARWHITE,
        font          = list(family = "Inter", size = 11, color = COL_INDIGO),
        margin        = list(l = 10, r = 20, t = 10, b = 50),
        xaxis = list(
          title        = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          range        = x_range,
          showgrid     = TRUE,
          gridcolor    = COL_BORDER,
          zeroline     = TRUE,
          zerolinecolor = COL_BORDER,
          tickfont     = list(size = 10),
          tickformat   = ".1f"
        ),
        yaxis = list(
          showticklabels = FALSE,
          showgrid       = FALSE,
          categoryorder  = "trace"
        ),
        showlegend = FALSE
      ) |>
      config(displayModeBar = FALSE)
  }
  
  line_layout <- function(p, y_label) {
    p |>
      layout(
        paper_bgcolor = COL_NEARWHITE,
        plot_bgcolor  = COL_NEARWHITE,
        font          = list(family = "Inter", size = 11, color = COL_INDIGO),
        margin        = list(l = 10, r = 20, t = 10, b = 40),
        xaxis = list(
          showgrid  = TRUE, gridcolor = COL_BORDER,
          zeroline  = FALSE, tickfont = list(size = 10)
        ),
        yaxis = list(
          title     = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          showgrid  = TRUE, gridcolor = COL_BORDER,
          tickfont  = list(size = 10), tickformat = ".1f"
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      ) |>
      config(displayModeBar = FALSE)
  }
  
  # ── Single-gender bar chart ───────────────────────────────────────────────────
  make_bar <- function(plot_code, .level, .gender, .year, reverse,
                       india_col, y_label, x_range = NULL) {
    df <- students_wb |>
      filter(plot == plot_code, level == .level,
             gender == .gender, year == .year, !is.na(value))
    
    if (nrow(df) == 0) {
      return(
        plotly_empty() |>
          layout(
            paper_bgcolor = COL_NEARWHITE,
            plot_bgcolor  = COL_NEARWHITE,
            annotations   = list(list(
              text = "Data not available for this year",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 13, color = "#A0A8C0", family = "Inter")
            ))
          ) |>
          config(displayModeBar = FALSE)
      )
    }
    
    df <- df |>
      group_by(country_iso3) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(if (reverse) desc(value) else value) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", india_col, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3)
      )
    
    n_ctry    <- nrow(df)
    tick_size <- if (n_ctry > 120) 6 else if (n_ctry > 80) 7 else 8
    
    plot_ly(df,
            x             = ~value,
            y             = ~country_f,
            type          = "bar",
            orientation   = "h",
            marker        = list(color = ~bar_col),
            hovertemplate = "<b>%{y}</b>: %{x:.1f}<extra></extra>"
    ) |>
      bar_layout(y_label, x_range = x_range) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        yaxis  = list(
          title          = "",
          showticklabels = TRUE,
          tickfont       = list(size = tick_size, color = COL_INDIGO),
          showgrid       = FALSE,
          categoryorder  = "trace",
          automargin     = TRUE
        )
      )
  }
  
  # ── Single-gender line chart ──────────────────────────────────────────────────
  make_line <- function(plot_code, .level, .gender, .udise_level, india_col, y_label) {
    df_world <- students_wb |>
      filter(plot == plot_code, level == .level,
             gender == .gender, !is.na(value), country_iso3 != "IND")
    df_india <- students_wb |>
      filter(plot == plot_code, level == .level,
             gender == .gender, country_iso3 == "IND", !is.na(value))
    df_udise <- if (!is.null(.udise_level)) {
      students_udise |>
        filter(plot == plot_code, udise_level == .udise_level, gender == .gender)
    } else {
      NULL
    }
    
    p <- plot_ly()
    
    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p,
                     data      = filter(df_world, country_iso3 == ctry),
                     x = ~year, y = ~value, type = "scatter", mode = "lines",
                     line      = list(color = COL_GREY_LINE, width = 0.8),
                     opacity   = 0.5, showlegend = FALSE, hoverinfo = "skip"
      )
    }
    
    if (nrow(df_india) > 0) {
      p <- add_trace(p,
                     data = df_india, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     name = "India (WB)",
                     line   = list(color = india_col, width = 2.5),
                     marker = list(color = india_col, size = 5),
                     hovertemplate = "India (WB): %{y:.1f}<extra></extra>"
      )
    }
    
    if (!is.null(df_udise) && nrow(df_udise) > 0) {
      p <- add_trace(p,
                     data = df_udise, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     name = "India (UDISE)",
                     line   = list(color = COL_UDISE, width = 2, dash = "dash"),
                     marker = list(color = COL_UDISE, size = 5),
                     hovertemplate = "India (UDISE): %{y:.1f}<extra></extra>"
      )
    }
    
    line_layout(p, y_label)
  }
  
  # ── Dispatch: bar (total or split) ───────────────────────────────────────────
  render_bar <- function(plot_code, level, year, reverse = FALSE) {
    req(year)
    meta <- PLOT_META[[plot_code]]
    mode <- gender_mode()
    
    if (mode == "total") {
      make_bar(plot_code, level, "Total", year, reverse, COL_ROSE, meta$y_label)
    } else {
      .l <- level; .y <- year
      df_f <- students_wb |>
        filter(plot == plot_code, level == .l, gender == "Female", year == .y, !is.na(value))
      df_m <- students_wb |>
        filter(plot == plot_code, level == .l, gender == "Male",   year == .y, !is.na(value))
      
      all_vals <- c(df_f$value, df_m$value)
      if (length(all_vals) == 0 || all(is.na(all_vals))) {
        x_range <- NULL
      } else {
        rng     <- range(all_vals, na.rm = TRUE)
        pad     <- diff(rng) * 0.08
        x_range <- c(rng[1] - pad, rng[2] + pad)
      }
      
      pf <- make_bar(plot_code, .l, "Female", .y, reverse, COL_FEMALE, meta$y_label, x_range)
      pm <- make_bar(plot_code, .l, "Male",   .y, reverse, COL_MALE,   meta$y_label, x_range)
      
      subplot(pf, pm, nrows = 1, shareY = TRUE, titleX = TRUE) |>
        layout(
          annotations = list(
            list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
            list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_MALE))
          )
        ) |>
        config(displayModeBar = FALSE)
    }
  }
  
  # ── Dispatch: line chart (total or split) ────────────────────────────────────
  render_timeseries <- function(plot_code, level, udise_level = NULL) {
    meta <- PLOT_META[[plot_code]]
    mode <- gender_mode()
    
    if (mode == "total") {
      make_line(plot_code, level, "Total", udise_level, COL_ROSE, meta$y_label)
    } else {
      pf <- make_line(plot_code, level, "Female", udise_level, COL_FEMALE, meta$y_label)
      pm <- make_line(plot_code, level, "Male",   udise_level, COL_MALE,   meta$y_label)
      
      subplot(pf, pm, nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE) |>
        layout(
          showlegend = TRUE,
          legend     = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
          annotations = list(
            list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
            list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_MALE))
          )
        ) |>
        config(displayModeBar = FALSE)
    }
  }
  
  # ── Plot outputs ──────────────────────────────────────────────────────────────

  output$plot_ger <- renderPlotly({
    level <- input$ger_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") return(render_bar("GER", level, year))
    gmode       <- gender_mode()
    genders     <- if (gmode == "split") c("Female", "Male") else "Total"
    meta        <- PLOT_META[["GER"]]

    # UDISE level matched to the selected WB level
    udise_level <- switch(level,
      "Primary"     = "Preparatory",
      "Secondary"   = "Secondary",
      NULL
    )

    make_ger_line <- function(.gender, col) {
      df_world <- students_wb |>
        filter(plot == "GER", level == level, gender == .gender,
               !is.na(value), country_iso3 != "IND")
      df_india <- students_wb |>
        filter(plot == "GER", level == level, gender == .gender,
               country_iso3 == "IND", !is.na(value))
      df_udise <- if (!is.null(udise_level)) {
        udise_tab1 |>
          filter(indicator_id == "4010", level == udise_level,
                 gender == .gender, !is.na(value))
      } else NULL

      p <- plot_ly()
      for (ctry in unique(df_world$country_iso3)) {
        p <- add_trace(p,
          data = filter(df_world, country_iso3 == ctry),
          x = ~year, y = ~value, type = "scatter", mode = "lines",
          line = list(color = COL_GREY_LINE, width = 0.8),
          opacity = 0.5, showlegend = FALSE, hoverinfo = "skip"
        )
      }
      if (nrow(df_india) > 0) {
        p <- add_trace(p, data = df_india, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India (WB)",
          line   = list(color = col, width = 2.5),
          marker = list(color = col, size = 5),
          hovertemplate = "India (WB): %{y:.1f}<extra></extra>"
        )
      }
      if (!is.null(df_udise) && nrow(df_udise) > 0) {
        p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India (UDISE)",
          line   = list(color = COL_UDISE, width = 2, dash = "dash"),
          marker = list(color = COL_UDISE, size = 5),
          hovertemplate = "India (UDISE): %{y:.1f}<extra></extra>"
        )
      }
      line_layout(p, meta$y_label)
    }

    if (gmode == "total") {
      make_ger_line("Total", COL_ROSE)
    } else {
      pf <- make_ger_line("Female", COL_FEMALE)
      pm <- make_ger_line("Male",   COL_MALE)
      subplot(pf, pm, nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE) |>
        layout(
          showlegend = TRUE,
          legend     = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
          annotations = list(
            list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
            list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 12, color = COL_MALE))
          )
        ) |>
        config(displayModeBar = FALSE)
    }
  })
  
  output$plot_ner <- renderPlotly({
    level <- input$ner_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") return(render_bar("NER", level, year))
    gmode       <- gender_mode()
    genders     <- if (gmode == "split") c("Female", "Male") else "Total"
    meta        <- PLOT_META[["NER"]]
    udise_level <- switch(level, "Primary" = "Preparatory", "Secondary" = "Secondary", NULL)

    make_ner_line <- function(.gender, col) {
      df_world <- students_wb |>
        filter(plot == "NER", level == level, gender == .gender,
               !is.na(value), country_iso3 != "IND")
      df_india <- students_wb |>
        filter(plot == "NER", level == level, gender == .gender,
               country_iso3 == "IND", !is.na(value))
      df_udise <- if (!is.null(udise_level)) {
        udise_tab1 |>
          filter(indicator_id == "4011", level == udise_level,
                 gender == .gender, !is.na(value))
      } else NULL

      p <- plot_ly()
      for (ctry in unique(df_world$country_iso3)) {
        p <- add_trace(p,
          data = filter(df_world, country_iso3 == ctry),
          x = ~year, y = ~value, type = "scatter", mode = "lines",
          line = list(color = COL_GREY_LINE, width = 0.8),
          opacity = 0.5, showlegend = FALSE, hoverinfo = "skip"
        )
      }
      if (nrow(df_india) > 0)
        p <- add_trace(p, data = df_india, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India (WB)",
          line = list(color = col, width = 2.5), marker = list(color = col, size = 5),
          hovertemplate = "India (WB): %{y:.1f}<extra></extra>")
      if (!is.null(df_udise) && nrow(df_udise) > 0)
        p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India (UDISE)",
          line = list(color = COL_UDISE, width = 2, dash = "dash"),
          marker = list(color = COL_UDISE, size = 5),
          hovertemplate = "India (UDISE): %{y:.1f}<extra></extra>")
      line_layout(p, meta$y_label)
    }

    if (gmode == "total") {
      make_ner_line("Total", COL_ROSE)
    } else {
      pf <- make_ner_line("Female", COL_FEMALE)
      pm <- make_ner_line("Male",   COL_MALE)
      subplot(pf, pm, nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE) |>
        layout(showlegend = TRUE,
               legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
               annotations = list(
                 list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
                      showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
                 list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
                      showarrow = FALSE, font = list(size = 12, color = COL_MALE)))) |>
        config(displayModeBar = FALSE)
    }
  })

  # ── Tab 5: Literacy ───────────────────────────────────────────────────────────
  output$plot_lit_adt <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("LIT_ADT", "Primary", year)
    else render_timeseries("LIT_ADT", "Primary")
  })

  output$plot_lit_yth <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("LIT_YTH", "Primary", year)
    else render_timeseries("LIT_YTH", "Primary")
  })

  # GPI: no gender split — always Total
  output$plot_lit_yth_gpi <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["LIT_YTH_GPI"]]
    if (selected_view() == "bar") make_bar("LIT_YTH_GPI", "Primary", "Total", year, FALSE, COL_ROSE, meta$y_label)
    else make_line("LIT_YTH_GPI", "Primary", "Total", NULL, COL_ROSE, meta$y_label)
  })

  # ── Tab 4: Educational Attainment ────────────────────────────────────────────
  output$plot_attain_prm <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_PRM", "Primary", year)
    else render_timeseries("ATTAIN_PRM", "Primary")
  })

  output$plot_attain_lsec <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_LSEC", "Primary", year)
    else render_timeseries("ATTAIN_LSEC", "Primary")
  })

  output$plot_attain_usec <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_USEC", "Primary", year)
    else render_timeseries("ATTAIN_USEC", "Primary")
  })

  output$plot_attain_ter <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_TER", "Primary", year)
    else render_timeseries("ATTAIN_TER", "Primary")
  })

  output$plot_attain_st <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["ATTAIN_ST"]]
    if (selected_view() == "bar") make_bar("ATTAIN_ST", "Primary", "Total", year, FALSE, COL_ROSE, meta$y_label)
    else make_line("ATTAIN_ST", "Primary", "Total", NULL, COL_ROSE, meta$y_label)
  })

  output$plot_attain_ms <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_MS", "Primary", year)
    else render_timeseries("ATTAIN_MS", "Primary")
  })

  output$plot_attain_do <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("ATTAIN_DO", "Primary", year)
    else render_timeseries("ATTAIN_DO", "Primary")
  })

  # ── Tab 3: Learning Outcomes ──────────────────────────────────────────────────
  # HTS, LAYS, EYS: HCI indicators with no gender breakdown — always Total.
  output$plot_hts <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["HTS"]]
    if (selected_view() == "bar") make_bar("HTS", "Primary", "Total", year, FALSE, COL_ROSE, meta$y_label)
    else make_line("HTS", "Primary", "Total", NULL, COL_ROSE, meta$y_label)
  })

  output$plot_lays <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["LAYS"]]
    if (selected_view() == "bar") make_bar("LAYS", "Primary", "Total", year, FALSE, COL_ROSE, meta$y_label)
    else make_line("LAYS", "Primary", "Total", NULL, COL_ROSE, meta$y_label)
  })

  output$plot_eys <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["EYS"]]
    if (selected_view() == "bar") make_bar("EYS", "Primary", "Total", year, FALSE, COL_ROSE, meta$y_label)
    else make_line("EYS", "Primary", "Total", NULL, COL_ROSE, meta$y_label)
  })

  # LP, LD, SD: WDI with gender split; reverse scale (lower = better).
  output$plot_lp <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("LP", "Primary", year, reverse = TRUE)
    else render_timeseries("LP", "Primary")
  })

  output$plot_ld <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("LD", "Primary", year, reverse = TRUE)
    else render_timeseries("LD", "Primary")
  })

  output$plot_sd <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("SD", "Primary", year, reverse = TRUE)
    else render_timeseries("SD", "Primary")
  })

  output$plot_compl_prm <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("COMPL_PRM", "Primary", year)
    else render_timeseries("COMPL_PRM", "Primary")
  })

  output$plot_compl_lsec <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("COMPL_LSEC", "Lower Secondary", year)
    else render_timeseries("COMPL_LSEC", "Lower Secondary")
  })

  output$plot_prs5 <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("PRS5", "Primary", year)
    else render_timeseries("PRS5", "Primary")
  })

  output$plot_prog <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("PROG", "Primary", year)
    else render_timeseries("PROG", "Primary")
  })

  output$plot_rep <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("REP", "Primary", year, reverse = TRUE)
    else render_timeseries("REP", "Primary", "Preparatory")
  })
  
  output$plot_pers <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("PERS", "Primary", year)
    else render_timeseries("PERS", "Primary", "Preparatory")
  })

  output$plot_nir <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("NIR", "Primary", year)
    else render_timeseries("NIR", "Primary")
  })

  output$plot_gir <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("GIR", "Primary", year)
    else render_timeseries("GIR", "Primary")
  })

  # GPI has no gender breakdown — always renders Total regardless of gender toggle.
  output$plot_gpi <- renderPlotly({
    level <- input$gpi_level %||% "Primary"
    year  <- input$selected_year
    meta  <- PLOT_META[["GPI"]]
    if (selected_view() == "bar")
      return(make_bar("GPI", level, "Total", year, FALSE, COL_ROSE, meta$y_label))
    wb_level <- level
    udise_level_gpi <- switch(level,
      "Primary"   = "Preparatory",
      "Secondary" = "Secondary",
      NULL
    )

    df_world <- students_wb |>
      filter(plot == "GPI", level == wb_level, gender == "Total",
             !is.na(value), country_iso3 != "IND")
    df_india <- students_wb |>
      filter(plot == "GPI", level == wb_level, gender == "Total",
             country_iso3 == "IND", !is.na(value))
    df_udise <- if (!is.null(udise_level_gpi)) {
      udise_tab1 |>
        filter(indicator_id == "4032", level == udise_level_gpi, !is.na(value))
    } else NULL

    p <- plot_ly()
    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p,
        data = filter(df_world, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line = list(color = COL_GREY_LINE, width = 0.8),
        opacity = 0.5, showlegend = FALSE, hoverinfo = "skip"
      )
    }
    if (nrow(df_india) > 0)
      p <- add_trace(p, data = df_india, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India (WB)",
        line = list(color = COL_ROSE, width = 2.5),
        marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India (WB): %{y:.2f}<extra></extra>")
    if (!is.null(df_udise) && nrow(df_udise) > 0)
      p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India (UDISE)",
        line = list(color = COL_UDISE, width = 2, dash = "dash"),
        marker = list(color = COL_UDISE, size = 5),
        hovertemplate = "India (UDISE): %{y:.2f}<extra></extra>")

    p |> line_layout(meta$y_label) |>
      layout(yaxis = list(tickformat = ".2f"))
  })

  output$plot_oos <- renderPlotly({
    level <- input$oos_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") render_bar("OOS", level, year, reverse = TRUE)
    else render_timeseries("OOS", level)
  })

  # PRIV has no gender breakdown in WB — always renders Total regardless of gender toggle.
  output$plot_priv <- renderPlotly({
    level <- input$priv_level %||% "Primary"
    year  <- input$selected_year
    meta  <- PLOT_META[["PRIV"]]
    if (selected_view() == "bar")
      return(make_bar("PRIV", level, "Total", year, FALSE, COL_ROSE, meta$y_label))

    df_world <- students_wb |>
      filter(plot == "PRIV", level == level, gender == "Total",
             !is.na(value), country_iso3 != "IND")
    df_india <- students_wb |>
      filter(plot == "PRIV", level == level, gender == "Total",
             country_iso3 == "IND", !is.na(value))

    # UDISE sidebar callout value for this level
    priv_wb_level <- switch(level, "Primary" = "Primary", "Secondary" = "Secondary", NULL)
    udise_priv <- if (!is.null(priv_wb_level)) {
      udise_4002 |> filter(wb_level == priv_wb_level)
    } else NULL

    p <- plot_ly()
    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p,
        data = filter(df_world, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line = list(color = COL_GREY_LINE, width = 0.8),
        opacity = 0.5, showlegend = FALSE, hoverinfo = "skip"
      )
    }
    if (nrow(df_india) > 0)
      p <- add_trace(p, data = df_india, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India (WB)",
        line = list(color = COL_ROSE, width = 2.5),
        marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India (WB): %{y:.1f}%<extra></extra>")
    if (!is.null(udise_priv) && nrow(udise_priv) > 0)
      p <- add_trace(p, data = udise_priv, x = ~year, y = ~value,
        type = "scatter", mode = "markers", name = "India (UDISE 2024-25)",
        marker = list(color = COL_UDISE, size = 8, symbol = "diamond"),
        hovertemplate = "India (UDISE): %{y:.1f}%<extra></extra>")

    line_layout(p, meta$y_label)
  })

  output$plot_overage <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("OVERAGE", "Primary", year)
    else render_timeseries("OVERAGE", "Primary")
  })

  output$plot_enrl_num <- renderPlotly({
    level <- input$enrl_num_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") make_bar("ENRL_NUM", level, "Total", year, FALSE, COL_ROSE,
                                           "Students enrolled")
    else make_line("ENRL_NUM", level, "Total", NULL, COL_ROSE, "Students enrolled")
  })

  output$plot_enrl_pctf <- renderPlotly({
    level <- input$enrl_pctf_level %||% "Primary"
    year  <- input$selected_year
    meta  <- PLOT_META[["ENRL_PCT_F"]]
    if (selected_view() == "bar") make_bar("ENRL_PCT_F", level, "Total", year, FALSE,
                                           COL_ROSE, meta$y_label)
    else make_line("ENRL_PCT_F", level, "Total", NULL, COL_ROSE, meta$y_label)
  })

  # ── PISA renders (pisa_means; India absent) ───────────────────────────────────
  make_pisa_bar <- function(plot_code, .year, y_label) {
    df <- pisa_means |>
      filter(plot == plot_code, gender == "Total", year == .year, !is.na(value))
    if (nrow(df) == 0) {
      yr_max <- pisa_means |> filter(plot == plot_code) |> pull(year) |> max(na.rm = TRUE)
      df <- pisa_means |> filter(plot == plot_code, gender == "Total", year == yr_max, !is.na(value))
    }
    if (nrow(df) == 0) return(plotly_empty() |> config(displayModeBar = FALSE))
    df <- df |>
      group_by(country_iso3) |> summarise(value = mean(value, na.rm=TRUE), .groups="drop") |>
      arrange(value) |>
      mutate(country_f = factor(country_iso3, levels = country_iso3))
    tick_size <- if (nrow(df) > 60) 7 else 8
    plot_ly(df, x = ~value, y = ~country_f, type = "bar", orientation = "h",
            marker = list(color = COL_STEEL),
            hovertemplate = "<b>%{y}</b>: %{x:.0f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        xaxis  = list(tickformat = ".0f"),
        yaxis  = list(title = "", showticklabels = TRUE,
                      tickfont = list(size = tick_size, color = COL_INDIGO),
                      showgrid = FALSE, categoryorder = "trace", automargin = TRUE)
      )
  }

  make_pisa_line <- function(plot_code, y_label) {
    df <- pisa_means |> filter(plot == plot_code, gender == "Total", !is.na(value))
    p  <- plot_ly()
    for (ctry in unique(df$country_iso3)) {
      p <- add_trace(p, data = filter(df, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line = list(color = COL_GREY_LINE, width = 0.8),
        opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }
    line_layout(p, y_label) |> layout(yaxis = list(tickformat = ".0f"))
  }

  output$plot_pisa_math <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") make_pisa_bar("PISA_MATH", year, "Mean score (PISA scale)")
    else make_pisa_line("PISA_MATH", "Mean score (PISA scale)")
  })
  output$plot_pisa_read <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") make_pisa_bar("PISA_READ", year, "Mean score (PISA scale)")
    else make_pisa_line("PISA_READ", "Mean score (PISA scale)")
  })
  output$plot_pisa_sci <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") make_pisa_bar("PISA_SCI", year, "Mean score (PISA scale)")
    else make_pisa_line("PISA_SCI", "Mean score (PISA scale)")
  })

}