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
  
  # ── Current context ───────────────────────────────────────────────────────────
  current_context <- reactive({
    tab <- input$active_tab
    if (is.null(tab) || tab == "Enrolment & Access") {
      list(plot = "GER", level = input$ger_level %||% "Primary")
    } else {
      list(plot = "REP", level = "Primary")
    }
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
    tab          <- input$active_tab
    is_enrolment <- is.null(tab) || tab == "Enrolment & Access"
    gmode        <- gender_mode()
    genders      <- if (gmode == "split") c("Female", "Male") else "Total"
    
    if (is_enrolment) {
      plot1 <- "GER"; plot2 <- "NER"
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
      lbl1  <- "Gross Enrolment Ratio (GER)"
      lbl2  <- "Net Enrolment Rate (NER)"
      desc1 <- "Total enrolment at a given level as % of the official school-age population. May exceed 100% due to over- and under-age students."
      desc2 <- "Enrolment of the official school-age group only, as % of that population. More precisely reflects access than GER."
      src1  <- "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.ENRR"
      src2  <- "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.PRM.NENR"
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
    
    tagList(
      div(style = label_style, lbl1),
      div(style = desc_style, desc1),
      tags$a("Source: UNESCO/WDI", href = src1, target = "_blank", style = link_style),
      div(style = "font-size: 0.72rem; color: #A0A8C0; margin-bottom: 5px;",
          wb_latest(plot1, lev1)),
      make_udise_box(plot1, udise_lev1, genders),
      
      tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
      
      div(style = label_style, lbl2),
      div(style = desc_style, desc2),
      tags$a("Source: UNESCO/WDI", href = src2, target = "_blank", style = link_style),
      div(style = "font-size: 0.72rem; color: #A0A8C0; margin-bottom: 5px;",
          wb_latest(plot2, lev2)),
      make_udise_box(plot2, udise_lev2, genders)
    )
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
    udise_level <- switch(level, "Primary" = "Preparatory", "Secondary" = "Secondary", NULL)
    if (selected_view() == "bar") render_bar("GER", level, year)
    else render_timeseries("GER", level, udise_level)
  })
  
  output$plot_ner <- renderPlotly({
    level <- input$ner_level %||% "Primary"
    year  <- input$selected_year
    udise_level <- switch(level, "Primary" = "Preparatory", "Secondary" = "Secondary", NULL)
    if (selected_view() == "bar") render_bar("NER", level, year)
    else render_timeseries("NER", level, udise_level)
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
  
}