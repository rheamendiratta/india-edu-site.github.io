# server.R — World > Teachers Shiny App

server <- function(input, output, session) {

  # ── Gender mode ────────────────────────────────────────────────────────────
  gender_mode <- reactiveVal("total")
  observeEvent(input$gender_total, {
    gender_mode("total")
    shinyjs::addClass("gender_total",    "active")
    shinyjs::removeClass("gender_split", "active")
    shinyjs::removeClass("gender_legend","visible")
  })
  observeEvent(input$gender_split, {
    gender_mode("split")
    shinyjs::addClass("gender_split",   "active")
    shinyjs::removeClass("gender_total","active")
    shinyjs::addClass("gender_legend",  "visible")
  })

  # ── View toggle ────────────────────────────────────────────────────────────
  selected_view <- reactiveVal("time")
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

  # ── Year slider ────────────────────────────────────────────────────────────
  output$year_slider_ui <- renderUI({
    if (selected_view() != "bar") return(NULL)
    tab    <- input$active_tab %||% "Teacher Workforce"
    plot   <- if (tab == "Pupil-Teacher Ratio") "TCHR_PTR" else "TCHR_FEM"
    level  <- if (tab == "Pupil-Teacher Ratio") input$ptr_level %||% "Primary"
              else input$fem_level %||% "Primary"
    years  <- teachers_wb |>
      filter(plot == plot, level == level, gender == "Total") |>
      pull(year) |> unique() |> sort()
    if (length(years) == 0) return(NULL)
    sliderInput("selected_year", "Year",
                min = min(years), max = max(years), value = max(years),
                step = 1, sep = "", ticks = FALSE, width = "100%")
  })

  # ── Sidebar descriptions ───────────────────────────────────────────────────
  output$indicator_description <- renderUI({
    tab   <- input$active_tab %||% "Teacher Workforce"
    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.1rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-bottom: 8px;"
    hr_s  <- "border-color: #2A2A2A; margin: 8px 0;"

    india_val <- function(plot_code, level, gender = "Total", label = NULL, data_src = teachers_wb) {
      meta <- PLOT_META[[plot_code]]
      if (is.null(meta)) return(NULL)
      lbl <- if (!is.null(label)) label else meta$title
      row <- data_src |>
        filter(plot == plot_code, .data$level == level, .data$gender == gender,
               country_iso3 == "IND", !is.na(value)) |>
        slice_max(year, n = 1, with_ties = FALSE)
      if (nrow(row) == 0) return(NULL)
      v <- if (row$value[1] >= 1000) formatC(row$value[1], format = "f", digits = 0, big.mark = ",")
           else as.character(round(row$value[1], 2))
      tagList(
        div(style = lbl_s, lbl),
        div(style = val_s, v),
        div(style = sub_s, paste0("WB \u00b7 ", row$year[1]))
      )
    }

    items <- if (tab == "Teacher Workforce") {
      list(india_val("TCHR_FEM", input$fem_level %||% "Primary"),
           india_val("TCHR_NUM", input$num_level %||% "Primary"))
    } else if (tab == "Teacher Training") {
      list(india_val("TCHR_TRN", input$trn_level %||% "Primary"))
    } else if (tab == "Pupil-Teacher Ratio") {
      list(india_val("TCHR_PTR", input$ptr_level %||% "Primary"))
    } else list()

    items <- Filter(Negate(is.null), items)
    if (length(items) == 0) return(NULL)

    rows <- list()
    for (i in seq_along(items)) {
      rows[[length(rows) + 1]] <- items[[i]]
      if (i < length(items)) rows[[length(rows) + 1]] <- tags$hr(style = hr_s)
    }
    tagList(
      div(style = "font-size: 0.65rem; color: #999993; letter-spacing: 0.06em; margin-bottom: 8px; text-transform: uppercase;",
          "India \u00b7 Latest available"),
      tagList(rows)
    )
  })


  # ── Layout helpers ─────────────────────────────────────────────────────────
  bar_layout <- function(p, y_label, y_range = NULL) {
    p |> layout(
      paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
      font   = list(family = "Inter", size = 11, color = COL_INDIGO),
      margin = list(l = 20, r = 20, t = 10, b = 60),
      xaxis  = list(showgrid = FALSE, zeroline = FALSE,
                    tickfont = list(size = 9, color = COL_INDIGO),
                    ticks = "", showline = FALSE),
      yaxis  = list(title = list(text = y_label, font = list(size = 11)),
                    range = y_range,
                    showgrid = TRUE, gridcolor = COL_BORDER,
                    zeroline = TRUE, zerolinecolor = COL_BORDER,
                    tickfont = list(size = 10), tickformat = ".1f"),
      showlegend = FALSE
    ) |> config(displayModeBar = FALSE)
  }

  line_layout <- function(p, y_label) {
    p |> layout(
      paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
      font   = list(family = "Inter", size = 11, color = COL_INDIGO),
      margin = list(l = 10, r = 20, t = 10, b = 40),
      xaxis  = list(showgrid = TRUE, gridcolor = COL_BORDER,
                    zeroline = FALSE, tickfont = list(size = 10)),
      yaxis  = list(title = list(text = y_label, font = list(size = 11)),
                    showgrid = TRUE, gridcolor = COL_BORDER,
                    tickfont = list(size = 10), tickformat = ".1f"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
    ) |> config(displayModeBar = FALSE)
  }

  # ── Single bar chart (always Total for non-gender plots) ───────────────────
  make_bar <- function(plot_code, .level, .gender, .year, reverse, col, y_label, y_range = NULL) {
    df <- teachers_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             year == .year, !is.na(value))

    if (nrow(df) == 0) {
      return(plotly_empty() |>
        layout(paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
               annotations = list(list(text = "No data available for this selection",
                 x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 13, color = "#999993", family = "Inter")))) |>
        config(displayModeBar = FALSE))
    }

    df <- df |>
      group_by(country_iso3) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(if (reverse) desc(value) else value) |>
      left_join(country_names, by = c("country_iso3" = "iso3c")) |>
      mutate(bar_col   = if_else(country_iso3 == "IND", col, COL_STEEL),
             country_f = factor(country_iso3, levels = country_iso3),
             label     = coalesce(country_name, country_iso3))

    n <- nrow(df)
    labeled <- unique(c(as.character(df$country_f[1]),
                        if ("IND" %in% df$country_iso3) "IND",
                        as.character(df$country_f[n])))
    t_text  <- df$label[match(labeled, df$country_iso3)]

    plot_ly(df, x = ~country_f, y = ~value, type = "bar",
            marker = list(color = ~bar_col),
            text = ~label,
            hovertemplate = "<b>%{text}</b>: %{y:.1f}<extra></extra>") |>
      bar_layout(y_label, y_range = y_range) |>
      layout(xaxis = list(tickmode = "array", tickvals = labeled,
                          ticktext = t_text, tickangle = 0))
  }

  # ── Render bar: total or gender split ──────────────────────────────────────
  render_bar <- function(plot_code, level, year, reverse = FALSE) {
    req(year)
    meta <- PLOT_META[[plot_code]]
    mode <- if (meta$has_gender) gender_mode() else "total"
    col  <- COL_ROSE

    if (mode == "total") {
      make_bar(plot_code, level, "Total", year, reverse, col, meta$y_label)
    } else {
      pf <- make_bar(plot_code, level, "Female", year, reverse, COL_FEMALE, meta$y_label)
      pm <- make_bar(plot_code, level, "Male",   year, reverse, COL_MALE,   meta$y_label)
      subplot(pf, pm, nrows = 1, shareY = TRUE, titleX = TRUE) |>
        layout(annotations = list(
          list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
          list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 12, color = COL_MALE)))) |>
        config(displayModeBar = FALSE)
    }
  }

  # ── Line chart ─────────────────────────────────────────────────────────────
  make_line <- function(plot_code, .level, .gender, col, y_label,
                        udise_df = NULL, udise_wb_level = NULL) {
    df_world <- teachers_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             !is.na(value), country_iso3 != "IND")
    df_india <- teachers_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             country_iso3 == "IND", !is.na(value))

    p <- plot_ly()
    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p, data = filter(df_world, country_iso3 == ctry),
                     x = ~year, y = ~value, type = "scatter", mode = "lines",
                     line = list(color = COL_GREY_LINE, width = 0.8),
                     opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }
    if (nrow(df_india) > 0)
      p <- add_trace(p, data = df_india, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers", name = "India (WB)",
                     line = list(color = col, width = 2.5),
                     marker = list(color = col, size = 5),
                     hovertemplate = "India (WB): %{y:.1f}<extra></extra>")

    # UDISE overlay for PTR
    if (!is.null(udise_df) && !is.null(udise_wb_level)) {
      ud <- udise_df |> filter(wb_level == udise_wb_level, !is.na(value))
      if (nrow(ud) > 0)
        p <- add_trace(p, data = ud, x = ~year, y = ~value,
                       type = "scatter", mode = "lines+markers", name = "India (UDISE)",
                       line = list(color = COL_UDISE, width = 2, dash = "dash"),
                       marker = list(color = COL_UDISE, size = 5),
                       hovertemplate = "India (UDISE): %{y:.1f}<extra></extra>")
    }
    line_layout(p, y_label)
  }

  render_timeseries <- function(plot_code, level, udise_df = NULL, udise_wb_level = NULL) {
    meta <- PLOT_META[[plot_code]]
    mode <- if (meta$has_gender) gender_mode() else "total"

    if (mode == "total") {
      make_line(plot_code, level, "Total", COL_ROSE, meta$y_label, udise_df, udise_wb_level)
    } else {
      pf <- make_line(plot_code, level, "Female", COL_FEMALE, meta$y_label)
      pm <- make_line(plot_code, level, "Male",   COL_MALE,   meta$y_label)
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
  }

  # ── Tab 4 helper: multi-level line overlay ─────────────────────────────────
  make_multi_level_line <- function(plot_code, levels, y_label) {
    level_cols <- c("Pre-primary" = "#9b8ea8", "Primary" = COL_ROSE,
                    "Secondary" = "#5b8fa6", "Tertiary" = "#7a9e7e")
    p <- plot_ly()
    for (lv in levels) {
      df_ind <- teachers_wb |>
        filter(plot == plot_code, level == lv, gender == "Total",
               country_iso3 == "IND", !is.na(value))
      col <- level_cols[lv] %||% COL_ROSE
      if (nrow(df_ind) > 0)
        p <- add_trace(p, data = df_ind, x = ~year, y = ~value,
                       type = "scatter", mode = "lines+markers", name = lv,
                       line = list(color = col, width = 2),
                       marker = list(color = col, size = 5),
                       hovertemplate = paste0(lv, ": %{y:.1f}<extra></extra>"))
    }
    line_layout(p, y_label)
  }

  make_multi_level_bar <- function(plot_code, levels, year, y_label) {
    level_cols <- c("Pre-primary" = "#9b8ea8", "Primary" = COL_ROSE,
                    "Secondary" = "#5b8fa6", "Tertiary" = "#7a9e7e")
    p <- plot_ly()
    for (lv in levels) {
      df <- teachers_wb |>
        filter(plot == plot_code, level == lv, gender == "Total",
               year == year, !is.na(value)) |>
        arrange(value) |>
        mutate(country_f = factor(country_iso3, levels = country_iso3))
      col <- level_cols[lv] %||% COL_ROSE
      if (nrow(df) > 0)
        p <- add_trace(p, data = df, x = ~value, y = ~country_f,
                       type = "bar", orientation = "h", name = lv,
                       marker = list(color = col, opacity = 0.8),
                       hovertemplate = paste0("<b>%{y}</b> (", lv, "): %{x:.1f}<extra></extra>"))
    }
    p |> bar_layout(y_label) |>
      layout(barmode = "overlay", showlegend = TRUE,
             legend = list(orientation = "h", x = 0, y = -0.15, font = list(size = 10)),
             margin = list(l = 42, r = 20, t = 10, b = 50),
             yaxis = list(title = "", showticklabels = TRUE,
                          tickfont = list(size = 6), categoryorder = "trace", automargin = TRUE))
  }

  # ── Plot outputs ────────────────────────────────────────────────────────────

  # Tab 1: Teacher numbers (no gender split — total only)
  output$plot_num <- renderPlotly({
    level <- input$num_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") make_bar("TCHR_NUM", level, "Total", year, FALSE, COL_ROSE,
                                           PLOT_META$TCHR_NUM$y_label)
    else make_line("TCHR_NUM", level, "Total", COL_ROSE, PLOT_META$TCHR_NUM$y_label)
  })

  # Tab 1: % female (no gender split — the metric IS female %)
  output$plot_fem <- renderPlotly({
    level <- input$fem_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") make_bar("TCHR_FEM", level, "Total", year, FALSE, COL_ROSE,
                                           PLOT_META$TCHR_FEM$y_label)
    else make_line("TCHR_FEM", level, "Total", COL_ROSE, PLOT_META$TCHR_FEM$y_label)
  })

  # Tab 2: trained teachers (gender split applies)
  output$plot_trn <- renderPlotly({
    level <- input$trn_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") render_bar("TCHR_TRN", level, year)
    else render_timeseries("TCHR_TRN", level)
  })

  # Tab 3: PTR (no gender split; UDISE 2007 overlay)
  output$plot_ptr <- renderPlotly({
    level <- input$ptr_level %||% "Primary"
    year  <- input$selected_year
    if (selected_view() == "bar") make_bar("TCHR_PTR", level, "Total", year, TRUE, COL_ROSE,
                                           PLOT_META$TCHR_PTR$y_label)
    else render_timeseries("TCHR_PTR", level, udise_df = udise_ptr, udise_wb_level = level)
  })

  # Tab 4 (OECD): salary ratio + instruction time
  make_oecd_bar <- function(plot_code, .level, y_label) {
    yr_max <- oecd_eag |> filter(plot == plot_code, level == .level) |> pull(year) |> max(na.rm = TRUE)
    df <- oecd_eag |>
      filter(plot == plot_code, level == .level, year == yr_max, gender == "Total", !is.na(value)) |>
      group_by(country_iso3) |> summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(value) |>
      mutate(country_f = factor(country_iso3, levels = country_iso3))
    if (nrow(df) == 0) return(plotly_empty() |> config(displayModeBar = FALSE))
    plot_ly(df, x = ~value, y = ~country_f, type = "bar", orientation = "h",
            marker = list(color = COL_STEEL),
            hovertemplate = "<b>%{y}</b>: %{x:.2f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        annotations = list(list(text = paste("Latest year:", yr_max), x = 1, y = -0.08,
          xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#999993"), xanchor = "right")),
        yaxis = list(title = "", showticklabels = TRUE,
                     tickfont = list(size = 8, color = COL_INDIGO),
                     showgrid = FALSE, categoryorder = "trace", automargin = TRUE)
      )
  }

  make_oecd_line <- function(plot_code, .level, y_label) {
    df <- oecd_eag |> filter(plot == plot_code, level == .level, gender == "Total", !is.na(value))
    p  <- plot_ly()
    for (ctry in unique(df$country_iso3)) {
      p <- add_trace(p, data = filter(df, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line = list(color = COL_GREY_LINE, width = 0.9),
        opacity = 0.6, showlegend = FALSE, hoverinfo = "skip")
    }
    line_layout(p, y_label)
  }

  output$plot_sal <- renderPlotly({
    level <- input$sal_level %||% "Primary"
    if (selected_view() == "bar") make_oecd_bar("EAG_SAL_TCH", level, "Salary ratio (vs. similar workers)")
    else make_oecd_line("EAG_SAL_TCH", level, "Salary ratio (vs. similar workers)")
  })

  output$plot_instr <- renderPlotly({
    level <- input$instr_level %||% "Primary"
    if (selected_view() == "bar") make_oecd_bar("EAG_INSTR_TIME", level, "Instruction time (hours/year)")
    else make_oecd_line("EAG_INSTR_TIME", level, "Instruction time (hours/year)")
  })
}
