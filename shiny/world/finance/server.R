server <- function(input, output, session) {

  # ── View toggle ───────────────────────────────────────────────────────────────
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

  # ── Year slider ───────────────────────────────────────────────────────────────
  output$year_slider_ui <- renderUI({
    if (selected_view() != "bar") return(NULL)
    years <- finance_wb |>
      filter(plot == "EXP_GDP", !is.na(value)) |>
      pull(year) |> unique() |> sort()
    if (length(years) == 0) return(NULL)
    sliderInput("selected_year", "Year",
                min = min(years), max = max(years), value = max(years),
                step = 1, sep = "", ticks = FALSE, width = "100%")
  })

  # ── Indicator descriptions ────────────────────────────────────────────────────
  output$indicator_description <- renderUI({
    tab   <- input$active_tab %||% "Overall Spending"
    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.1rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-bottom: 8px;"
    hr_s  <- "border-color: #2A2A2A; margin: 8px 0;"

    india_val <- function(plot_code, level, gender = "Total", label = NULL, data_src = finance_wb) {
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

    items <- if (tab == "Overall Spending") {
      list(india_val("EXP_GDP",     "Total"),
           india_val("EXP_GOVTBDG", "Total"))
    } else if (tab == "Spending by Level") {
      list(india_val("EXP_PER_STU", input$per_stu_level %||% "Primary"),
           india_val("EXP_SHARE",   input$share_level   %||% "Primary"))
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


  # ── Layout helpers ────────────────────────────────────────────────────────────
  bar_layout <- function(p, y_label, y_range = NULL) {
    p |>
      layout(
        paper_bgcolor = COL_NEARWHITE,
        plot_bgcolor  = COL_NEARWHITE,
        font          = list(family = "Inter", size = 11, color = COL_INDIGO),
        margin        = list(l = 20, r = 20, t = 10, b = 60),
        xaxis = list(showgrid = FALSE, zeroline = FALSE,
                     tickfont = list(size = 9, color = COL_INDIGO),
                     ticks = "", showline = FALSE),
        yaxis = list(
          title    = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          range    = y_range,
          showgrid = TRUE, gridcolor = COL_BORDER,
          zeroline = TRUE, zerolinecolor = COL_BORDER,
          tickfont = list(size = 10), tickformat = ".1f"
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
        xaxis = list(showgrid = TRUE, gridcolor = COL_BORDER, zeroline = FALSE,
                     tickfont = list(size = 10)),
        yaxis = list(
          title    = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          showgrid = TRUE, gridcolor = COL_BORDER,
          tickfont = list(size = 10), tickformat = ".1f"
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      ) |>
      config(displayModeBar = FALSE)
  }

  # ── Bar chart (Total only — no gender split for finance) ─────────────────────
  make_bar <- function(plot_code, .level, .year, y_label) {
    df <- finance_wb |>
      filter(plot == plot_code, level == .level, year == .year, !is.na(value))

    if (nrow(df) == 0) {
      return(
        plotly_empty() |>
          layout(paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
                 annotations = list(list(text = "Data not available for this year",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE, font = list(size = 13, color = "#999993", family = "Inter")
                 ))) |>
          config(displayModeBar = FALSE)
      )
    }

    df <- df |>
      group_by(country_iso3) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(value) |>
      left_join(country_names, by = c("country_iso3" = "iso3c")) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", COL_ROSE, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3),
        label     = coalesce(country_name, country_iso3)
      )

    n <- nrow(df)
    labeled <- unique(c(as.character(df$country_f[1]),
                        if ("IND" %in% df$country_iso3) "IND",
                        as.character(df$country_f[n])))
    t_text  <- df$label[match(labeled, df$country_iso3)]

    plot_ly(df, x = ~country_f, y = ~value, type = "bar",
            marker = list(color = ~bar_col),
            text = ~label,
            hovertemplate = "<b>%{text}</b>: %{y:.1f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(xaxis = list(tickmode = "array", tickvals = labeled,
                          ticktext = t_text, tickangle = 0))
  }

  # ── Time-series chart ─────────────────────────────────────────────────────────
  make_line <- function(plot_code, .level, y_label) {
    df_world <- finance_wb |>
      filter(plot == plot_code, level == .level, !is.na(value), country_iso3 != "IND")
    df_india <- finance_wb |>
      filter(plot == plot_code, level == .level, country_iso3 == "IND", !is.na(value))

    p <- plot_ly()

    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p,
        data    = filter(df_world, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line    = list(color = COL_GREY_LINE, width = 0.8),
        opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }

    if (nrow(df_india) > 0) {
      p <- add_trace(p, data = df_india, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India",
        line   = list(color = COL_ROSE, width = 2.5),
        marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India: %{y:.1f}<extra></extra>")
    }

    line_layout(p, y_label)
  }

  # ── Plot outputs ──────────────────────────────────────────────────────────────

  output$plot_exp_gdp <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["EXP_GDP"]]
    if (selected_view() == "bar") make_bar("EXP_GDP", "Total", year, meta$y_label)
    else make_line("EXP_GDP", "Total", meta$y_label)
  })

  output$plot_exp_govtbdg <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["EXP_GOVTBDG"]]
    if (selected_view() == "bar") make_bar("EXP_GOVTBDG", "Total", year, meta$y_label)
    else make_line("EXP_GOVTBDG", "Total", meta$y_label)
  })

  output$plot_per_stu <- renderPlotly({
    level <- input$per_stu_level %||% "Primary"
    year  <- input$selected_year
    meta  <- PLOT_META[["EXP_PER_STU"]]
    if (selected_view() == "bar") make_bar("EXP_PER_STU", level, year, meta$y_label)
    else make_line("EXP_PER_STU", level, meta$y_label)
  })

  output$plot_share <- renderPlotly({
    level <- input$share_level %||% "Primary"
    year  <- input$selected_year
    meta  <- PLOT_META[["EXP_SHARE"]]
    if (selected_view() == "bar") make_bar("EXP_SHARE", level, year, meta$y_label)
    else make_line("EXP_SHARE", level, meta$y_label)
  })

  # ── OECD expenditure per student ──────────────────────────────────────────────
  make_oecd_bar <- function(.level, y_label) {
    yr_max <- oecd_eag |> filter(plot == "EAG_EXP_STUD", level == .level) |>
      pull(year) |> max(na.rm = TRUE)
    df <- oecd_eag |>
      filter(plot == "EAG_EXP_STUD", level == .level, year == yr_max, !is.na(value)) |>
      group_by(country_iso3) |> summarise(value = mean(value, na.rm=TRUE), .groups="drop") |>
      arrange(value) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", COL_ROSE, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3)
      )
    if (nrow(df) == 0) return(plotly_empty() |> config(displayModeBar = FALSE))
    tick_size <- if (nrow(df) > 40) 7 else 8
    plot_ly(df, x = ~value, y = ~country_f, type = "bar", orientation = "h",
            marker = list(color = ~bar_col),
            hovertemplate = "<b>%{y}</b>: $%{x:,.0f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        xaxis  = list(tickformat = ",.0f"),
        yaxis  = list(title = "", showticklabels = TRUE,
                      tickfont = list(size = tick_size, color = COL_INDIGO),
                      showgrid = FALSE, categoryorder = "trace", automargin = TRUE),
        annotations = list(list(text = paste("Latest year:", yr_max), x = 1, y = -0.08,
          xref = "paper", yref = "paper", showarrow = FALSE,
          font = list(size = 9, color = "#999993"), xanchor = "right"))
      )
  }

  make_oecd_line <- function(.level, y_label) {
    df <- oecd_eag |> filter(plot == "EAG_EXP_STUD", level == .level, !is.na(value))
    df_india <- df |> filter(country_iso3 == "IND")
    df_world <- df |> filter(country_iso3 != "IND")
    p <- plot_ly()
    for (ctry in unique(df_world$country_iso3)) {
      p <- add_trace(p, data = filter(df_world, country_iso3 == ctry),
        x = ~year, y = ~value, type = "scatter", mode = "lines",
        line = list(color = COL_GREY_LINE, width = 0.8),
        opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }
    if (nrow(df_india) > 0)
      p <- add_trace(p, data = df_india, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India",
        line = list(color = COL_ROSE, width = 2.5), marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India: $%{y:,.0f}<extra></extra>")
    line_layout(p, y_label)
  }

  output$plot_oecd_exp <- renderPlotly({
    level <- input$oecd_exp_level %||% "Primary"
    if (selected_view() == "bar") make_oecd_bar(level, "USD PPP per student")
    else make_oecd_line(level, "USD PPP per student")
  })
}
