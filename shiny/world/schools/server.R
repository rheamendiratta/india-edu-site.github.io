# server.R — World > Schools Shiny App

server <- function(input, output, session) {

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

  # ── Current context (tab + selections) for year slider ─────────────────────
  current_context <- reactive({
    tab <- input$active_tab %||% "System Structure"
    if (tab == "System Structure") {
      # Show year range from whichever WB dataset is relevant
      list(source = "wb", plot = "PRIV", level = input$priv_level %||% "Primary")
    } else if (tab == "PISA School Insights") {
      list(source = "pisa", plot = "SCH_STR", level = NULL)
    } else {
      list(source = "none")
    }
  })

  # ── Year slider ────────────────────────────────────────────────────────────
  output$year_slider_ui <- renderUI({
    tab <- input$active_tab %||% "System Structure"
    # Tab 2 (School Infrastructure) has no global ranking view — hide slider
    if (tab == "School Infrastructure") return(NULL)
    if (selected_view() != "bar") return(NULL)

    ctx <- current_context()
    years <- if (ctx$source == "wb") {
      schools_wb |>
        filter(plot == ctx$plot, level == ctx$level, !is.na(value)) |>
        pull(year) |> unique() |> sort()
    } else if (ctx$source == "pisa") {
      pisa_schools |> pull(year) |> unique() |> sort()
    } else {
      integer(0)
    }

    if (length(years) == 0) return(NULL)
    sliderInput("selected_year", "Year",
                min = min(years), max = max(years), value = max(years),
                step = 1, sep = "", ticks = FALSE, width = "100%")
  })

  # ── Sidebar indicator descriptions ────────────────────────────────────────
  output$indicator_description <- renderUI({
    tab   <- input$active_tab %||% "System Structure"
    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.1rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-bottom: 8px;"
    hr_s  <- "border-color: #2A2A2A; margin: 8px 0;"

    india_wb <- function(plot_code, level) {
      meta <- PLOT_META[[plot_code]]
      if (is.null(meta)) return(NULL)
      row <- schools_wb |>
        filter(plot == plot_code, .data$level == level, gender == "Total",
               country_iso3 == "IND", !is.na(value)) |>
        slice_max(year, n = 1, with_ties = FALSE)
      if (nrow(row) == 0) return(NULL)
      tagList(
        div(style = lbl_s, meta$title),
        div(style = val_s, round(row$value[1], 1)),
        div(style = sub_s, paste0("WB · ", row$year[1]))
      )
    }

    if (tab == "System Structure") {
      items <- list(
        india_wb("SCHED_DUR", input$dur_level %||% "Primary"),
        india_wb("PRIV",      input$priv_level %||% "Primary")
      )
      items <- Filter(Negate(is.null), items)
      if (length(items) == 0) return(
        div(style = lbl_s, "UDISE data shown in School Infrastructure tab.")
      )
    } else if (tab == "School Infrastructure") {
      return(div(
        div(style = "font-size: 0.65rem; color: #999993; letter-spacing: 0.06em; margin-bottom: 8px; text-transform: uppercase;",
            "India (UDISE+)"),
        div(style = lbl_s, "Schools by Management Type"),
        div(style = sub_s, "Government · Private · Aided")
      ))
    } else {
      return(div(style = sub_s, "India does not participate in PISA."))
    }

    rows <- list()
    for (i in seq_along(items)) {
      rows[[length(rows) + 1]] <- items[[i]]
      if (i < length(items)) rows[[length(rows) + 1]] <- tags$hr(style = hr_s)
    }
    tagList(
      div(style = "font-size: 0.65rem; color: #999993; letter-spacing: 0.06em; margin-bottom: 8px; text-transform: uppercase;",
          "India · Latest available"),
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

  empty_plot <- function(msg = "No data available for this selection") {
    plotly_empty() |>
      layout(
        paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
        annotations = list(list(
          text = msg, x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 13, color = "#999993", family = "Inter")
        ))
      ) |> config(displayModeBar = FALSE)
  }

  # ── make_bar: horizontal bar chart, India in COL_ROSE ──────────────────────
  # plot_code: one of SCHED_DUR, COMP_DUR, PRIV  (uses schools_wb)
  # For PISA plots pass source="pisa"
  make_bar <- function(df, country_iso3_col = "country_iso3", value_col = "value",
                       title = NULL, y_label, reverse = FALSE) {
    df <- df |>
      filter(!is.na(.data[[value_col]])) |>
      group_by(.data[[country_iso3_col]]) |>
      summarise(value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
      rename(country_iso3 = 1) |>
      arrange(if (reverse) desc(value) else value) |>
      left_join(country_names, by = c("country_iso3" = "iso3c")) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", COL_ROSE, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3),
        label     = coalesce(country_name, country_iso3)
      )

    if (nrow(df) == 0) return(empty_plot())

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

  # ── make_line: grey country lines + India WB line + optional UDISE overlay ─
  # data_df should be the pre-filtered data frame for the chosen plot/level
  # india_df: rows where country_iso3 == "IND" (can be 0-row if India absent)
  # udise_df: optional overlay data frame with columns year, value
  make_line <- function(data_df, y_label,
                        udise_df = NULL, udise_label = "India · UDISE+",
                        india_col = COL_ROSE) {
    world_df <- data_df |> filter(country_iso3 != "IND", !is.na(value))
    india_df <- data_df |> filter(country_iso3 == "IND", !is.na(value))

    p <- plot_ly()

    for (ctry in unique(world_df$country_iso3)) {
      p <- add_trace(p, data = filter(world_df, country_iso3 == ctry),
                     x = ~year, y = ~value, type = "scatter", mode = "lines",
                     line = list(color = COL_GREY_LINE, width = 0.8),
                     opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }

    if (nrow(india_df) > 0) {
      p <- add_trace(p, data = india_df,
                     x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers", name = "India · World Bank",
                     line   = list(color = india_col, width = 2.5),
                     marker = list(color = india_col, size = 5),
                     hovertemplate = "India · World Bank: %{y:.1f}<extra></extra>")
    }

    if (!is.null(udise_df) && nrow(udise_df) > 0) {
      p <- add_trace(p, data = udise_df,
                     x = ~year, y = ~value,
                     type = "scatter", mode = "markers", name = udise_label,
                     marker = list(color = COL_UDISE, size = 9, symbol = "diamond"),
                     line   = list(color = COL_UDISE, width = 2, dash = "dash"),
                     hovertemplate = paste0(udise_label, ": %{y:.1f}<extra></extra>"))
    }

    line_layout(p, y_label)
  }

  # ── Helpers to get WB data for a given plot/level ──────────────────────────
  wb_data <- function(plot_code, level) {
    schools_wb |> filter(plot == plot_code, level == level)
  }

  # Map dur_level input to (plot_code, level) pair
  dur_plot_level <- function(dur_level) {
    if (dur_level == "Compulsory") {
      list(plot = "COMP_DUR", level = "Compulsory")
    } else {
      list(plot = "SCHED_DUR", level = dur_level)
    }
  }

  # ── Tab 1: Education System Duration ──────────────────────────────────────
  output$plot_dur <- renderPlotly({
    dl    <- dur_plot_level(input$dur_level %||% "Primary")
    df    <- wb_data(dl$plot, dl$level)
    meta  <- PLOT_META[[dl$plot]]
    year  <- input$selected_year

    if (selected_view() == "bar") {
      req(year)
      df_yr <- df |> filter(year == !!year)
      make_bar(df_yr, "country_iso3", "value", y_label = meta$y_label, reverse = FALSE)
    } else {
      make_line(df, meta$y_label)
    }
  })

  # ── Tab 1: Private Enrolment Share ─────────────────────────────────────────
  output$plot_priv <- renderPlotly({
    level <- input$priv_level %||% "Primary"
    df    <- wb_data("PRIV", level)
    meta  <- PLOT_META$PRIV
    year  <- input$selected_year

    # UDISE overlay: map wb level → udise wb_level column
    udise_level <- if (level == "Primary") "Primary" else "Secondary"
    udise_pt    <- udise_4002 |>
      filter(wb_level == udise_level, !is.na(value)) |>
      select(year, value)

    if (selected_view() == "bar") {
      req(year)
      df_yr <- df |> filter(year == !!year)
      make_bar(df_yr, "country_iso3", "value", y_label = meta$y_label, reverse = FALSE)
    } else {
      make_line(df, meta$y_label,
                udise_df    = udise_pt,
                udise_label = "India · UDISE+")
    }
  })

  # ── Tab 2: India Schools by Management Type ────────────────────────────────
  output$plot_mgmt <- renderPlotly({
    mgmt_cols <- c(
      "Government"      = COL_INDIGO,
      "Aided"           = COL_UDISE,
      "Private Unaided" = COL_ROSE,
      "Other"           = COL_STEEL
    )

    df <- udise_schools |> filter(!is.na(pct_schools))

    p <- plot_ly()
    for (cat in names(mgmt_cols)) {
      df_cat <- df |> filter(category == cat)
      if (nrow(df_cat) == 0) next
      p <- add_trace(p, data = df_cat,
                     x = ~year, y = ~pct_schools,
                     type = "scatter", mode = "lines+markers",
                     name = cat,
                     line   = list(color = mgmt_cols[[cat]], width = 2.5),
                     marker = list(color = mgmt_cols[[cat]], size = 5),
                     hovertemplate = paste0("<b>", cat, "</b> %{x}: %{y:.1f}%<extra></extra>"))
    }

    p |> layout(
      paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
      font   = list(family = "Inter", size = 11, color = COL_INDIGO),
      margin = list(l = 10, r = 20, t = 10, b = 40),
      xaxis  = list(title = list(text = "Year", font = list(size = 11)),
                    showgrid = TRUE, gridcolor = COL_BORDER,
                    zeroline = FALSE, tickfont = list(size = 10),
                    dtick = 2),
      yaxis  = list(title = list(text = "Share of schools (%)", font = list(size = 11)),
                    showgrid = TRUE, gridcolor = COL_BORDER,
                    tickfont = list(size = 10), tickformat = ".1f"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
    ) |> config(displayModeBar = FALSE)
  })

  # ── Tab 3: PISA plots (India absent — no India line) ──────────────────────
  make_pisa_bar <- function(plot_code, year) {
    meta <- PLOT_META[[plot_code]]
    req(year)
    df <- pisa_schools |>
      filter(plot == plot_code, year == !!year, !is.na(value)) |>
      select(country_iso3, value)
    if (nrow(df) == 0) return(empty_plot())
    make_bar(df, "country_iso3", "value", y_label = meta$y_label,
             reverse = meta$reverse_scale)
  }

  make_pisa_line <- function(plot_code) {
    meta <- PLOT_META[[plot_code]]
    df   <- pisa_schools |> filter(plot == plot_code, !is.na(value))
    if (nrow(df) == 0) return(empty_plot())

    p <- plot_ly()
    for (ctry in unique(df$country_iso3)) {
      p <- add_trace(p, data = filter(df, country_iso3 == ctry),
                     x = ~year, y = ~value, type = "scatter", mode = "lines",
                     line = list(color = COL_GREY_LINE, width = 0.8),
                     opacity = 0.5, showlegend = FALSE, hoverinfo = "skip")
    }
    line_layout(p, meta$y_label)
  }

  output$plot_str <- renderPlotly({
    if (selected_view() == "bar") make_pisa_bar("SCH_STR", input$selected_year)
    else make_pisa_line("SCH_STR")
  })

  output$plot_pct_public <- renderPlotly({
    if (selected_view() == "bar") make_pisa_bar("SCH_PCT_PUBLIC", input$selected_year)
    else make_pisa_line("SCH_PCT_PUBLIC")
  })

  output$plot_fund_gov <- renderPlotly({
    if (selected_view() == "bar") make_pisa_bar("SCH_FUND_GOV", input$selected_year)
    else make_pisa_line("SCH_FUND_GOV")
  })

  output$plot_staff_short <- renderPlotly({
    if (selected_view() == "bar") make_pisa_bar("SCH_STAFF_SHORT", input$selected_year)
    else make_pisa_line("SCH_STAFF_SHORT")
  })
}
