server <- function(input, output, session) {

  # ── Gender mode ───────────────────────────────────────────────────────────────
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
    years <- hci_wb |>
      filter(plot == "HCI", gender == "Total", !is.na(value)) |>
      pull(year) |> unique() |> sort()
    if (length(years) == 0) return(NULL)
    sliderInput("selected_year", "Year",
                min = min(years), max = max(years), value = max(years),
                step = 1, sep = "", ticks = FALSE, width = "100%")
  })

  # ── Indicator descriptions ────────────────────────────────────────────────────
  output$indicator_description <- renderUI({
    tab   <- input$active_tab %||% "Human Capital Index"
    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.1rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-bottom: 8px;"
    hr_s  <- "border-color: #2A2A2A; margin: 8px 0;"

    india_val <- function(plot_code, level, gender = "Total", label = NULL, data_src = hci_wb) {
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

    items <- if (tab == "Human Capital Index") {
      list(india_val("HCI",  "Total"),
           india_val("HTS",  "Primary"),
           india_val("LAYS", "Primary"),
           india_val("EYS",  "Primary"))
    } else if (tab == "Human Development Index") {
      list(india_val("HDI", "Total"))
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
          tickfont = list(size = 10), tickformat = ".2f"
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
          tickfont = list(size = 10), tickformat = ".2f"
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10))
      ) |>
      config(displayModeBar = FALSE)
  }

  # ── Single-gender bar ─────────────────────────────────────────────────────────
  make_bar <- function(plot_code, .level, .gender, .year, india_col, y_label,
                       y_range = NULL) {
    df <- hci_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             year == .year, !is.na(value))

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
        bar_col   = if_else(country_iso3 == "IND", india_col, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3),
        label     = coalesce(country_name, country_iso3)
      )

    n <- nrow(df)
    labeled <- unique(c(as.character(df$country_f[1]),
                        if ("IND" %in% df$country_iso3) "IND",
                        as.character(df$country_f[n])))
    t_text  <- df$label[match(labeled, df$country_iso3)]

    plot_ly(df, x = ~country_f, y = ~value, type = "bar",
            marker        = list(color = ~bar_col),
            text          = ~label,
            hovertemplate = "<b>%{text}</b>: %{y:.2f}<extra></extra>") |>
      bar_layout(y_label, y_range = y_range) |>
      layout(xaxis = list(tickmode = "array", tickvals = labeled,
                          ticktext = t_text, tickangle = 0))
  }

  # ── Single-gender line ────────────────────────────────────────────────────────
  make_line <- function(plot_code, .level, .gender, india_col, y_label) {
    df_world <- hci_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             !is.na(value), country_iso3 != "IND")
    df_india <- hci_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             country_iso3 == "IND", !is.na(value))

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
        line   = list(color = india_col, width = 2.5),
        marker = list(color = india_col, size = 5),
        hovertemplate = "India: %{y:.2f}<extra></extra>")
    }
    line_layout(p, y_label)
  }

  # ── Dispatch: bar (total or gender split) ─────────────────────────────────────
  render_bar <- function(plot_code, level, year, has_gender = TRUE) {
    req(year)
    meta <- PLOT_META[[plot_code]]
    mode <- gender_mode()

    if (!has_gender || mode == "total") {
      make_bar(plot_code, level, "Total", year, COL_ROSE, meta$y_label)
    } else {
      pf <- make_bar(plot_code, level, "Female", year, COL_FEMALE, meta$y_label)
      pm <- make_bar(plot_code, level, "Male",   year, COL_MALE,   meta$y_label)
      subplot(pf, pm, nrows = 1, shareY = TRUE, titleX = TRUE) |>
        layout(annotations = list(
          list(text = "Female", x = 0.22, y = 1.04, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 12, color = COL_FEMALE)),
          list(text = "Male",   x = 0.78, y = 1.04, xref = "paper", yref = "paper",
               showarrow = FALSE, font = list(size = 12, color = COL_MALE))
        )) |>
        config(displayModeBar = FALSE)
    }
  }

  # ── Dispatch: line (total or gender split) ────────────────────────────────────
  render_timeseries <- function(plot_code, level, has_gender = TRUE) {
    meta <- PLOT_META[[plot_code]]
    mode <- gender_mode()

    if (!has_gender || mode == "total") {
      make_line(plot_code, level, "Total", COL_ROSE, meta$y_label)
    } else {
      pf <- make_line(plot_code, level, "Female", COL_FEMALE, meta$y_label)
      pm <- make_line(plot_code, level, "Male",   COL_MALE,   meta$y_label)
      subplot(pf, pm, nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE) |>
        layout(
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
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

  output$plot_hci <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("HCI",  "Total",   year, has_gender = TRUE)
    else                          render_timeseries("HCI", "Total",   has_gender = TRUE)
  })

  output$plot_hts <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("HTS",  "Primary", year, has_gender = FALSE)
    else                          render_timeseries("HTS", "Primary", has_gender = FALSE)
  })

  output$plot_lays <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("LAYS", "Primary", year, has_gender = FALSE)
    else                          render_timeseries("LAYS", "Primary", has_gender = FALSE)
  })

  output$plot_eys <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("EYS",  "Primary", year, has_gender = FALSE)
    else                          render_timeseries("EYS", "Primary", has_gender = FALSE)
  })

  output$plot_hdi <- renderPlotly({
    year <- input$selected_year
    if (selected_view() == "bar") render_bar("HDI",  "Total", year, has_gender = FALSE)
    else                          render_timeseries("HDI", "Total", has_gender = FALSE)
  })

  output$plot_hci_stnt <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["HCI_STNT"]]
    if (selected_view() == "bar") make_bar("HCI_STNT", "Total", "Total", year, COL_ROSE, meta$y_label)
    else make_line("HCI_STNT", "Total", "Total", COL_ROSE, meta$y_label)
  })

  output$plot_hci_amrt <- renderPlotly({
    year <- input$selected_year
    meta <- PLOT_META[["HCI_AMRT"]]
    if (selected_view() == "bar") make_bar("HCI_AMRT", "Total", "Total", year, COL_ROSE, meta$y_label)
    else make_line("HCI_AMRT", "Total", "Total", COL_ROSE, meta$y_label)
  })
}
