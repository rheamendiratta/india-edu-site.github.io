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
    tab   <- input$active_tab %||% "Enrolment & Access"
    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.1rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-bottom: 8px;"
    hr_s  <- "border-color: #2A2A2A; margin: 8px 0;"

    india_val <- function(plot_code, level, gender = "Total", label = NULL) {
      meta <- PLOT_META[[plot_code]]
      if (is.null(meta)) return(NULL)
      lbl <- if (!is.null(label)) label else meta$title
      row <- students_wb |>
        filter(plot == plot_code, .data$level == level, .data$gender == gender,
               country_iso3 == "IND", !is.na(value)) |>
        slice_max(year, n = 1, with_ties = FALSE)
      if (nrow(row) == 0) return(NULL)
      v <- if (row$value[1] >= 1000) formatC(row$value[1], format = "f", digits = 0, big.mark = ",")
           else as.character(round(row$value[1], 1))
      tagList(
        div(style = lbl_s, lbl),
        div(style = val_s, v),
        div(style = sub_s, paste0("WB \u00b7 ", row$year[1]))
      )
    }

    items <- if (tab == "Enrolment & Access") {
      list(india_val("GER",  input$ger_level  %||% "Primary"),
           india_val("NER",  input$ner_level  %||% "Primary"),
           india_val("GPI",  input$gpi_level  %||% "Primary"),
           india_val("OOS",  input$oos_level  %||% "Primary"),
           india_val("PRIV", input$priv_level %||% "Primary"))
    } else if (tab == "Completion, Persistence & Flow") {
      list(india_val("COMPL_PRM",  "Primary"),
           india_val("COMPL_LSEC", "Lower Secondary"),
           india_val("PERS",       "Primary"),
           india_val("REP",        "Primary"))
    } else if (tab == "Learning Outcomes") {
      list(india_val("HTS",  "Primary"),
           india_val("LAYS", "Primary"),
           india_val("EYS",  "Primary"),
           india_val("LP",   "Primary"))
    } else if (tab == "Educational Attainment") {
      list(india_val("ATTAIN_PRM",  "Primary"),
           india_val("ATTAIN_LSEC", "Primary"),
           india_val("ATTAIN_USEC", "Primary"),
           india_val("ATTAIN_TER",  "Primary"))
    } else if (tab == "Literacy") {
      list(india_val("LIT_ADT", "Primary"),
           india_val("LIT_YTH", "Primary"))
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

  
  # ── Plot layout helpers ───────────────────────────────────────────────────────
  
  # Bar chart layout — title handled separately, not via layout(title=)
  bar_layout <- function(p, y_label, y_range = NULL) {
    p |>
      layout(
        paper_bgcolor = COL_NEARWHITE,
        plot_bgcolor  = COL_NEARWHITE,
        font          = list(family = "Inter", size = 11, color = COL_INDIGO),
        margin        = list(l = 20, r = 20, t = 10, b = 60),
        xaxis = list(
          showgrid = FALSE, zeroline = FALSE,
          tickfont = list(size = 9, color = COL_INDIGO),
          ticks = "", showline = FALSE
        ),
        yaxis = list(
          title    = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          range    = y_range,
          showgrid = TRUE,  gridcolor = COL_BORDER,
          zeroline = TRUE,  zerolinecolor = COL_BORDER,
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
                       india_col, y_label, y_range = NULL) {
    df <- students_wb |>
      filter(plot == plot_code, level == .level,
             gender == .gender, year == .year, !is.na(value))

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
      arrange(if (reverse) desc(value) else value) |>
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

    plot_ly(df,
      x = ~country_f, y = ~value, type = "bar",
      marker        = list(color = ~bar_col),
      text          = ~label,
      hovertemplate = "<b>%{text}</b>: %{y:.1f}<extra></extra>"
    ) |>
      bar_layout(y_label, y_range = y_range) |>
      layout(xaxis = list(tickmode = "array", tickvals = labeled,
                          ticktext = t_text, tickangle = 0))
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
                     name = "India · World Bank",
                     line   = list(color = india_col, width = 2.5),
                     marker = list(color = india_col, size = 5),
                     hovertemplate = "India · World Bank: %{y:.1f}<extra></extra>"
      )
    }
    
    if (!is.null(df_udise) && nrow(df_udise) > 0) {
      p <- add_trace(p,
                     data = df_udise, x = ~year, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     name = "India · UDISE+",
                     line   = list(color = COL_UDISE, width = 2, dash = "dash"),
                     marker = list(color = COL_UDISE, size = 5),
                     hovertemplate = "India · UDISE+: %{y:.1f}<extra></extra>"
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
      pf <- make_bar(plot_code, level, "Female", year, reverse, COL_FEMALE, meta$y_label)
      pm <- make_bar(plot_code, level, "Male",   year, reverse, COL_MALE,   meta$y_label)

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
          type = "scatter", mode = "lines+markers", name = "India · World Bank",
          line   = list(color = col, width = 2.5),
          marker = list(color = col, size = 5),
          hovertemplate = "India · World Bank: %{y:.1f}<extra></extra>"
        )
      }
      if (!is.null(df_udise) && nrow(df_udise) > 0) {
        p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India · UDISE+",
          line   = list(color = COL_UDISE, width = 2, dash = "dash"),
          marker = list(color = COL_UDISE, size = 5),
          hovertemplate = "India · UDISE+: %{y:.1f}<extra></extra>"
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
          type = "scatter", mode = "lines+markers", name = "India · World Bank",
          line = list(color = col, width = 2.5), marker = list(color = col, size = 5),
          hovertemplate = "India · World Bank: %{y:.1f}<extra></extra>")
      if (!is.null(df_udise) && nrow(df_udise) > 0)
        p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
          type = "scatter", mode = "lines+markers", name = "India · UDISE+",
          line = list(color = COL_UDISE, width = 2, dash = "dash"),
          marker = list(color = COL_UDISE, size = 5),
          hovertemplate = "India · UDISE+: %{y:.1f}<extra></extra>")
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
        type = "scatter", mode = "lines+markers", name = "India · World Bank",
        line = list(color = COL_ROSE, width = 2.5),
        marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India · World Bank: %{y:.2f}<extra></extra>")
    if (!is.null(df_udise) && nrow(df_udise) > 0)
      p <- add_trace(p, data = df_udise, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers", name = "India · UDISE+",
        line = list(color = COL_UDISE, width = 2, dash = "dash"),
        marker = list(color = COL_UDISE, size = 5),
        hovertemplate = "India · UDISE+: %{y:.2f}<extra></extra>")

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
        type = "scatter", mode = "lines+markers", name = "India · World Bank",
        line = list(color = COL_ROSE, width = 2.5),
        marker = list(color = COL_ROSE, size = 5),
        hovertemplate = "India · World Bank: %{y:.1f}%<extra></extra>")
    if (!is.null(udise_priv) && nrow(udise_priv) > 0)
      p <- add_trace(p, data = udise_priv, x = ~year, y = ~value,
        type = "scatter", mode = "markers", name = "India (UDISE 2024-25)",
        marker = list(color = COL_UDISE, size = 8, symbol = "diamond"),
        hovertemplate = "India · UDISE+: %{y:.1f}%<extra></extra>")

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
      left_join(country_names, by = c("country_iso3" = "iso3c")) |>
      mutate(country_f = factor(country_iso3, levels = country_iso3),
             label = coalesce(country_name, country_iso3))
    n <- nrow(df)
    labeled <- unique(c(as.character(df$country_f[1]), as.character(df$country_f[n])))
    t_text  <- df$label[match(labeled, df$country_iso3)]
    plot_ly(df, x = ~country_f, y = ~value, type = "bar",
            marker = list(color = COL_STEEL),
            text = ~label,
            hovertemplate = "<b>%{text}</b>: %{y:.0f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(yaxis = list(tickformat = ".0f"),
             xaxis = list(tickmode = "array", tickvals = labeled,
                          ticktext = t_text, tickangle = 0))
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