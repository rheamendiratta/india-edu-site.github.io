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
    tab         <- input$active_tab %||% "Human Capital Index"
    label_style <- "font-size: 0.78rem; font-weight: 600; color: #c9a0a0; margin-bottom: 3px;"
    desc_style  <- "font-size: 0.75rem; color: #A0A8C0; line-height: 1.5; margin-bottom: 4px;"
    link_style  <- "color: #A0A8C0; font-size: 0.72rem; display: block; margin-bottom: 8px;"

    india_latest <- function(plot_code, level, gender = "Total") {
      yr <- hci_wb |>
        filter(plot == plot_code, level == level, gender == gender,
               country_iso3 == "IND", !is.na(value)) |>
        pull(year) |> max(na.rm = TRUE)
      if (is.infinite(yr)) "No WB data for India" else paste0("Latest India WB: ", yr)
    }

    entry <- function(label, desc, url, plot_code, level) {
      tagList(
        div(style = label_style, label),
        div(style = desc_style, desc),
        tags$a("Source: WB HCI/WDI", href = url, target = "_blank", style = link_style),
        div(style = "font-size: 0.72rem; color: #A0A8C0; margin-bottom: 5px;",
            india_latest(plot_code, level))
      )
    }

    if (tab == "Human Capital Index") {
      tagList(
        entry("Human Capital Index (HCI)",
          "World Bank index (0–1) measuring human capital a child born today can expect to attain by age 18. Combines survival, schooling quality, and health. Gender split available.",
          "https://www.worldbank.org/en/publication/human-capital",
          "HCI", "Total"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        entry("Harmonized Test Scores",
          "Country-average test scores harmonised across international and regional assessments (PISA, TIMSS, PIRLS, etc.) onto a common scale of 300–625.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.HLOS",
          "HTS", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        entry("Learning-Adjusted Years of Schooling (LAYS)",
          "Expected years of school weighted by learning quality. 12 years of poor schooling may yield fewer LAYS than 8 years of strong schooling.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.LAYS",
          "LAYS", "Primary"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        entry("Expected Years of Schooling",
          "Number of years a school-age child can expect to spend in education. Input to the LAYS and HDI education component.",
          "https://databank.worldbank.org/metadataglossary/human-capital-index/series/HD.HCI.EYRS",
          "EYS", "Primary")
      )
    } else {
      tagList(
        entry("Human Development Index (HDI)",
          "UNDP composite index combining life expectancy, education (expected & mean years of schooling), and GNI per capita. Scale 0–1.",
          "https://hdr.undp.org/data-center/human-development-index",
          "HDI", "Total")
      )
    }
  })

  # ── Layout helpers ────────────────────────────────────────────────────────────
  bar_layout <- function(p, y_label, x_range = NULL) {
    p |>
      layout(
        paper_bgcolor = COL_NEARWHITE,
        plot_bgcolor  = COL_NEARWHITE,
        font          = list(family = "Inter", size = 11, color = COL_INDIGO),
        margin        = list(l = 10, r = 20, t = 10, b = 50),
        xaxis = list(
          title     = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          range     = x_range,
          showgrid  = TRUE, gridcolor = COL_BORDER,
          zeroline  = TRUE, zerolinecolor = COL_BORDER,
          tickfont  = list(size = 10), tickformat = ".2f"
        ),
        yaxis = list(showticklabels = FALSE, showgrid = FALSE, categoryorder = "trace"),
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
                       x_range = NULL) {
    df <- hci_wb |>
      filter(plot == plot_code, level == .level, gender == .gender,
             year == .year, !is.na(value)) |>
      group_by(country_iso3) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(value) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", india_col, COL_STEEL),
        country_f = factor(country_iso3, levels = country_iso3)
      )

    if (nrow(df) == 0) {
      return(
        plotly_empty() |>
          layout(paper_bgcolor = COL_NEARWHITE, plot_bgcolor = COL_NEARWHITE,
                 annotations = list(list(
                   text = "Data not available for this year",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 13, color = "#A0A8C0", family = "Inter")
                 ))) |>
          config(displayModeBar = FALSE)
      )
    }

    n_ctry    <- nrow(df)
    tick_size <- if (n_ctry > 120) 6 else if (n_ctry > 80) 7 else 8

    plot_ly(df, x = ~value, y = ~country_f, type = "bar", orientation = "h",
            marker        = list(color = ~bar_col),
            hovertemplate = "<b>%{y}</b>: %{x:.2f}<extra></extra>") |>
      bar_layout(y_label, x_range) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        yaxis  = list(title = "", showticklabels = TRUE,
                      tickfont = list(size = tick_size, color = COL_INDIGO),
                      showgrid = FALSE, categoryorder = "trace", automargin = TRUE)
      )
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
      df_f <- hci_wb |> filter(plot == plot_code, level == level, gender == "Female",
                                year == year, !is.na(value))
      df_m <- hci_wb |> filter(plot == plot_code, level == level, gender == "Male",
                                year == year, !is.na(value))
      all_vals <- c(df_f$value, df_m$value)
      if (length(all_vals) == 0 || all(is.na(all_vals))) {
        x_range <- NULL
      } else {
        rng <- range(all_vals, na.rm = TRUE)
        pad <- diff(rng) * 0.08
        x_range <- c(rng[1] - pad, rng[2] + pad)
      }
      pf <- make_bar(plot_code, level, "Female", year, COL_FEMALE, meta$y_label, x_range)
      pm <- make_bar(plot_code, level, "Male",   year, COL_MALE,   meta$y_label, x_range)
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
