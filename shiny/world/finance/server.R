server <- function(input, output, session) {

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
    tab        <- input$active_tab %||% "Overall Spending"
    label_style <- "font-size: 0.78rem; font-weight: 600; color: #c9a0a0; margin-bottom: 3px;"
    desc_style  <- "font-size: 0.75rem; color: #A0A8C0; line-height: 1.5; margin-bottom: 4px;"
    link_style  <- "color: #A0A8C0; font-size: 0.72rem; display: block; margin-bottom: 8px;"

    india_latest <- function(plot_code, level) {
      yr <- finance_wb |>
        filter(plot == plot_code, level == level, country_iso3 == "IND", !is.na(value)) |>
        pull(year) |> max(na.rm = TRUE)
      if (is.infinite(yr)) "No WB data for India" else paste0("Latest India WB: ", yr)
    }

    entry <- function(label, desc, url, plot_code, level) {
      tagList(
        div(style = label_style, label),
        div(style = desc_style, desc),
        tags$a("Source: WDI", href = url, target = "_blank", style = link_style),
        div(style = "font-size: 0.72rem; color: #A0A8C0; margin-bottom: 5px;",
            india_latest(plot_code, level))
      )
    }

    if (tab == "Overall Spending") {
      tagList(
        entry("Govt Expenditure on Education (% of GDP)",
          "Government expenditure on education (current, capital, and transfers) as % of GDP. Reflects fiscal commitment; OECD average ~5%.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.XPD.TOTL.GD.ZS",
          "EXP_GDP", "Total"),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        entry("Govt Expenditure on Education (% of Total Govt Spending)",
          "Education as % of total government expenditure. Captures education's relative priority within the national budget.",
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.XPD.TOTL.GB.ZS",
          "EXP_GOVTBDG", "Total")
      )
    } else {
      lev_stu   <- input$per_stu_level   %||% "Primary"
      lev_share <- input$share_level     %||% "Primary"
      tagList(
        entry(
          paste0("Expenditure per Student, ", lev_stu, " (% of GDP per Capita)"),
          paste0("Annual govt expenditure per ", tolower(lev_stu),
                 " student as % of GDP per capita. Adjusts for income differences across countries."),
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.XPD.PRIM.PC.ZS",
          "EXP_PER_STU", lev_stu),
        tags$hr(style = "border-color: #3D4268; margin: 12px 0;"),
        entry(
          paste0("Education Budget Share — ", lev_share),
          paste0("Government ", tolower(lev_share),
                 " education expenditure as % of total government education expenditure."),
          "https://databank.worldbank.org/metadataglossary/world-development-indicators/series/SE.XPD.PRIM.ZS",
          "EXP_SHARE", lev_share)
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
          title        = list(text = y_label, font = list(size = 11, color = COL_INDIGO)),
          range        = x_range,
          showgrid     = TRUE, gridcolor = COL_BORDER,
          zeroline     = TRUE, zerolinecolor = COL_BORDER,
          tickfont     = list(size = 10), tickformat = ".1f"
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
      filter(plot == plot_code, level == .level, year == .year, !is.na(value)) |>
      group_by(country_iso3) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(value) |>
      mutate(
        bar_col   = if_else(country_iso3 == "IND", COL_ROSE, COL_STEEL),
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
            hovertemplate = "<b>%{y}</b>: %{x:.1f}<extra></extra>") |>
      bar_layout(y_label) |>
      layout(
        margin = list(l = 42, r = 20, t = 10, b = 50),
        yaxis  = list(title = "", showticklabels = TRUE,
                      tickfont = list(size = tick_size, color = COL_INDIGO),
                      showgrid = FALSE, categoryorder = "trace", automargin = TRUE)
      )
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
          font = list(size = 9, color = "#A0A8C0"), xanchor = "right"))
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
