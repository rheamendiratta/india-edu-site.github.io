server <- function(input, output, session) {
  
  # ── Reactive: selected indicator metadata ───────────────────────────────────
  
  selected_meta <- reactive({
    indicator_meta |> filter(source_code == input$indicator)
  })
  
  # ── Reactive: filter map data ───────────────────────────────────────────────
  
  map_filtered <- reactive({
    map_data |>
      filter(source_code == input$indicator, year == input$year)
  })
  
  # ── Update year slider when indicator changes ───────────────────────────────
  
  observeEvent(input$indicator, {
    years_available <- map_data |>
      filter(source_code == input$indicator) |>
      pull(year) |>
      unique() |>
      sort()
    
    updateSliderInput(
      session, "year",
      min   = min(years_available),
      max   = max(years_available),
      value = max(years_available)
    )
  })
  
  # ── India callout ───────────────────────────────────────────────────────────
  
  output$india_callout <- renderUI({
    india_val <- map_filtered() |>
      filter(country_iso3 == "IND") |>
      pull(value)
    
    if (length(india_val) == 0 || is.na(india_val)) {
      div(
        class = "india-callout",
        div("India", style = "font-weight:600; margin-bottom:4px;"),
        div(
          style = "color:#c9a0a0; font-size:0.8rem;",
          "No data for ", input$year
        )
      )
    } else {
      div(
        class = "india-callout",
        div("India", style = "font-weight:600; margin-bottom:4px;"),
        div(class = "value", round(india_val, 2)),
        div(
          style = "color:#a0a8c0; font-size:0.75rem; margin-top:2px;",
          input$year, " · ", selected_meta()$label
        )
      )
    }
  })
  
  # ── Choropleth ──────────────────────────────────────────────────────────────
  
  output$choropleth <- renderPlotly({
    df    <- map_filtered()
    label <- selected_meta()$label
    
    geo_joined <- world_geo |>
      left_join(df, by = c("ISO3166.1.Alpha.3" = "country_iso3")) |>
      mutate(
        is_india   = ISO3166.1.Alpha.3 == "IND",
        hover_text = paste0(
          "<b>", name, "</b><br>",
          label, ": ",
          ifelse(is.na(value), "No data", paste0(round(value, 2), " (", ISO3166.1.Alpha.3, ")"))
        )
      )
    
    plot_ly(
      data       = geo_joined,
      split      = ~name,
      color      = ~value,
      colors     = colorRamp(c("#f0eeea", "#c9a0a0", "#2e3250")),
      stroke     = I("white"),
      span       = I(0.5),
      text       = ~hover_text,
      hoverinfo  = "text",
      showlegend = FALSE
    ) |>
      colorbar(
        title      = label,
        tickfont   = list(family = "Inter", color = "#2e3250"),
        titlefont  = list(family = "Inter", color = "#2e3250")
      ) |>
      layout(
        margin     = list(l = 0, r = 0, t = 10, b = 0),
        paper_bgcolor = "#fafaf8",
        plot_bgcolor  = "#fafaf8",
        geo        = list(
          showframe      = FALSE,
          showcoastlines = FALSE,
          projection     = list(type = "natural earth"),
          bgcolor        = "#fafaf8"
        ),
        font = list(family = "Inter")
      )
  })
  
  # ── Coverage bar chart ──────────────────────────────────────────────────────
  
  output$coverage_chart <- renderPlotly({
    df <- coverage |>
      filter(source_code == input$indicator) |>
      arrange(year) |>
      mutate(
        bar_color  = ifelse(india_has_data, "#c9a0a0", "#d8d5d0"),
        hover_text = paste0(
          "<b>", year, "</b><br>",
          n_countries, " countries with data",
          ifelse(india_has_data, "<br><b style='color:#c9a0a0'>India: available</b>", "")
        )
      )
    
    plot_ly(df,
            x           = ~n_countries,
            y           = ~as.factor(year),
            type        = "bar",
            orientation = "h",
            marker      = list(
              color = ~bar_color,
              line  = list(color = "rgba(0,0,0,0)", width = 0)
            ),
            text        = ~hover_text,
            hoverinfo   = "text"
    ) |>
      layout(
        xaxis = list(
          title    = "Countries with data",
          color    = "#2e3250",
          tickfont = list(family = "Inter", size = 10)
        ),
        yaxis = list(
          title      = "",
          autorange  = "reversed",
          tickfont   = list(family = "Inter", size = 10),
          color      = "#2e3250"
        ),
        showlegend    = FALSE,
        margin        = list(l = 10, r = 10, t = 30, b = 40),
        plot_bgcolor  = "#fafaf8",
        paper_bgcolor = "#fafaf8",
        font          = list(family = "Inter", color = "#2e3250"),
        title         = list(
          text = "Data coverage by year",
          font = list(family = "Inter", size = 12, color = "#2e3250"),
          x    = 0
        )
      )
  })
}