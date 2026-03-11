server <- function(input, output, session) {
  
  # Reactive
  
  selected_meta <- reactive({
    indicator_meta |> filter(source_code == input$indicator)
  })
  
  map_filtered <- reactive({
    map_data |> filter(source_code == input$indicator, year == input$year)
  })
  
  # update year slider 
  
  observe({
    years_available <- map_data |>
      filter(source_code == input$indicator) |>
      pull(year) |> unique() |> sort()
    
    updateSliderInput(session, "year",
                      min = min(years_available),
                      max = max(years_available),
                      value = max(years_available))
  }) |> bindEvent(input$indicator, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # map title
  
  output$map_title <- renderText({
    paste0("Global trends in ", selected_meta()$label)
  })
  
  # India callout
  
  output$india_callout <- renderUI({
    india_val <- map_filtered() |> filter(country_iso3 == "IND") |> pull(value)
    if (length(india_val) == 0 || is.na(india_val)) {
      div(class = "india-callout",
          div("India", style = "font-weight:600; margin-bottom:4px;"),
          div(style = "color:#c9a0a0; font-size:0.8rem;", "No data for ", input$year))
    } else {
      div(class = "india-callout",
          div("India", style = "font-weight:600; margin-bottom:4px;"),
          div(class = "value", round(india_val, 2)),
          div(style = "color:#a0a8c0; font-size:0.75rem; margin-top:2px;",
              input$year, " · ", selected_meta()$label))
    }
  })
  
  # indicator description
  
  output$indicator_description <- renderUI({
    desc <- indicator_descriptions[[input$indicator]]
    if (!is.null(desc)) {
      div(style = "color:#a0a8c0; font-size:0.78rem; line-height:1.5; margin-top:4px;", desc)
    }
  })
  
  # base leaflet
  
  output$choropleth <- renderLeaflet({
    leaflet(world_geo,
            options = leafletOptions(
              zoomControl    = TRUE,
              scrollWheelZoom = FALSE,
              worldCopyJump  = FALSE
            )
    ) |>
      setView(lng = 20, lat = 20, zoom = 2) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels)
  })
  
  # update only colours when indicator/year changes 
  
  observe({
    df          <- map_filtered()
    label       <- selected_meta()$label
    req(nrow(df) > 0)
    
    val_range <- range(df$value, na.rm = TRUE)
    req(is.finite(val_range[1]), is.finite(val_range[2]))
    
    # Join data to geo
    geo_data <- world_geo |>
      left_join(df, by = c("ISO3166.1.Alpha.3" = "country_iso3"))
    
    pal <- colorNumeric(
      palette  = colorRampPalette(c("#f0eeea", "#c9a0a0", "#2e3250"))(100),
      domain   = val_range,
      na.color = "#1a1a1a"
    )
    
    # Highlight India
    india_color <- "#c9a0a0"
    
    labels <- paste0(
      "<b>", geo_data$name, "</b><br>",
      label, ": ",
      ifelse(is.na(geo_data$value), "No data", round(geo_data$value, 2))
    ) |> lapply(htmltools::HTML)
    
    # Update only the polygon colours — not the whole map
    leafletProxy("choropleth", data = geo_data) |>
      clearControls() |>
      clearShapes() |>
      addPolygons(
        fillColor   = ~pal(value),
        fillOpacity = 0.85,
        color       = "white",
        weight      = 0.5,
        opacity     = 1,
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-family" = "Inter", "font-size" = "12px"),
          direction = "auto"
        ),
        highlight = highlightOptions(
          weight      = 2,
          color       = "#2e3250",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position  = "bottomright",
        pal       = pal,
        values    = ~value[!is.na(value)],
        title     = label,
        opacity   = 0.85,
        layerId   = "legend",
        labFormat = labelFormat(digits = 2)
      ) |>
      addLegend(
        position  = "bottomright",
        colors    = "#1a1a1a",
        labels    = "No data",
        opacity   = 0.85,
        title     = NULL,
        layerId   = "legend_na"
      )
  })
  
  # coverage bar chart 
  
  output$coverage_chart <- renderPlotly({
    df <- coverage |>
      filter(source_code == input$indicator) |>
      arrange(year) |>
      mutate(bar_color = ifelse(india_has_data, "#c9a0a0", "#d8d5d0"))
    
    plot_ly(df,
            x           = ~n_countries,
            y           = ~as.factor(year),
            type        = "bar",
            orientation = "h",
            marker      = list(color = ~bar_color, line = list(color = "rgba(0,0,0,0)", width = 0)),
            hoverinfo   = "none"
    ) |>
      layout(
        xaxis = list(title = "Countries with data", color = "#2e3250",
                     tickfont = list(family = "Inter", size = 10)),
        yaxis = list(title = "", autorange = "reversed",
                     tickfont = list(family = "Inter", size = 10), color = "#2e3250"),
        showlegend    = FALSE,
        margin        = list(l = 10, r = 10, t = 40, b = 40),
        plot_bgcolor  = "#fafaf8",
        paper_bgcolor = "#fafaf8",
        font          = list(family = "Inter", color = "#2e3250"),
        title = list(
          text = paste0(
            "Data coverage by year",
            "  <span style='font-size:11px; color:#c9a0a0'>■ India has data</span>",
            "  <span style='font-size:11px; color:#d8d5d0'>■ No India data</span>"
          ),
          font = list(family = "Inter", size = 12, color = "#2e3250"), x = 0)
      )
  })
}