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
    india_row <- map_data |>
      filter(source_code == input$indicator, country_iso3 == "IND", !is.na(value)) |>
      slice_max(year, n = 1, with_ties = FALSE)

    sc <- input$indicator
    src_label <- if      (startsWith(sc, "WB_WDI_"))  "World Bank WDI"
                 else if (startsWith(sc, "WB_HCI_"))  "World Bank HCI"
                 else if (startsWith(sc, "WB_SSGD_")) "World Bank"
                 else                                   "World Bank"

    lbl_s <- "font-size: 0.73rem; font-weight: 600; color: #e8ad4a; margin-bottom: 1px;"
    val_s <- "font-size: 1.3rem; font-weight: 700; color: #fdfaf6; line-height: 1;"
    sub_s <- "font-size: 0.67rem; color: #999993; margin-top: 3px;"

    if (nrow(india_row) == 0) {
      div(class = "india-callout",
          div(style = lbl_s, "India"),
          div(style = sub_s, "No data available"))
    } else {
      div(class = "india-callout",
          div(style = lbl_s, "India · Latest available"),
          div(style = val_s, round(india_row$value[1], 2)),
          div(style = sub_s, paste0(src_label, " · ", india_row$year[1])))
    }
  })
  
  # indicator description
  
  output$indicator_description <- renderUI({
    desc <- indicator_descriptions[[input$indicator]]
    if (!is.null(desc)) {
      div(style = "color:#999993; font-size:0.78rem; line-height:1.5; margin-top:4px;", desc)
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
      fitBounds(-130, -42, 155, 72) |>
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
      palette  = colorRampPalette(c("#fdfaf6", "#e8ad4a", "#1a1a1a"))(100),
      domain   = val_range,
      na.color = "#1a1a1a"
    )
    
    # Highlight India
    india_color <- "#e8ad4a"
    
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
          color       = "#1a1a1a",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position  = "bottomleft",
        pal       = pal,
        values    = ~value[!is.na(value)],
        title     = paste0(strwrap(label, width = 20), collapse = "<br>"),
        opacity   = 0.85,
        layerId   = "legend",
        labFormat = labelFormat(digits = 2)
      ) |>
      addLegend(
        position  = "bottomleft",
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
      mutate(bar_color = ifelse(india_has_data, "#e8ad4a", "#c8c4be"))
    
    plot_ly(df,
            x           = ~n_countries,
            y           = ~as.factor(year),
            type        = "bar",
            orientation = "h",
            marker      = list(color = ~bar_color, line = list(color = "rgba(0,0,0,0)", width = 0)),
            text        = ~if_else(india_has_data, "India included", "No India data"),
        hovertemplate = "<b>%{y}</b>: %{x} countries · %{text}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "Countries with data", color = "#1a1a1a",
                     tickfont = list(family = "Inter", size = 10)),
        yaxis = list(title = "", autorange = "reversed",
                     tickfont = list(family = "Inter", size = 10), color = "#1a1a1a"),
        showlegend    = FALSE,
        margin        = list(l = 10, r = 10, t = 40, b = 40),
        plot_bgcolor  = "#fdfaf6",
        paper_bgcolor = "#fdfaf6",
        font          = list(family = "Inter", color = "#1a1a1a"),
        title = list(
          text = paste0(
            "Data coverage by year",
            "  <span style='font-size:11px; color:#e8ad4a'>■ India has data</span>",
            "  <span style='font-size:11px; color:#c8c4be'>■ No India data</span>"
          ),
          font = list(family = "Inter", size = 12, color = "#1a1a1a"), x = 0)
      )
  })
}