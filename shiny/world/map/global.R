library(tidyverse)
library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(sf)
library(jsonlite)

# Data 

map_data     <- read_parquet("../../../data/clean/map.parquet")
coverage     <- read_parquet("../../../data/clean/map_coverage.parquet")
world_geo    <- st_read("../../../data/clean/geo/world.geojson", quiet = TRUE)
registry     <- read_json("../../../data_pipeline/indicator_registry.json",
                          simplifyVector = TRUE)

# Indicator metadata 

map_ids <- c(195, 16, 57, 18, 24, 168, 96, 107, 101, 15)

indicator_meta <- registry |>
  filter(id %in% map_ids) |>
  select(id, source_code, label)

# Named vector for dropdown: label → source_code
indicator_choices <- setNames(indicator_meta$source_code, indicator_meta$label)

# Default values 

DEFAULT_INDICATOR <- indicator_choices[[1]]  # first indicator
DEFAULT_YEAR      <- map_data |>
  filter(source_code == DEFAULT_INDICATOR) |>
  pull(year) |>
  max()
