library(tidyverse)
library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(sf)
library(jsonlite)

# Data 

map_data  <- readRDS("data/map.rds")
coverage  <- readRDS("data/map_coverage.rds")
world_geo <- readRDS("data/geo/world_geo.rds")
registry  <- readRDS("data/registry.rds")

# Indicator metadata 

map_ids <- c(24, 168, 107, 101, 57, 18, 16, 195, 96, 15)

indicator_meta <- registry |>
  filter(id %in% map_ids) |>
  select(id, source_code, label)

# Named vector for dropdown: label → source_code
indicator_choices <- setNames(indicator_meta$source_code, indicator_meta$label)

indicator_descriptions <- c(
  "WB_WDI_HD_HCI_OVRL"       = "Measures the amount of human capital a child born today can expect to attain by age 18, given the risks of poor health and poor education in their country.",
  "WB_HCI_LAYS"              = "The number of years of school a child can expect to attend, adjusted downward for the quality of learning. A child may be in school for 10 years but only learning the equivalent of 7.",
  "WB_WDI_SE_LPV_PRIM"       = "The share of children who cannot read and understand a simple text by age 10. Combines children who are out of school with those who are in school but not learning.",
  "WB_HCI_TEST"              = "A cross-country comparable measure of learning outcomes, combining results from major international and regional student achievement tests.",
  "WB_WDI_SE_XPD_TOTL_GD_ZS" = "How much a government spends on education — from primary through tertiary — as a share of the country's total economic output.",
  "WB_WDI_SE_PRM_NENR"       = "The share of primary school-age children who are actually enrolled in primary school. Unlike gross enrolment, this excludes over-age and under-age students.",
  "WB_WDI_SE_ADT_LITR_ZS"    = "The percentage of people aged 15 and above who can read and write a short statement about their everyday life.",
  "WB_WDI_SE_PRM_CMPT_ZS"    = "The percentage of children who reach and complete the final grade of primary school.",
  "WB_WDI_SE_PRM_UNER_ZS"    = "The share of primary school-age children who are not enrolled in any school.",
  "WB_SSGD_HDI_INDEX"        = "A summary measure of a country's average achievement across three dimensions: a long and healthy life, access to knowledge, and a decent standard of living."
)

# Default values 

DEFAULT_INDICATOR <- indicator_choices[[1]]  # first indicator
DEFAULT_YEAR      <- map_data |>
  filter(source_code == DEFAULT_INDICATOR) |>
  pull(year) |>
  max()
