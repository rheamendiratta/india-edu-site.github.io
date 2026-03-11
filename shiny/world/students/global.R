# global.R — World > Students Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)

# ── Data ───────────────────────────────────────────────────────────────────────
students_wb    <- readRDS("data/students_wb.rds")
students_udise <- readRDS("data/students_udise.rds")

# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colour palette ─────────────────────────────────────────────────────────────
COL_INDIGO      <- "#2E3250"
COL_ROSE        <- "#C9A0A0"
COL_OFFWHITE    <- "#F0EEEA"
COL_NEARWHITE   <- "#FAFAF8"
COL_UDISE       <- "#7A9E9F"
COL_FEMALE      <- "#d4787a"   # warm coral-rose
COL_MALE        <- "#5b8fa6"   # teal-slate
COL_GREY_LINE   <- "#D0CEC9"
COL_STEEL       <- "#b8c4d4"   # other countries bars
COL_BORDER      <- "#E8E6E2"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  GER = list(
    title         = "Gross Enrolment Ratio (GER)",
    y_label       = "GER (% of school-age population)",
    has_levels    = TRUE,
    wb_levels     = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
    udise_id      = "4010",
    reverse_scale = FALSE
  ),
  NER = list(
    title         = "Net Enrolment Rate (NER)",
    y_label       = "NER (% of school-age population)",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary"),
    udise_id      = "4011",
    reverse_scale = FALSE
  ),
  REP = list(
    title         = "Repetition Rate, Primary",
    y_label       = "Repetition Rate (% of enrolled pupils)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = "4016",
    reverse_scale = TRUE
  ),
  PERS = list(
    title         = "Persistence to Last Grade of Primary",
    y_label       = "Persistence to Last Grade (% of cohort)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = "4033",
    reverse_scale = FALSE
  )
)