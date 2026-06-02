# global.R — World > Teachers Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)

# ── Data ───────────────────────────────────────────────────────────────────────
teachers_wb <- readRDS("data/teachers_wb.rds")
udise_ptr   <- readRDS("data/udise_ptr.rds")
oecd_eag    <- readRDS("data/oecd_eag.rds")

# ── Utilities ──────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#2E3250"
COL_ROSE      <- "#C9A0A0"
COL_OFFWHITE  <- "#F0EEEA"
COL_NEARWHITE <- "#FAFAF8"
COL_UDISE     <- "#7A9E9F"
COL_FEMALE    <- "#d4787a"
COL_MALE      <- "#4a80b4"
COL_GREY_LINE <- "#D0CEC9"
COL_STEEL     <- "#b8c4d4"
COL_BORDER    <- "#E8E6E2"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  TCHR_NUM = list(
    title         = "Teachers (Number)",
    y_label       = "Number of teachers",
    wb_levels     = c("Primary", "Secondary"),
    reverse_scale = FALSE,
    has_gender    = FALSE
  ),
  TCHR_FEM = list(
    title         = "Teachers, % Female",
    y_label       = "Female share of teaching workforce (%)",
    wb_levels     = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
    reverse_scale = FALSE,
    has_gender    = FALSE   # the metric IS the female %
  ),
  TCHR_TRN = list(
    title         = "Trained Teachers (%)",
    y_label       = "% of teachers who are trained / qualified",
    wb_levels     = c("Pre-primary", "Primary", "Secondary"),
    reverse_scale = FALSE,
    has_gender    = TRUE
  ),
  TCHR_PTR = list(
    title         = "Pupil-Teacher Ratio",
    y_label       = "Pupils per teacher",
    wb_levels     = c("Pre-primary", "Primary", "Secondary", "Tertiary"),
    reverse_scale = TRUE,
    has_gender    = FALSE
  )
)
