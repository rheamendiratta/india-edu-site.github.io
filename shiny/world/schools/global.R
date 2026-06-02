# global.R — World > Schools Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)

# ── Data ───────────────────────────────────────────────────────────────────────
schools_wb    <- readRDS("data/schools_wb.rds")
udise_4002    <- readRDS("data/udise_4002.rds")
udise_schools <- readRDS("data/udise_schools.rds")
pisa_schools  <- readRDS("data/pisa_schools.rds")

# ── Utilities ──────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#2E3250"
COL_ROSE      <- "#C9A0A0"
COL_OFFWHITE  <- "#F0EEEA"
COL_NEARWHITE <- "#FAFAF8"
COL_UDISE     <- "#7A9E9F"
COL_GREY_LINE <- "#D0CEC9"
COL_STEEL     <- "#b8c4d4"
COL_BORDER    <- "#E8E6E2"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  SCHED_DUR = list(
    title         = "Education System Duration",
    y_label       = "Duration (years)",
    wb_levels     = c("Pre-primary", "Primary", "Secondary"),
    reverse_scale = FALSE
  ),
  COMP_DUR = list(
    title         = "Compulsory Schooling Duration",
    y_label       = "Duration (years)",
    wb_levels     = c("Compulsory"),
    reverse_scale = FALSE
  ),
  PRIV = list(
    title         = "Private Enrolment Share",
    y_label       = "Private enrolment (%)",
    wb_levels     = c("Primary", "Secondary"),
    reverse_scale = FALSE
  ),
  SCH_STR = list(
    title         = "Student-Teacher Ratio (PISA School Questionnaire)",
    y_label       = "Students per teacher",
    reverse_scale = TRUE
  ),
  SCH_PCT_PUBLIC = list(
    title         = "% Public Schools (PISA)",
    y_label       = "% public schools",
    reverse_scale = FALSE
  ),
  SCH_FUND_GOV = list(
    title         = "Government Funding Share (PISA)",
    y_label       = "% government funded",
    reverse_scale = FALSE
  ),
  SCH_STAFF_SHORT = list(
    title         = "Staff Shortage Index (PISA)",
    y_label       = "Staff shortage index",
    reverse_scale = TRUE
  )
)
