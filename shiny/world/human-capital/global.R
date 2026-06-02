# global.R — World > Human Capital & Development Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)

# ── Data ───────────────────────────────────────────────────────────────────────
hci_wb <- readRDS("data/hci_wb.rds")

# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#2E3250"
COL_ROSE      <- "#C9A0A0"
COL_OFFWHITE  <- "#F0EEEA"
COL_NEARWHITE <- "#FAFAF8"
COL_FEMALE    <- "#d4787a"
COL_MALE      <- "#4a80b4"
COL_GREY_LINE <- "#D0CEC9"
COL_STEEL     <- "#b8c4d4"
COL_BORDER    <- "#E8E6E2"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  HCI = list(
    title         = "Human Capital Index",
    y_label       = "HCI (0–1 scale)",
    level         = "Total",
    has_gender    = TRUE,
    reverse_scale = FALSE
  ),
  HTS = list(
    title         = "Harmonized Test Scores",
    y_label       = "Score (300–625 scale)",
    level         = "Primary",
    has_gender    = FALSE,
    reverse_scale = FALSE
  ),
  LAYS = list(
    title         = "Learning-Adjusted Years of Schooling",
    y_label       = "LAYS (years)",
    level         = "Primary",
    has_gender    = FALSE,
    reverse_scale = FALSE
  ),
  EYS = list(
    title         = "Expected Years of Schooling",
    y_label       = "Expected years",
    level         = "Primary",
    has_gender    = FALSE,
    reverse_scale = FALSE
  ),
  HDI = list(
    title         = "Human Development Index",
    y_label       = "HDI (0–1 scale)",
    level         = "Total",
    has_gender    = FALSE,
    reverse_scale = FALSE
  ),
  HCI_STNT = list(
    title         = "HCI: Under-5 Stunting Rate",
    y_label       = "Stunting rate (% of children under 5)",
    level         = "Total",
    has_gender    = FALSE,
    reverse_scale = TRUE
  ),
  HCI_AMRT = list(
    title         = "HCI: Adult Survival Rate",
    y_label       = "Probability of survival (fraction)",
    level         = "Total",
    has_gender    = FALSE,
    reverse_scale = FALSE
  )
)
