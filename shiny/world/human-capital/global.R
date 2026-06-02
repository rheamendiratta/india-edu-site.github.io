# global.R — World > Human Capital & Development Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(WDI)

# ── Data ───────────────────────────────────────────────────────────────────────
hci_wb        <- readRDS("data/hci_wb.rds")
country_names <- as_tibble(WDI::WDI_data$country) |>
  select(iso3c, country_name = country) |>
  filter(!is.na(iso3c), nchar(iso3c) == 3)

# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#1A1A1A"
COL_ROSE      <- "#E8AD4A"
COL_OFFWHITE  <- "#FDFAF6"
COL_NEARWHITE <- "#FDFAF6"
COL_FEMALE    <- "#D4687A"
COL_MALE      <- "#4A5899"
COL_GREY_LINE <- "#C8C4BE"
COL_STEEL     <- "rgba(122, 128, 60, 0.25)"
COL_BORDER    <- "#E8E4DC"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  HCI      = list(title="Human Capital Index",                  y_label="HCI (0–1)",        level="Total",   has_gender=TRUE,  reverse_scale=FALSE, description="World Bank index (0–1) measuring human capital a child born today can expect by age 18."),
  HTS      = list(title="Harmonized Test Scores",               y_label="Score (300–625)",  level="Primary", has_gender=FALSE, reverse_scale=FALSE, description="Harmonised test score combining international assessments onto a 300–625 scale."),
  LAYS     = list(title="Learning-Adjusted Years of Schooling", y_label="LAYS (years)",     level="Primary", has_gender=FALSE, reverse_scale=FALSE, description="Expected years of school discounted by learning quality."),
  EYS      = list(title="Expected Years of Schooling",          y_label="Expected years",   level="Primary", has_gender=FALSE, reverse_scale=FALSE, description="Years of schooling a school-age child can expect to receive."),
  HDI      = list(title="Human Development Index",              y_label="HDI (0–1)",        level="Total",   has_gender=FALSE, reverse_scale=FALSE, description="UNDP composite index combining health, education, and income (0–1)."),
  HCI_STNT = list(title="HCI: Under-5 Stunting Rate",          y_label="Stunting rate (%)",level="Total",   has_gender=FALSE, reverse_scale=TRUE,  description="% of children under 5 who are stunted (height-for-age < -2 SD). Lower is better."),
  HCI_AMRT = list(title="HCI: Adult Survival Rate",            y_label="Survival rate",    level="Total",   has_gender=FALSE, reverse_scale=FALSE, description="Probability of surviving from age 15 to 60.")
)
