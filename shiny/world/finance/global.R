# global.R — World > Finance & Expenditure Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)

# ── Data ───────────────────────────────────────────────────────────────────────
finance_wb <- readRDS("data/finance_wb.rds")
oecd_eag   <- readRDS("data/oecd_eag.rds")

# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#2E3250"
COL_ROSE      <- "#C9A0A0"
COL_OFFWHITE  <- "#F0EEEA"
COL_NEARWHITE <- "#FAFAF8"
COL_GREY_LINE <- "#D0CEC9"
COL_STEEL     <- "#b8c4d4"
COL_BORDER    <- "#E8E6E2"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  EXP_GDP = list(
    title         = "Govt Education Expenditure (% of GDP)",
    y_label       = "% of GDP",
    has_levels    = FALSE,
    wb_levels     = "Total",
    reverse_scale = FALSE
  ),
  EXP_GOVTBDG = list(
    title         = "Govt Education Expenditure (% of Total Govt Spending)",
    y_label       = "% of total government expenditure",
    has_levels    = FALSE,
    wb_levels     = "Total",
    reverse_scale = FALSE
  ),
  EXP_PER_STU = list(
    title         = "Expenditure per Student (% of GDP per Capita)",
    y_label       = "% of GDP per capita",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary", "Tertiary"),
    reverse_scale = FALSE
  ),
  EXP_SHARE = list(
    title         = "Share of Education Budget by Level",
    y_label       = "% of government education budget",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary", "Tertiary"),
    reverse_scale = FALSE
  ),
  OECD_EXP_STUD = list(
    title         = "Annual Expenditure per Student (USD PPP)",
    y_label       = "USD PPP per student",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Lower Secondary", "Upper Secondary", "Tertiary"),
    reverse_scale = FALSE
  )
)
