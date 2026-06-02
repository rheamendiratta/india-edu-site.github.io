# global.R — World > Schools Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)
library(WDI)

# ── Data ───────────────────────────────────────────────────────────────────────
schools_wb    <- readRDS("data/schools_wb.rds")
udise_4002    <- readRDS("data/udise_4002.rds")
udise_schools <- readRDS("data/udise_schools.rds")
pisa_schools  <- readRDS("data/pisa_schools.rds")

# ── Country name lookup ────────────────────────────────────────────────────────
country_names <- as_tibble(WDI::WDI_data$country) |>
  select(iso3c, country_name = country) |>
  filter(!is.na(iso3c), nchar(iso3c) == 3)

# ── Utilities ──────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colours ────────────────────────────────────────────────────────────────────
COL_INDIGO    <- "#1A1A1A"
COL_ROSE      <- "#E8AD4A"
COL_OFFWHITE  <- "#FDFAF6"
COL_NEARWHITE <- "#FDFAF6"
COL_UDISE     <- "#7A9E9F"
COL_GREY_LINE <- "#C8C4BE"
COL_STEEL     <- "rgba(122, 128, 60, 0.25)"
COL_BORDER    <- "#E8E4DC"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  SCHED_DUR      = list(title="Education System Duration",           y_label="Duration (years)",        wb_levels=c("Pre-primary","Primary","Secondary"), reverse_scale=FALSE, description="Officially mandated number of years in each education cycle."),
  COMP_DUR       = list(title="Compulsory Schooling Duration",       y_label="Duration (years)",        wb_levels=c("Compulsory"),                        reverse_scale=FALSE, description="Number of years of legally compulsory schooling."),
  PRIV           = list(title="Private Enrolment Share",             y_label="Private enrolment (%)",   wb_levels=c("Primary","Secondary"),               reverse_scale=FALSE, description="% of students enrolled in private institutions."),
  SCH_STR        = list(title="Student-Teacher Ratio (PISA)",        y_label="Students per teacher",    wb_levels=NULL,                                   reverse_scale=TRUE,  description="Average students per teacher, from PISA school questionnaire."),
  SCH_PCT_PUBLIC = list(title="% Public Schools (PISA)",             y_label="% public schools",        wb_levels=NULL,                                   reverse_scale=FALSE, description="% of schools that are publicly managed (PISA)."),
  SCH_FUND_GOV   = list(title="Government Funding Share (PISA)",     y_label="% government funded",     wb_levels=NULL,                                   reverse_scale=FALSE, description="Average share of school funding from government sources (PISA)."),
  SCH_STAFF_SHORT= list(title="Staff Shortage Index (PISA)",         y_label="Staff shortage index",    wb_levels=NULL,                                   reverse_scale=TRUE,  description="Teacher/staff shortage index as perceived by school principals (PISA).")
)
