# global.R — World > Teachers Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)
library(WDI)

# ── Data ───────────────────────────────────────────────────────────────────────
teachers_wb <- readRDS("data/teachers_wb.rds")
udise_ptr   <- readRDS("data/udise_ptr.rds")
oecd_eag    <- readRDS("data/oecd_eag.rds")

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
COL_FEMALE    <- "#D4687A"
COL_MALE      <- "#4A5899"
COL_GREY_LINE <- "#C8C4BE"
COL_STEEL     <- "rgba(122, 128, 60, 0.25)"
COL_BORDER    <- "#E8E4DC"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  TCHR_NUM = list(title="Teachers (Number)",   y_label="Number of teachers",           wb_levels=c("Primary","Secondary"),                              reverse_scale=FALSE, has_gender=FALSE, description="Total headcount of teachers at a given education level."),
  TCHR_FEM = list(title="Teachers, % Female",  y_label="Female share of teachers (%)", wb_levels=c("Pre-primary","Primary","Secondary","Tertiary"),      reverse_scale=FALSE, has_gender=FALSE, description="Female share of the teaching workforce at each education level."),
  TCHR_TRN = list(title="Trained Teachers (%)",y_label="Trained teachers (%)",         wb_levels=c("Pre-primary","Primary","Secondary"),                 reverse_scale=FALSE, has_gender=TRUE,  description="% of teachers who have received the minimum required teacher training."),
  TCHR_PTR = list(title="Pupil-Teacher Ratio", y_label="Pupils per teacher",           wb_levels=c("Pre-primary","Primary","Secondary","Tertiary"),      reverse_scale=TRUE,  has_gender=FALSE, description="Average pupils per teacher. Lower values indicate better staffing.")
)
