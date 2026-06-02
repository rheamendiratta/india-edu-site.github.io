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
udise_tab1     <- readRDS("data/udise_tab1.rds")
udise_4002     <- readRDS("data/udise_4002.rds")
pisa_means     <- readRDS("data/pisa_means.rds")


# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colour palette ─────────────────────────────────────────────────────────────
COL_INDIGO      <- "#2E3250"
COL_ROSE        <- "#C9A0A0"
COL_OFFWHITE    <- "#F0EEEA"
COL_NEARWHITE   <- "#FAFAF8"
COL_UDISE       <- "#7A9E9F"
COL_FEMALE      <- "#d4787a"   # warm coral-rose
COL_MALE        <- "#4a80b4"   # teal-slate
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
  ),
  NIR = list(
    title         = "Net Intake Rate, Primary",
    y_label       = "NIR (% of official school-entry age population)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  GIR = list(
    title         = "Gross Intake Ratio, Primary",
    y_label       = "GIR (% of official school-entry age population)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  GPI = list(
    title         = "Gender Parity Index (GPI of GER)",
    y_label       = "GPI (Female/Male ratio)",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary", "Tertiary"),
    udise_id      = "4032",
    reverse_scale = FALSE
  ),
  OOS = list(
    title         = "Out-of-School Rate",
    y_label       = "Out-of-School Rate (%)",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Lower Secondary"),
    udise_id      = NULL,
    reverse_scale = TRUE
  ),
  PRIV = list(
    title         = "Private School Enrolment Share",
    y_label       = "Students in Private Institutions (%)",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary"),
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  OVERAGE = list(
    title         = "Over-Age Enrolment, Primary",
    y_label       = "Over-Age Enrolment (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 2 additions ───────────────────────────────────────────────────────────
  COMPL_PRM = list(
    title         = "Primary Completion Rate",
    y_label       = "Completion Rate (% of relevant age group)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  COMPL_LSEC = list(
    title         = "Lower Secondary Completion Rate",
    y_label       = "Completion Rate (% of relevant age group)",
    has_levels    = FALSE,
    wb_levels     = "Lower Secondary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  PRS5 = list(
    title         = "Persistence to Grade 5",
    y_label       = "Persistence (% of cohort reaching Grade 5)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  PROG = list(
    title         = "Progression to Secondary",
    y_label       = "Progression Rate (% of Grade 6 entrants from primary)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 3 additions ───────────────────────────────────────────────────────────
  HTS = list(
    title         = "Harmonized Test Scores",
    y_label       = "Score (300–625 scale)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  LAYS = list(
    title         = "Learning-Adjusted Years of Schooling",
    y_label       = "LAYS (years)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  EYS = list(
    title         = "Expected Years of Schooling",
    y_label       = "Expected years",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  LP = list(
    title         = "Learning Poverty, Primary",
    y_label       = "Learning Poverty Rate (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = TRUE
  ),
  LD = list(
    title         = "Learning Deprivation, Primary",
    y_label       = "Learning Deprivation Rate (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = TRUE
  ),
  SD = list(
    title         = "School Deprivation, Primary",
    y_label       = "School Deprivation Rate (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = TRUE
  ),
  # ── Tab 4 additions ───────────────────────────────────────────────────────────
  ATTAIN_PRM = list(
    title         = "Primary Attainment, Adults 25+",
    y_label       = "% of adults (25+) with at least primary education",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ATTAIN_LSEC = list(
    title         = "Lower Secondary Attainment, Adults 25+",
    y_label       = "% of adults (25+) with at least lower secondary",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ATTAIN_USEC = list(
    title         = "Upper Secondary Attainment, Adults 25+",
    y_label       = "% of adults (25+) with at least upper secondary",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ATTAIN_TER = list(
    title         = "Tertiary Attainment (Bachelor's+), Adults 25+",
    y_label       = "% of adults (25+) with at least a Bachelor's degree",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 5: Literacy ───────────────────────────────────────────────────────────
  LIT_ADT = list(
    title         = "Adult Literacy Rate (15+)",
    y_label       = "Literacy Rate (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  LIT_YTH = list(
    title         = "Youth Literacy Rate (15–24)",
    y_label       = "Literacy Rate (%)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  LIT_YTH_GPI = list(
    title         = "Youth Literacy Gender Parity Index",
    y_label       = "GPI (Female/Male ratio)",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 1 additions ───────────────────────────────────────────────────────────
  ENRL_NUM = list(
    title         = "Enrolment (Number of Students)",
    y_label       = "Students enrolled",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary", "Secondary General"),
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ENRL_PCT_F = list(
    title         = "Enrolment, % Female",
    y_label       = "Female share of enrolment (%)",
    has_levels    = TRUE,
    wb_levels     = c("Primary", "Secondary"),
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 3 PISA additions ─────────────────────────────────────────────────────
  PISA_MATH = list(
    title         = "PISA: Mean Mathematics Score",
    y_label       = "Mean score (PISA scale)",
    has_levels    = FALSE,
    wb_levels     = "Secondary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  PISA_READ = list(
    title         = "PISA: Mean Reading Score",
    y_label       = "Mean score (PISA scale)",
    has_levels    = FALSE,
    wb_levels     = "Secondary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  PISA_SCI = list(
    title         = "PISA: Mean Science Score",
    y_label       = "Mean score (PISA scale)",
    has_levels    = FALSE,
    wb_levels     = "Secondary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  # ── Tab 4 extended attainment ─────────────────────────────────────────────────
  ATTAIN_ST = list(
    title         = "Short-Cycle Tertiary Attainment, Adults 25+",
    y_label       = "% of adults (25+) with short-cycle tertiary",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ATTAIN_MS = list(
    title         = "Master's Attainment, Adults 25+",
    y_label       = "% of adults (25+) with at least a Master's degree",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  ),
  ATTAIN_DO = list(
    title         = "Doctoral Attainment, Adults 25+",
    y_label       = "% of adults (25+) with a doctoral degree",
    has_levels    = FALSE,
    wb_levels     = "Primary",
    udise_id      = NULL,
    reverse_scale = FALSE
  )
)