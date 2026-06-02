# global.R — World > Students Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(stringr)
library(WDI)

# ── Data ───────────────────────────────────────────────────────────────────────
students_wb    <- readRDS("data/students_wb.rds")
students_udise <- readRDS("data/students_udise.rds")
udise_tab1     <- readRDS("data/udise_tab1.rds")
udise_4002     <- readRDS("data/udise_4002.rds")
pisa_means     <- readRDS("data/pisa_means.rds")


# ── Country name lookup ───────────────────────────────────────────────────────
country_names <- as_tibble(WDI::WDI_data$country) |>
  select(iso3c, country_name = country) |>
  filter(!is.na(iso3c), nchar(iso3c) == 3)

# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Colour palette ─────────────────────────────────────────────────────────────
COL_INDIGO      <- "#1A1A1A"
COL_ROSE        <- "#E8AD4A"
COL_OFFWHITE    <- "#FDFAF6"
COL_NEARWHITE   <- "#FDFAF6"
COL_UDISE       <- "#7A9E9F"
COL_FEMALE      <- "#D4687A"
COL_MALE        <- "#4A5899"
COL_GREY_LINE   <- "#C8C4BE"
COL_STEEL       <- "rgba(122, 128, 60, 0.25)"
COL_BORDER      <- "#E8E4DC"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  GER        = list(title="Gross Enrolment Ratio (GER)",            y_label="Enrolment ratio (%)", has_levels=TRUE,  wb_levels=c("Pre-primary","Primary","Secondary","Tertiary"), udise_id="4010", reverse_scale=FALSE, description="Total enrolment as % of official school-age population. May exceed 100% due to over/under-age pupils."),
  NER        = list(title="Net Enrolment Rate (NER)",               y_label="Net enrolment rate (%)", has_levels=TRUE,  wb_levels=c("Primary","Secondary"), udise_id="4011", reverse_scale=FALSE, description="Enrolment of official school-age pupils only, as % of that population."),
  REP        = list(title="Repetition Rate, Primary",               y_label="Repetition Rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id="4016", reverse_scale=TRUE,  description="% of primary pupils repeating their current grade. Lower is better."),
  PERS       = list(title="Persistence to Last Grade of Primary",   y_label="Persistence (%)", has_levels=FALSE, wb_levels="Primary", udise_id="4033", reverse_scale=FALSE, description="% of Grade 1 pupils reaching the final grade of primary without dropping out."),
  NIR        = list(title="Net Intake Rate, Primary",               y_label="Net intake rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="New Grade 1 entrants of official school-entry age as % of that age group."),
  GIR        = list(title="Gross Intake Ratio, Primary",            y_label="Gross intake ratio (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="All new Grade 1 entrants regardless of age, as % of official school-entry age group."),
  GPI        = list(title="Gender Parity Index (GPI of GER)",       y_label="Gender parity index", has_levels=TRUE, wb_levels=c("Primary","Secondary","Tertiary"), udise_id="4032", reverse_scale=FALSE, description="Female-to-male enrolment ratio. Below 1 = male advantage; above 1 = female advantage."),
  OOS        = list(title="Out-of-School Rate",                     y_label="Out-of-School Rate (%)", has_levels=TRUE, wb_levels=c("Primary","Lower Secondary"), udise_id=NULL, reverse_scale=TRUE, description="% of school-age children not enrolled. Lower is better."),
  PRIV       = list(title="Private School Enrolment Share",         y_label="Private enrolment (%)", has_levels=TRUE, wb_levels=c("Primary","Secondary"), udise_id=NULL, reverse_scale=FALSE, description="% of students enrolled in private institutions."),
  OVERAGE    = list(title="Over-Age Enrolment, Primary",            y_label="Over-Age Enrolment (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of primary pupils more than 2 years above official age for their grade."),
  COMPL_PRM  = list(title="Primary Completion Rate",                y_label="Completion Rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of children completing the final grade of primary education."),
  COMPL_LSEC = list(title="Lower Secondary Completion Rate",        y_label="Completion Rate (%)", has_levels=FALSE, wb_levels="Lower Secondary", udise_id=NULL, reverse_scale=FALSE, description="% completing lower secondary (approx. Grade 9)."),
  PRS5       = list(title="Persistence to Grade 5",                 y_label="Persistence (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of Grade 1 pupils who survive to Grade 5 without dropping out."),
  PROG       = list(title="Progression to Secondary",               y_label="Progression Rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of primary completers who enrol in Grade 6 of secondary."),
  HTS        = list(title="Harmonized Test Scores",                 y_label="Score (300–625)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="Harmonised test score combining international assessments onto a common 300–625 scale."),
  LAYS       = list(title="Learning-Adjusted Years of Schooling",   y_label="Learning-adjusted years", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="Expected years of school discounted by learning quality."),
  EYS        = list(title="Expected Years of Schooling",            y_label="Expected years", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="Years of schooling a school-age child can expect to receive."),
  LP         = list(title="Learning Poverty, Primary",              y_label="Learning Poverty (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=TRUE, description="% of children who cannot read a simple text by age 10. Lower is better."),
  LD         = list(title="Learning Deprivation, Primary",          y_label="Learning Deprivation (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=TRUE, description="% of in-school primary children below minimum reading proficiency. Lower is better."),
  SD         = list(title="School Deprivation, Primary",            y_label="School Deprivation (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=TRUE, description="% of primary-age children who are out of school. Lower is better."),
  ATTAIN_PRM = list(title="Primary Attainment, Adults 25+",         y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with at least primary education completed."),
  ATTAIN_LSEC= list(title="Lower Secondary Attainment, Adults 25+", y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with at least lower secondary education."),
  ATTAIN_USEC= list(title="Upper Secondary Attainment, Adults 25+", y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with at least upper secondary education."),
  ATTAIN_TER = list(title="Tertiary Attainment (Bachelor's+), Adults 25+", y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with at least a Bachelor's or equivalent."),
  LIT_ADT    = list(title="Adult Literacy Rate (15+)",              y_label="Literacy Rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 15+ who can read and write a short simple statement."),
  LIT_YTH    = list(title="Youth Literacy Rate (15–24)",            y_label="Literacy Rate (%)", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of people aged 15–24 who can read and write."),
  LIT_YTH_GPI= list(title="Youth Literacy Gender Parity Index",     y_label="Gender parity index", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="Female-to-male ratio of youth literacy rates."),
  ENRL_NUM   = list(title="Enrolment (Number of Students)",         y_label="Students enrolled", has_levels=TRUE, wb_levels=c("Primary","Secondary","Secondary General"), udise_id=NULL, reverse_scale=FALSE, description="Total number of students enrolled at a given education level."),
  ENRL_PCT_F = list(title="Enrolment, % Female",                   y_label="Female share (%)", has_levels=TRUE, wb_levels=c("Primary","Secondary"), udise_id=NULL, reverse_scale=FALSE, description="Female students as % of total enrolment at a given level."),
  PISA_MATH  = list(title="PISA: Mean Mathematics Score",           y_label="Mean score (PISA)", has_levels=FALSE, wb_levels="Secondary", udise_id=NULL, reverse_scale=FALSE, description="Country mean PISA mathematics score. India is not in PISA."),
  PISA_READ  = list(title="PISA: Mean Reading Score",               y_label="Mean score (PISA)", has_levels=FALSE, wb_levels="Secondary", udise_id=NULL, reverse_scale=FALSE, description="Country mean PISA reading score. India is not in PISA."),
  PISA_SCI   = list(title="PISA: Mean Science Score",               y_label="Mean score (PISA)", has_levels=FALSE, wb_levels="Secondary", udise_id=NULL, reverse_scale=FALSE, description="Country mean PISA science score. India is not in PISA."),
  ATTAIN_ST  = list(title="Short-Cycle Tertiary Attainment, Adults 25+", y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with a short-cycle tertiary qualification (e.g. associate degree)."),
  ATTAIN_MS  = list(title="Master's Attainment, Adults 25+",        y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with at least a Master's degree."),
  ATTAIN_DO  = list(title="Doctoral Attainment, Adults 25+",        y_label="% adults 25+", has_levels=FALSE, wb_levels="Primary", udise_id=NULL, reverse_scale=FALSE, description="% of adults 25+ with a doctoral degree.")
)