# global.R — World > Finance & Expenditure Shiny App

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(plotly)
library(WDI)

# ── Data ───────────────────────────────────────────────────────────────────────
finance_wb    <- readRDS("data/finance_wb.rds")
oecd_eag      <- readRDS("data/oecd_eag.rds")
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
COL_GREY_LINE <- "#C8C4BE"
COL_STEEL     <- "rgba(122, 128, 60, 0.25)"
COL_BORDER    <- "#E8E4DC"

# ── Plot metadata ──────────────────────────────────────────────────────────────
PLOT_META <- list(
  EXP_GDP      = list(title="Government Education Expenditure (% of GDP)",           y_label="% of GDP",                     has_levels=FALSE, wb_levels="Total",                                                  reverse_scale=FALSE, description="Government education expenditure as % of GDP. OECD average ~5%."),
  EXP_GOVTBDG  = list(title="Government Education Expenditure (% of Total Budget)",    y_label="% of total government expenditure",  has_levels=FALSE, wb_levels="Total",                                                  reverse_scale=FALSE, description="Education as % of total government expenditure."),
  EXP_PER_STU  = list(title="Expenditure per Student (% of GDP per Capita)",   y_label="% of GDP per capita",          has_levels=TRUE,  wb_levels=c("Primary","Secondary","Tertiary"),                      reverse_scale=FALSE, description="Annual government expenditure per student, as % of GDP per capita."),
  EXP_SHARE    = list(title="Share of Education Budget by Level",               y_label="% of education budget",        has_levels=TRUE,  wb_levels=c("Primary","Secondary","Tertiary"),                      reverse_scale=FALSE, description="Share of the government education budget allocated to a given level."),
  OECD_EXP_STUD= list(title="Annual Expenditure per Student — OECD (USD PPP)", y_label="USD PPP per student",          has_levels=TRUE,  wb_levels=c("Primary","Lower Secondary","Upper Secondary","Tertiary"),reverse_scale=FALSE, description="Annual expenditure per student in USD PPP. India: government sector only.")
)
