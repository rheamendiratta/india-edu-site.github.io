# clean_oecd_data.R
# Cleans 5 OECD EAG indicators fetched by fetch_oecd.R.
# Input:  data/raw/oecd/{276,277,278,279,280}_*.csv
# Output: data/clean/oecd_eag.rds
#
# Output schema: source_code · plot · level · gender · country_iso3 · year · value
#
# India coverage:
#   #277 (expenditure per student): present, public-sector (S13), 2010–2021
#   #276, #278, #279, #280: India absent — OECD survey gaps for partner countries

library(dplyr)
library(readr)
library(purrr)

OECD_IN   <- "data/raw/oecd"
CLEAN_OUT <- "data/clean"

# OECD aggregate / regional codes to drop (not sovereign countries)
OECD_AGGREGATES <- c(
  "OECD", "OEU", "OAVG", "G20", "G7M", "ODA",
  "EU20", "EU21", "EU22", "EU23", "EU24", "EU25", "EU26", "EU27", "EU28",
  "NMEC", "BRIICS"
)

# ISCED level → dashboard level label
ISCED_LEVEL <- c(
  ISCED11_0    = "Pre-primary",
  ISCED11_01   = "Pre-primary",
  ISCED11_02   = "Pre-primary",
  ISCED11_1    = "Primary",
  ISCED11_1_2  = "Primary",          # primary + lower secondary combined
  ISCED11_2    = "Lower Secondary",
  ISCED11_24   = "Lower Secondary",  # lower secondary to post-sec non-tertiary
  ISCED11_3    = "Upper Secondary",
  ISCED11_34   = "Upper Secondary",
  ISCED11_4    = "Upper Secondary",
  ISCED11_5T8  = "Tertiary",
  ISCED11_6    = "Tertiary",
  ISCED11_6T8  = "Tertiary",
  ISCED11A_6T8 = "Tertiary"
)

SEX_GENDER <- c("_T" = "Total", "F" = "Female", "M" = "Male")

safe_read <- function(path) {
  if (!file.exists(path)) {
    message("  Missing: ", path, " — skipping")
    return(NULL)
  }
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  # Normalise the time-period column: some flows use TIME_PERIOD, others REF_PERIOD
  if (!"TIME_PERIOD" %in% names(df) && "REF_PERIOD" %in% names(df))
    df <- dplyr::rename(df, TIME_PERIOD = REF_PERIOD)
  df
}


# ── #276  Tertiary attainment, 25–34 (DF_LSO_NEAC_ALL) ───────────────────────
# India absent. ~50 OECD+partner countries.
clean_276 <- function() {
  df <- safe_read(file.path(OECD_IN, "276_attainment_ter.csv"))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df |>
    filter(
      !is.na(OBS_VALUE),
      MEASURE             == "POP",
      UNIT_MEASURE        == "PT_POP_SEX_AGE",
      STATISTICAL_OPERATION == "OBS",
      !REF_AREA %in% OECD_AGGREGATES
    ) |>
    mutate(
      source_code  = "OECD_EAG_ATT_TER_25T34",
      plot         = "EAG_ATT_TER",
      level        = "Tertiary",
      gender       = recode(SEX, !!!SEX_GENDER, .default = NA_character_),
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = OBS_VALUE
    ) |>
    filter(!is.na(gender)) |>
    select(source_code, plot, level, gender, country_iso3, year, value)
}


# ── #277  Expenditure per student, USD PPP (DF_UOE_INDIC_FIN_PERSTUD) ────────
# India present (S13 = government sector). 50 countries.
clean_277 <- function() {
  df <- safe_read(file.path(OECD_IN, "277_exp_perstud.csv"))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df |>
    filter(
      !is.na(OBS_VALUE),
      # PRICE_BASE="Q" = current prices — the EAG standard for cross-country comparison.
      # This also ensures one row per country+level+year (Q and V both present in some data).
      PRICE_BASE == "Q",
      # Use S13 (government) for all countries — the only source available for India.
      EXP_SOURCE %in% c("S13", "_T"),
      !REF_AREA %in% OECD_AGGREGATES,
      # Cap implausible values (>100 k USD PPP per student signals a unit error in source data).
      OBS_VALUE < 1e5
    ) |>
    # For countries reporting both S13 and _T, keep _T (all sources, more complete).
    # India only has S13, so it is kept automatically.
    arrange(REF_AREA, EDUCATION_LEV, TIME_PERIOD, desc(EXP_SOURCE == "_T")) |>
    distinct(REF_AREA, EDUCATION_LEV, TIME_PERIOD, .keep_all = TRUE) |>
    mutate(
      level = recode(EDUCATION_LEV, !!!ISCED_LEVEL, .default = NA_character_)
    ) |>
    filter(!is.na(level)) |>
    mutate(
      source_code  = "OECD_EAG_FIN_PERSTUD",
      plot         = "EAG_EXP_STUD",
      gender       = "Total",
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = OBS_VALUE
    ) |>
    select(source_code, plot, level, gender, country_iso3, year, value)
}


# ── #278  Teacher salaries relative to similar workers (DF_TCH_REL) ──────────
# India absent. 34 OECD member countries.
# UNIT_MEASURE = FCTR_EARN_FT_SIM  (ratio of earnings to full-time similar workers)
clean_278 <- function() {
  df <- safe_read(file.path(OECD_IN, "278_sal_tchr.csv"))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df |>
    filter(
      !is.na(OBS_VALUE),
      UNIT_MEASURE == "FCTR_EARN_FT_SIM",
      !REF_AREA %in% OECD_AGGREGATES
    ) |>
    mutate(
      level = recode(EDUCATION_LEV, !!!ISCED_LEVEL, .default = NA_character_),
      gender = recode(SEX, !!!SEX_GENDER, .default = NA_character_)
    ) |>
    filter(!is.na(level), !is.na(gender)) |>
    mutate(
      source_code  = "OECD_EAG_SAL_ACT_REL_SIM",
      plot         = "EAG_SAL_TCH",
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = OBS_VALUE
    ) |>
    select(source_code, plot, level, gender, country_iso3, year, value)
}


# ── #279  Instruction time (hours per year) (DF_EAG_IT_ISCED) ────────────────
# India absent. 125 countries (UOE reporting).
clean_279 <- function() {
  df <- safe_read(file.path(OECD_IN, "279_instr_time.csv"))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df |>
    filter(
      !is.na(OBS_VALUE),
      !REF_AREA %in% OECD_AGGREGATES
    ) |>
    mutate(
      level = recode(EDUCATION_LEV, !!!ISCED_LEVEL, .default = NA_character_)
    ) |>
    filter(!is.na(level)) |>
    mutate(
      source_code  = "OECD_EAG_IT_ISCED",
      plot         = "EAG_INSTR_TIME",
      gender       = "Total",
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = OBS_VALUE
    ) |>
    select(source_code, plot, level, gender, country_iso3, year, value)
}


# ── #280  Student-teacher ratio, public institutions (DF_UOE_NF_PERS_STR) ────
# India absent. 45 countries.
clean_280 <- function() {
  df <- safe_read(file.path(OECD_IN, "280_str.csv"))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df |>
    filter(
      !is.na(OBS_VALUE),
      !REF_AREA %in% OECD_AGGREGATES
    ) |>
    mutate(
      level = recode(EDUCATION_LEV, !!!ISCED_LEVEL, .default = NA_character_)
    ) |>
    filter(!is.na(level)) |>
    mutate(
      source_code  = "OECD_EAG_UOE_PERS_STR",
      plot         = "EAG_STU_TCH",
      gender       = "Total",
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = OBS_VALUE
    ) |>
    select(source_code, plot, level, gender, country_iso3, year, value)
}


# ── Combine and save ──────────────────────────────────────────────────────────
message("Cleaning OECD EAG indicators...")

oecd_eag <- bind_rows(
  clean_276(),
  clean_277(),
  clean_278(),
  clean_279(),
  clean_280()
)

message("\nTotal rows: ", nrow(oecd_eag))
message("India in dataset: ", any(oecd_eag$country_iso3 == "IND"))

message("\nCoverage by plot / level / gender:")
oecd_eag |>
  group_by(plot, level, gender) |>
  summarise(
    countries = n_distinct(country_iso3),
    min_year  = min(year, na.rm = TRUE),
    max_year  = max(year, na.rm = TRUE),
    india     = any(country_iso3 == "IND"),
    .groups   = "drop"
  ) |>
  print(n = 40)

saveRDS(oecd_eag, file.path(CLEAN_OUT, "oecd_eag.rds"))
message("\nSaved: data/clean/oecd_eag.rds")
