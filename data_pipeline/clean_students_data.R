# clean_students_data.R
# Merges WB + UDISE data into app-ready RDS files for World > Students app
# Output files (copy these into shiny/world/students/data/ before deploying):
#   data/clean/students_wb.rds     — WB data, all countries, long format
#   data/clean/students_udise.rds  — UDISE data, India only, long format

library(dplyr)
library(readr)
library(arrow)
library(fs)

CLEAN_OUT <- "data/clean"
dir_create(CLEAN_OUT)

# ── Aggregate codes to exclude (WB regions, income groups etc) ─────────────────
WB_AGGREGATES <- c(
  "WLD", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF",
  "AFE", "AFW", "ARB", "CEB", "EAP", "ECA", "EMU",
  "HIC", "HPC", "IBD", "IBT", "IDA", "IDX", "LAC",
  "LDC", "LIC", "LMC", "LMY", "LTE", "MIC", "MNA",
  "OED", "OSS", "PRE", "PSS", "PST", "SSA",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC"
)

# ── Indicator metadata ─────────────────────────────────────────────────────────
# Maps source_code -> plot, level, gender for easy filtering in the app

indicator_meta <- tribble(
  ~source_code,                  ~plot,   ~level,           ~gender,
  # GER
  "WB_WDI_SE_PRE_ENRR",          "GER",   "Pre-primary",    "Total",
  "WB_WDI_SE_PRE_ENRR_FE",       "GER",   "Pre-primary",    "Female",
  "WB_WDI_SE_PRE_ENRR_MA",       "GER",   "Pre-primary",    "Male",
  "WB_WDI_SE_PRM_ENRR",          "GER",   "Primary",        "Total",
  "WB_WDI_SE_PRM_ENRR_FE",       "GER",   "Primary",        "Female",
  "WB_WDI_SE_PRM_ENRR_MA",       "GER",   "Primary",        "Male",
  "WB_WDI_SE_SEC_ENRR",          "GER",   "Secondary",      "Total",
  "WB_WDI_SE_SEC_ENRR_FE",       "GER",   "Secondary",      "Female",
  "WB_WDI_SE_SEC_ENRR_MA",       "GER",   "Secondary",      "Male",
  "WB_WDI_SE_TER_ENRR",          "GER",   "Tertiary",       "Total",
  "WB_WDI_SE_TER_ENRR_FE",       "GER",   "Tertiary",       "Female",
  "WB_WDI_SE_TER_ENRR_MA",       "GER",   "Tertiary",       "Male",
  # NER
  "WB_WDI_SE_PRM_NENR",          "NER",   "Primary",        "Total",
  "WB_WDI_SE_PRM_NENR_FE",       "NER",   "Primary",        "Female",
  "WB_WDI_SE_PRM_NENR_MA",       "NER",   "Primary",        "Male",
  "WB_WDI_SE_SEC_NENR",          "NER",   "Secondary",      "Total",
  "WB_WDI_SE_SEC_NENR_FE",       "NER",   "Secondary",      "Female",
  "WB_WDI_SE_SEC_NENR_MA",       "NER",   "Secondary",      "Male",
  # Repetition Rate
  "WB_WDI_SE_PRM_REPT_ZS",       "REP",   "Primary",        "Total",
  "WB_WDI_SE_PRM_REPT_FE_ZS",    "REP",   "Primary",        "Female",
  "WB_WDI_SE_PRM_REPT_MA_ZS",    "REP",   "Primary",        "Male",
  # Persistence
  "WB_WDI_SE_PRM_PRSL_ZS",       "PERS",  "Primary",        "Total",
  "WB_WDI_SE_PRM_PRSL_FE_ZS",    "PERS",  "Primary",        "Female",
  "WB_WDI_SE_PRM_PRSL_MA_ZS",    "PERS",  "Primary",        "Male",
  # Net Intake Rate
  "WB_WDI_SE_PRM_NINT_ZS",       "NIR",     "Primary",           "Total",
  "WB_WDI_SE_PRM_NINT_FE_ZS",    "NIR",     "Primary",           "Female",
  "WB_WDI_SE_PRM_NINT_MA_ZS",    "NIR",     "Primary",           "Male",
  # Gross Intake Ratio
  "WB_WDI_SE_PRM_GINT_ZS",       "GIR",     "Primary",           "Total",
  "WB_WDI_SE_PRM_GINT_FE_ZS",    "GIR",     "Primary",           "Female",
  "WB_WDI_SE_PRM_GINT_MA_ZS",    "GIR",     "Primary",           "Male",
  # GPI (Gender Parity Index of GER)
  "WB_WDI_SE_ENR_PRIM_FM_ZS",    "GPI",     "Primary",           "Total",
  "WB_WDI_SE_ENR_SECO_FM_ZS",    "GPI",     "Secondary",         "Total",
  "WB_WDI_SE_ENR_TERT_FM_ZS",    "GPI",     "Tertiary",          "Total",
  # Out-of-School Rate
  "WB_WDI_SE_PRM_UNER_ZS",       "OOS",     "Primary",           "Total",
  "WB_WDI_SE_PRM_UNER_FE_ZS",    "OOS",     "Primary",           "Female",
  "WB_WDI_SE_PRM_UNER_MA_ZS",    "OOS",     "Primary",           "Male",
  "WB_WDI_SE_SEC_UNER_LO_ZS",    "OOS",     "Lower Secondary",   "Total",
  "WB_WDI_SE_SEC_UNER_LO_FE_ZS", "OOS",     "Lower Secondary",   "Female",
  "WB_WDI_SE_SEC_UNER_LO_MA_ZS", "OOS",     "Lower Secondary",   "Male",
  # Private Share (no gender breakdown from WB)
  "WB_WDI_SE_PRM_PRIV_ZS",       "PRIV",    "Primary",           "Total",
  "WB_WDI_SE_SEC_PRIV_ZS",       "PRIV",    "Secondary",         "Total",
  # Over-Age Enrolment
  "WB_WDI_SE_PRM_OENR_ZS",        "OVERAGE",    "Primary",           "Total",
  "WB_WDI_SE_PRM_OENR_FE_ZS",     "OVERAGE",    "Primary",           "Female",
  "WB_WDI_SE_PRM_OENR_MA_ZS",     "OVERAGE",    "Primary",           "Male",
  # Completion Rates (Tab 2)
  "WB_WDI_SE_PRM_CMPT_ZS",        "COMPL_PRM",  "Primary",           "Total",
  "WB_WDI_SE_PRM_CMPT_FE_ZS",     "COMPL_PRM",  "Primary",           "Female",
  "WB_WDI_SE_PRM_CMPT_MA_ZS",     "COMPL_PRM",  "Primary",           "Male",
  "WB_WDI_SE_SEC_CMPT_LO_ZS",     "COMPL_LSEC", "Lower Secondary",   "Total",
  "WB_WDI_SE_SEC_CMPT_LO_FE_ZS",  "COMPL_LSEC", "Lower Secondary",   "Female",
  "WB_WDI_SE_SEC_CMPT_LO_MA_ZS",  "COMPL_LSEC", "Lower Secondary",   "Male",
  # Persistence to Grade 5 (Tab 2)
  "WB_WDI_SE_PRM_PRS5_ZS",        "PRS5",       "Primary",           "Total",
  "WB_WDI_SE_PRM_PRS5_FE_ZS",     "PRS5",       "Primary",           "Female",
  "WB_WDI_SE_PRM_PRS5_MA_ZS",     "PRS5",       "Primary",           "Male",
  # Progression to Secondary (Tab 2)
  "WB_WDI_SE_PRM_PRSC_ZS",        "PROG",  "Primary",  "Total",
  "WB_WDI_SE_PRM_PRSC_FE_ZS",     "PROG",  "Primary",  "Female",
  "WB_WDI_SE_PRM_PRSC_MA_ZS",     "PROG",  "Primary",  "Male",
  # Harmonized quality scores (Tab 3) — HCI database; total only
  "WB_HCI_TEST",                   "HTS",   "Primary",  "Total",
  "WB_HCI_LAYS",                   "LAYS",  "Primary",  "Total",
  "WB_HCI_EYRS",                   "EYS",   "Primary",  "Total",
  # Learning Poverty Framework (Tab 3) — WDI; lower = better
  "WB_WDI_SE_LPV_PRIM",           "LP",    "Primary",  "Total",
  "WB_WDI_SE_LPV_PRIM_FE",        "LP",    "Primary",  "Female",
  "WB_WDI_SE_LPV_PRIM_MA",        "LP",    "Primary",  "Male",
  "WB_WDI_SE_LPV_PRIM_LD",        "LD",    "Primary",  "Total",
  "WB_WDI_SE_LPV_PRIM_LD_FE",     "LD",    "Primary",  "Female",
  "WB_WDI_SE_LPV_PRIM_LD_MA",     "LD",    "Primary",  "Male",
  "WB_WDI_SE_LPV_PRIM_SD",        "SD",    "Primary",  "Total",
  "WB_WDI_SE_LPV_PRIM_SD_FE",     "SD",    "Primary",  "Female",
  "WB_WDI_SE_LPV_PRIM_SD_MA",      "SD",           "Primary",  "Male",
  # Educational Attainment (Tab 4)
  "WB_WDI_SE_PRM_CUAT_ZS",         "ATTAIN_PRM",  "Primary",  "Total",
  "WB_WDI_SE_PRM_CUAT_FE_ZS",      "ATTAIN_PRM",  "Primary",  "Female",
  "WB_WDI_SE_PRM_CUAT_MA_ZS",      "ATTAIN_PRM",  "Primary",  "Male",
  "WB_WDI_SE_SEC_CUAT_LO_ZS",      "ATTAIN_LSEC", "Primary",  "Total",
  "WB_WDI_SE_SEC_CUAT_LO_FE_ZS",   "ATTAIN_LSEC", "Primary",  "Female",
  "WB_WDI_SE_SEC_CUAT_LO_MA_ZS",   "ATTAIN_LSEC", "Primary",  "Male",
  "WB_WDI_SE_SEC_CUAT_UP_ZS",      "ATTAIN_USEC", "Primary",  "Total",
  "WB_WDI_SE_SEC_CUAT_UP_FE_ZS",   "ATTAIN_USEC", "Primary",  "Female",
  "WB_WDI_SE_SEC_CUAT_UP_MA_ZS",   "ATTAIN_USEC", "Primary",  "Male",
  "WB_WDI_SE_TER_CUAT_BA_ZS",      "ATTAIN_TER",  "Primary",  "Total",
  "WB_WDI_SE_TER_CUAT_BA_FE_ZS",   "ATTAIN_TER",  "Primary",  "Female",
  "WB_WDI_SE_TER_CUAT_BA_MA_ZS",   "ATTAIN_TER",   "Primary",  "Male",
  # Literacy (Tab 5)
  "WB_WDI_SE_ADT_LITR_ZS",         "LIT_ADT",     "Primary",  "Total",
  "WB_WDI_SE_ADT_LITR_FE_ZS",      "LIT_ADT",     "Primary",  "Female",
  "WB_WDI_SE_ADT_LITR_MA_ZS",      "LIT_ADT",     "Primary",  "Male",
  "WB_WDI_SE_ADT_1524_LT_ZS",      "LIT_YTH",     "Primary",  "Total",
  "WB_WDI_SE_ADT_1524_LT_FE_ZS",   "LIT_YTH",     "Primary",  "Female",
  "WB_WDI_SE_ADT_1524_LT_MA_ZS",   "LIT_YTH",     "Primary",  "Male",
  "WB_WDI_SE_ADT_1524_LT_FM_ZS",   "LIT_YTH_GPI", "Primary",  "Total",
  # Enrolment numbers (Tab 1 extra)
  "WB_WDI_SE_PRM_ENRL",            "ENRL_NUM",    "Primary",           "Total",
  "WB_WDI_SE_PRM_ENRL_FE_ZS",      "ENRL_PCT_F",  "Primary",           "Total",
  "WB_WDI_SE_SEC_ENRL",            "ENRL_NUM",    "Secondary",         "Total",
  "WB_WDI_SE_SEC_ENRL_FE_ZS",      "ENRL_PCT_F",  "Secondary",         "Total",
  "WB_WDI_SE_SEC_ENRL_GC",         "ENRL_NUM",    "Secondary General", "Total",
  # Extended tertiary attainment (Tab 4 extra)
  "WB_WDI_SE_TER_CUAT_ST_ZS",      "ATTAIN_ST",   "Primary",  "Total",
  "WB_WDI_SE_TER_CUAT_MS_ZS",      "ATTAIN_MS",   "Primary",  "Total",
  "WB_WDI_SE_TER_CUAT_MS_FE_ZS",   "ATTAIN_MS",   "Primary",  "Female",
  "WB_WDI_SE_TER_CUAT_MS_MA_ZS",   "ATTAIN_MS",   "Primary",  "Male",
  "WB_WDI_SE_TER_CUAT_DO_ZS",      "ATTAIN_DO",   "Primary",  "Total",
  "WB_WDI_SE_TER_CUAT_DO_FE_ZS",   "ATTAIN_DO",   "Primary",  "Female",
  "WB_WDI_SE_TER_CUAT_DO_MA_ZS",   "ATTAIN_DO",   "Primary",  "Male"
)

# ── UDISE level -> WB level mapping ───────────────────────────────────────────
# Used to align UDISE overlay with the selected WB level in the app
udise_to_wb_level <- c(
  "Foundational"   = NA,            # no WB equivalent
  "Preparatory"    = "Primary",
  "Elementary"     = NA,            # combined, no direct WB equivalent
  "Middle"         = NA,            # Upper Primary, no WB equivalent
  "Secondary"      = "Secondary",
  "Higher Secondary" = NA           # no WB equivalent
)

# ── Load & clean WB data ───────────────────────────────────────────────────────
message("Loading WB data...")
wb_raw <- read_csv("data/raw/wb/students_raw.csv", show_col_types = FALSE)

# Append extras (enrolment numbers, extended attainment, HCI components)
extras_path <- "data/raw/wb/extras_raw.csv"
if (file.exists(extras_path)) {
  extras_raw <- read_csv(extras_path, show_col_types = FALSE)
  wb_raw <- bind_rows(wb_raw, extras_raw)
  message("  Appended ", nrow(extras_raw), " rows from extras_raw.csv")
}

# Deduplicate on natural key before any processing.
# Guards against students_raw.csv being appended to multiple times.
n_raw <- nrow(wb_raw)
wb_raw <- wb_raw |> distinct(source_code, country_iso3, year, .keep_all = TRUE)
if (nrow(wb_raw) < n_raw)
  message("  Removed ", n_raw - nrow(wb_raw), " duplicate rows from students_raw.csv")

students_wb <- wb_raw |>
  filter(!country_iso3 %in% WB_AGGREGATES) |>
  filter(!is.na(value), !is.na(year), !is.na(country_iso3)) |>
  select(source_code, country_iso3, year, value) |>   # drop pre-existing plot/level/gender
  inner_join(indicator_meta, by = "source_code") |>
  select(
    source_code, plot, level, gender,
    country_iso3, year, value
  ) |>
  arrange(plot, level, gender, country_iso3, year)

message("WB rows: ", nrow(students_wb))
message("WB countries: ", n_distinct(students_wb$country_iso3))
message("India in WB: ", any(students_wb$country_iso3 == "IND"))

# ── Load & clean UDISE data ────────────────────────────────────────────────────
message("\nLoading UDISE data...")
udise_raw <- readRDS("data/raw/udise/udise_students.rds")

# Map UDISE indicator IDs to plot codes and add wb_level for matching
students_udise <- udise_raw |>
  mutate(
    plot = case_when(
      indicator_id == "4010" ~ "GER",
      indicator_id == "4011" ~ "NER",
      indicator_id == "4016" ~ "REP",
      indicator_id == "4033" ~ "PERS",
    ),
    # wb_level: which WB level this UDISE level corresponds to (NA = no WB match)
    wb_level = udise_to_wb_level[level]
  ) |>
  select(
    indicator_id,
    label,
    plot,
    udise_level = level,
    wb_level,
    gender,
    acad_year,
    year,
    value
  ) |>
  arrange(plot, udise_level, gender, year)

message("UDISE rows: ", nrow(students_udise))
message("UDISE years: ", min(students_udise$year), " to ", max(students_udise$year))

# ── Save ───────────────────────────────────────────────────────────────────────
saveRDS(students_wb,    file.path(CLEAN_OUT, "students_wb.rds"))
saveRDS(students_udise, file.path(CLEAN_OUT, "students_udise.rds"))

message("\n── Clean complete ──")
message("Saved: data/clean/students_wb.rds")
message("Saved: data/clean/students_udise.rds")

# ── Coverage check ─────────────────────────────────────────────────────────────
message("\nWB coverage by plot/level/gender:")
students_wb |>
  group_by(plot, level, gender) |>
  summarise(
    countries = n_distinct(country_iso3),
    min_year  = min(year),
    max_year  = max(year),
    india     = any(country_iso3 == "IND"),
    .groups   = "drop"
  ) |>
  print(n = 30)

message("\nUDISE coverage by plot/level/gender:")
students_udise |>
  group_by(plot, udise_level, wb_level, gender) |>
  summarise(
    min_year = min(year),
    max_year = max(year),
    .groups  = "drop"
  ) |>
  print(n = 40)
