library(WDI)
library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(fs)
library(tidyr)

RAW_OUT <- "data/raw/wb"
dir_create(RAW_OUT)

registry <- read_json("data_pipeline/indicator_registry.json", simplifyVector = TRUE)

# ── The 10 map indicators

map_indicators <- tribble(
  ~id,  ~source_code,                   ~database_id,  ~label,
  195,  "WB_WDI_HD_HCI_OVRL",           "WB_WDI",      "Human Capital Index",
  16,   "WB_HCI_LAYS",                  "WB_HCI",      "Learning-Adjusted Years of Schooling",
  57,   "WB_WDI_SE_LPV_PRIM",           "WB_WDI",      "Learning Poverty, Primary",
  18,   "WB_HCI_TEST",                  "WB_HCI",      "Harmonized Test Scores",
  24,   "WB_WDI_SE_XPD_TOTL_GD_ZS",    "WB_WDI",      "Govt Expenditure on Education % GDP",
  168,  "WB_WDI_SE_PRM_NENR",           "WB_WDI",      "Primary Net Enrolment Rate",
  96,   "WB_WDI_SE_ADT_LITR_ZS",        "WB_WDI",      "Adult Literacy Rate",
  107,  "WB_WDI_SE_PRM_CMPT_ZS",        "WB_WDI",      "Primary Completion Rate",
  101,  "WB_WDI_SE_PRM_UNER_ZS",        "WB_WDI",      "Out-of-School Rate, Primary",
  15,   "WB_SSGD_HDI_INDEX",            "WB_SSGD",     "HDI Index"
)

students_indicators <- tribble(
  ~id,  ~source_code,                        ~database_id,  ~plot,      ~label,
  # GER
  151,  "WB_WDI_SE_PRE_ENRR",                "WB_WDI",      "GER",      "GER, Pre-Primary, Total",
  148,  "WB_WDI_SE_PRE_ENRR_FE",             "WB_WDI",      "GER",      "GER, Pre-Primary, Female",
  149,  "WB_WDI_SE_PRE_ENRR_MA",             "WB_WDI",      "GER",      "GER, Pre-Primary, Male",
  89,   "WB_WDI_SE_PRM_ENRR",                "WB_WDI",      "GER",      "GER, Primary, Total",
  110,  "WB_WDI_SE_PRM_ENRR_FE",             "WB_WDI",      "GER",      "GER, Primary, Female",
  63,   "WB_WDI_SE_PRM_ENRR_MA",             "WB_WDI",      "GER",      "GER, Primary, Male",
  77,   "WB_WDI_SE_SEC_ENRR",                "WB_WDI",      "GER",      "GER, Secondary, Total",
  138,  "WB_WDI_SE_SEC_ENRR_FE",             "WB_WDI",      "GER",      "GER, Secondary, Female",
  125,  "WB_WDI_SE_SEC_ENRR_MA",             "WB_WDI",      "GER",      "GER, Secondary, Male",
  116,  "WB_WDI_SE_TER_ENRR",                "WB_WDI",      "GER",      "GER, Tertiary, Total",
  104,  "WB_WDI_SE_TER_ENRR_FE",             "WB_WDI",      "GER",      "GER, Tertiary, Female",
  126,  "WB_WDI_SE_TER_ENRR_MA",             "WB_WDI",      "GER",      "GER, Tertiary, Male",
  # NER
  168,  "WB_WDI_SE_PRM_NENR",                "WB_WDI",      "NER",      "NER, Primary, Total",
  186,  "WB_WDI_SE_PRM_NENR_FE",             "WB_WDI",      "NER",      "NER, Primary, Female",
  191,  "WB_WDI_SE_PRM_NENR_MA",             "WB_WDI",      "NER",      "NER, Primary, Male",
  152,  "WB_WDI_SE_SEC_NENR",                "WB_WDI",      "NER",      "NER, Secondary, Total",
  158,  "WB_WDI_SE_SEC_NENR_FE",             "WB_WDI",      "NER",      "NER, Secondary, Female",
  171,  "WB_WDI_SE_SEC_NENR_MA",             "WB_WDI",      "NER",      "NER, Secondary, Male",
  # Repetition Rate
  162,  "WB_WDI_SE_PRM_REPT_ZS",             "WB_WDI",      "REP",      "Repetition Rate, Primary, Total",
  167,  "WB_WDI_SE_PRM_REPT_FE_ZS",          "WB_WDI",      "REP",      "Repetition Rate, Primary, Female",
  161,  "WB_WDI_SE_PRM_REPT_MA_ZS",          "WB_WDI",      "REP",      "Repetition Rate, Primary, Male",
  # Persistence
  117,  "WB_WDI_SE_PRM_PRSL_ZS",             "WB_WDI",      "PERS",     "Persistence to Last Grade, Primary, Total",
  73,   "WB_WDI_SE_PRM_PRSL_FE_ZS",          "WB_WDI",      "PERS",     "Persistence to Last Grade, Primary, Female",
  56,   "WB_WDI_SE_PRM_PRSL_MA_ZS",          "WB_WDI",      "PERS",     "Persistence to Last Grade, Primary, Male",
  # Net Intake Rate
  173,  "WB_WDI_SE_PRM_NINT_ZS",             "WB_WDI",      "NIR",      "Net Intake Rate, Primary, Total",
  180,  "WB_WDI_SE_PRM_NINT_FE_ZS",          "WB_WDI",      "NIR",      "Net Intake Rate, Primary, Female",
  178,  "WB_WDI_SE_PRM_NINT_MA_ZS",          "WB_WDI",      "NIR",      "Net Intake Rate, Primary, Male",
  # Gross Intake Ratio
  153,  "WB_WDI_SE_PRM_GINT_ZS",             "WB_WDI",      "GIR",      "Gross Intake Ratio, Primary, Total",
  163,  "WB_WDI_SE_PRM_GINT_FE_ZS",          "WB_WDI",      "GIR",      "Gross Intake Ratio, Primary, Female",
  164,  "WB_WDI_SE_PRM_GINT_MA_ZS",          "WB_WDI",      "GIR",      "Gross Intake Ratio, Primary, Male",
  # GPI (Gender Parity Index of GER)
  147,  "WB_WDI_SE_ENR_PRIM_FM_ZS",          "WB_WDI",      "GPI",      "GPI, Primary",
  146,  "WB_WDI_SE_ENR_SECO_FM_ZS",          "WB_WDI",      "GPI",      "GPI, Secondary",
  121,  "WB_WDI_SE_ENR_TERT_FM_ZS",          "WB_WDI",      "GPI",      "GPI, Tertiary",
  # Out-of-School Rate
  101,  "WB_WDI_SE_PRM_UNER_ZS",             "WB_WDI",      "OOS",      "OOS Rate, Primary, Total",
  111,  "WB_WDI_SE_PRM_UNER_FE_ZS",          "WB_WDI",      "OOS",      "OOS Rate, Primary, Female",
  124,  "WB_WDI_SE_PRM_UNER_MA_ZS",          "WB_WDI",      "OOS",      "OOS Rate, Primary, Male",
  50,   "WB_WDI_SE_SEC_UNER_LO_ZS",          "WB_WDI",      "OOS",      "OOS Rate, Lower Secondary, Total",
  115,  "WB_WDI_SE_SEC_UNER_LO_FE_ZS",       "WB_WDI",      "OOS",      "OOS Rate, Lower Secondary, Female",
  120,  "WB_WDI_SE_SEC_UNER_LO_MA_ZS",       "WB_WDI",      "OOS",      "OOS Rate, Lower Secondary, Male",
  # Private Share
  123,  "WB_WDI_SE_PRM_PRIV_ZS",             "WB_WDI",      "PRIV",     "Private Share, Primary",
  59,   "WB_WDI_SE_SEC_PRIV_ZS",             "WB_WDI",      "PRIV",     "Private Share, Secondary",
  # Over-Age Enrolment
  169,  "WB_WDI_SE_PRM_OENR_ZS",             "WB_WDI",      "OVERAGE",   "Over-Age Enrolment, Primary, Total",
  174,  "WB_WDI_SE_PRM_OENR_FE_ZS",          "WB_WDI",      "OVERAGE",   "Over-Age Enrolment, Primary, Female",
  175,  "WB_WDI_SE_PRM_OENR_MA_ZS",          "WB_WDI",      "OVERAGE",   "Over-Age Enrolment, Primary, Male",
  # Completion Rates (Tab 2)
  107,  "WB_WDI_SE_PRM_CMPT_ZS",             "WB_WDI",      "COMPL_PRM", "Primary Completion Rate, Total",
  83,   "WB_WDI_SE_PRM_CMPT_FE_ZS",          "WB_WDI",      "COMPL_PRM", "Primary Completion Rate, Female",
  122,  "WB_WDI_SE_PRM_CMPT_MA_ZS",          "WB_WDI",      "COMPL_PRM", "Primary Completion Rate, Male",
  131,  "WB_WDI_SE_SEC_CMPT_LO_ZS",          "WB_WDI",      "COMPL_LSEC","Lower Secondary Completion Rate, Total",
  64,   "WB_WDI_SE_SEC_CMPT_LO_FE_ZS",       "WB_WDI",      "COMPL_LSEC","Lower Secondary Completion Rate, Female",
  130,  "WB_WDI_SE_SEC_CMPT_LO_MA_ZS",       "WB_WDI",      "COMPL_LSEC","Lower Secondary Completion Rate, Male",
  # Persistence to Grade 5 (Tab 2)
  21,   "WB_WDI_SE_PRM_PRS5_ZS",             "WB_WDI",      "PRS5",      "Persistence to Grade 5, Total",
  20,   "WB_WDI_SE_PRM_PRS5_FE_ZS",          "WB_WDI",      "PRS5",      "Persistence to Grade 5, Female",
  22,   "WB_WDI_SE_PRM_PRS5_MA_ZS",          "WB_WDI",      "PRS5",      "Persistence to Grade 5, Male",
  # Progression to Secondary (Tab 2)
  177,  "WB_WDI_SE_PRM_PRSC_ZS",             "WB_WDI",      "PROG",      "Progression to Secondary, Total",
  192,  "WB_WDI_SE_PRM_PRSC_FE_ZS",          "WB_WDI",      "PROG",      "Progression to Secondary, Female",
  159,  "WB_WDI_SE_PRM_PRSC_MA_ZS",          "WB_WDI",      "PROG",      "Progression to Secondary, Male",
  # Harmonized quality scores (Tab 3) — WB_HCI database; no gender breakdown
  18,   "WB_HCI_TEST",                        "WB_HCI",      "HTS",       "Harmonized Test Scores",
  16,   "WB_HCI_LAYS",                        "WB_HCI",      "LAYS",      "Learning-Adjusted Years of Schooling",
  17,   "WB_HCI_EYRS",                        "WB_HCI",      "EYS",       "Expected Years of Schooling",
  # Learning Poverty Framework (Tab 3) — WB_WDI; has gender breakdown; lower = better
  57,   "WB_WDI_SE_LPV_PRIM",                "WB_WDI",      "LP",        "Learning Poverty, Primary, Total",
  97,   "WB_WDI_SE_LPV_PRIM_FE",             "WB_WDI",      "LP",        "Learning Poverty, Primary, Female",
  99,   "WB_WDI_SE_LPV_PRIM_MA",             "WB_WDI",      "LP",        "Learning Poverty, Primary, Male",
  98,   "WB_WDI_SE_LPV_PRIM_LD",             "WB_WDI",      "LD",        "Learning Deprivation, Primary, Total",
  140,  "WB_WDI_SE_LPV_PRIM_LD_FE",          "WB_WDI",      "LD",        "Learning Deprivation, Primary, Female",
  85,   "WB_WDI_SE_LPV_PRIM_LD_MA",          "WB_WDI",      "LD",        "Learning Deprivation, Primary, Male",
  86,   "WB_WDI_SE_LPV_PRIM_SD",             "WB_WDI",      "SD",        "School Deprivation, Primary, Total",
  100,  "WB_WDI_SE_LPV_PRIM_SD_FE",          "WB_WDI",      "SD",        "School Deprivation, Primary, Female",
  141,  "WB_WDI_SE_LPV_PRIM_SD_MA",          "WB_WDI",      "SD",           "School Deprivation, Primary, Male",
  # Educational Attainment, adults 25+ (Tab 4)
  127,  "WB_WDI_SE_PRM_CUAT_ZS",             "WB_WDI",      "ATTAIN_PRM",  "Attainment, at Least Primary, Total",
  108,  "WB_WDI_SE_PRM_CUAT_FE_ZS",          "WB_WDI",      "ATTAIN_PRM",  "Attainment, at Least Primary, Female",
  109,  "WB_WDI_SE_PRM_CUAT_MA_ZS",          "WB_WDI",      "ATTAIN_PRM",  "Attainment, at Least Primary, Male",
  113,  "WB_WDI_SE_SEC_CUAT_LO_ZS",          "WB_WDI",      "ATTAIN_LSEC", "Attainment, at Least Lower Secondary, Total",
  103,  "WB_WDI_SE_SEC_CUAT_LO_FE_ZS",       "WB_WDI",      "ATTAIN_LSEC", "Attainment, at Least Lower Secondary, Female",
  71,   "WB_WDI_SE_SEC_CUAT_LO_MA_ZS",       "WB_WDI",      "ATTAIN_LSEC", "Attainment, at Least Lower Secondary, Male",
  137,  "WB_WDI_SE_SEC_CUAT_UP_ZS",          "WB_WDI",      "ATTAIN_USEC", "Attainment, at Least Upper Secondary, Total",
  55,   "WB_WDI_SE_SEC_CUAT_UP_FE_ZS",       "WB_WDI",      "ATTAIN_USEC", "Attainment, at Least Upper Secondary, Female",
  106,  "WB_WDI_SE_SEC_CUAT_UP_MA_ZS",       "WB_WDI",      "ATTAIN_USEC", "Attainment, at Least Upper Secondary, Male",
  92,   "WB_WDI_SE_TER_CUAT_BA_ZS",          "WB_WDI",      "ATTAIN_TER",  "Attainment, at Least Bachelor's, Total",
  91,   "WB_WDI_SE_TER_CUAT_BA_FE_ZS",       "WB_WDI",      "ATTAIN_TER",  "Attainment, at Least Bachelor's, Female",
  69,   "WB_WDI_SE_TER_CUAT_BA_MA_ZS",       "WB_WDI",      "ATTAIN_TER",  "Attainment, at Least Bachelor's, Male",
  # Literacy (Tab 5)
  96,   "WB_WDI_SE_ADT_LITR_ZS",            "WB_WDI",      "LIT_ADT",     "Adult Literacy Rate, Total",
  88,   "WB_WDI_SE_ADT_LITR_FE_ZS",         "WB_WDI",      "LIT_ADT",     "Adult Literacy Rate, Female",
  58,   "WB_WDI_SE_ADT_LITR_MA_ZS",         "WB_WDI",      "LIT_ADT",     "Adult Literacy Rate, Male",
  139,  "WB_WDI_SE_ADT_1524_LT_ZS",         "WB_WDI",      "LIT_YTH",     "Youth Literacy Rate, Total",
  87,   "WB_WDI_SE_ADT_1524_LT_FE_ZS",      "WB_WDI",      "LIT_YTH",     "Youth Literacy Rate, Female",
  95,   "WB_WDI_SE_ADT_1524_LT_MA_ZS",      "WB_WDI",      "LIT_YTH",     "Youth Literacy Rate, Male",
  65,   "WB_WDI_SE_ADT_1524_LT_FM_ZS",      "WB_WDI",      "LIT_YTH_GPI", "Youth Literacy Gender Parity Index"
)

# ── Fetch function

fetch_one <- function(id, source_code, database_id, label) {
  message("Fetching [", id, "]: ", label)
  
  page_size <- 1000
  all_pages <- list()
  offset    <- 0
  
  repeat {
    url <- paste0(
      "https://data360api.worldbank.org/data360/data",
      "?indicator=", source_code,
      "&DATABASE_ID=", database_id,
      "&pageSize=", page_size,
      "&offset=", offset
    )
    
    resp <- tryCatch(
      request(url) |> req_timeout(60) |> req_perform(),
      error = function(e) {
        message("  ✗ Failed at offset ", offset, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(resp)) break
    
    raw <- tryCatch(
      resp |> resp_body_json(simplifyVector = TRUE),
      error = function(e) {
        message("  ✗ Parse error: ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(raw$value) || length(raw$value) == 0) break
    
    all_pages[[length(all_pages) + 1]] <- raw$value
    
    # Check if we have all rows
    total <- raw$count
    offset <- offset + page_size
    message("  ... fetched ", min(offset, total), " of ", total)
    
    if (offset >= total) break
    Sys.sleep(0.3)
  }
  
  if (length(all_pages) == 0) {
    message("  ✗ No data returned")
    return(NULL)
  }
  
  df <- bind_rows(all_pages) |>
    as_tibble() |>
    transmute(
      indicator_id = id,
      source_code  = source_code,
      country_iso3 = REF_AREA,
      year         = as.integer(TIME_PERIOD),
      value        = as.double(OBS_VALUE)
    ) |>
    filter(!is.na(value), !is.na(year))
  
  message("  ✓ ", nrow(df), " rows")
  df
}

# ── Run 

all_data <- list()

for (i in seq_len(nrow(map_indicators))) {
  row <- map_indicators[i, ]
  df  <- fetch_one(row$id, row$source_code, row$database_id, row$label)
  if (!is.null(df)) all_data[[row$source_code]] <- df
  Sys.sleep(0.5)
}

# ── Save

combined <- bind_rows(all_data)
write_csv(combined, file.path(RAW_OUT, "map_raw.csv"))

message("\n── Complete ──")
message("Rows: ", nrow(combined))
message("Indicators fetched: ", length(all_data), " of ", nrow(map_indicators))

failed <- map_indicators$source_code[!map_indicators$source_code %in% names(all_data)]
if (length(failed) > 0) message("Failed: ", paste(failed, collapse = ", "))

map_raw <- read_csv("data/raw/wb/map_raw.csv")

map_raw |>
  group_by(source_code) |>
  summarise(
    countries = n_distinct(country_iso3),
    years     = n_distinct(year),
    rows      = n()
  )

# ── Fetch students indicators via WDI package ─────────────────────────────────
# All students indicators are WB_WDI_* — the WDI package (api.worldbank.org/v2)
# has full historical coverage for all countries.
#
# NOTE: The data360 API used above for map indicators (HCI, SSGD) returns only
# a thin subset of WDI data and must NOT be used for students indicators.
#
# WDI source codes are derived by stripping "WB_WDI_" and replacing "_" with ".":
#   WB_WDI_SE_PRM_ENRR  →  SE.PRM.ENRR

students_indicators <- students_indicators |>
  mutate(wdi_code = gsub("_", ".", sub("^WB_WDI_", "", source_code)))

message("\nFetching ", nrow(students_indicators), " students indicators via WDI package...")

# Fetch all indicators in one bulk call (~2 min; overwrites students_raw.csv fresh each run).
wdi_raw <- WDI(
  country  = "all",
  indicator = students_indicators$wdi_code,
  start    = 1970,
  end      = 2025,
  extra    = FALSE,
  language = "en"
)

message("  WDI raw rows: ", nrow(wdi_raw))

wdi_code_to_meta <- students_indicators |> select(source_code, wdi_code, plot)

students_combined <- wdi_raw |>
  as_tibble() |>
  pivot_longer(
    cols      = any_of(students_indicators$wdi_code),
    names_to  = "wdi_code",
    values_to = "value"
  ) |>
  filter(!is.na(value), !is.na(year), !is.na(iso2c)) |>
  inner_join(wdi_code_to_meta, by = "wdi_code") |>
  transmute(
    indicator_id = NA_integer_,
    source_code,
    country_iso3 = iso2c,
    year         = as.integer(year),
    value        = as.double(value),
    plot
  )

# WDI returns ISO2 codes; convert to ISO3 using the package's built-in lookup.
iso2_to_iso3 <- WDI::WDI_data$country |>
  as_tibble() |>
  select(iso2c, iso3c)

students_combined <- students_combined |>
  left_join(iso2_to_iso3, by = c("country_iso3" = "iso2c")) |>
  mutate(country_iso3 = coalesce(iso3c, country_iso3)) |>
  select(-iso3c)

write_csv(students_combined, file.path(RAW_OUT, "students_raw.csv"))
message("Students rows: ", nrow(students_combined))
message("Students indicators fetched: ", n_distinct(students_combined$source_code),
        " of ", nrow(students_indicators))
