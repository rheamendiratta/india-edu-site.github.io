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
  ~id,  ~source_code,                    ~database_id,  ~plot,    ~label,
  # GER
  151,  "WB_WDI_SE_PRE_ENRR",            "WB_WDI",      "GER",    "GER, Pre-Primary, Total",
  148,  "WB_WDI_SE_PRE_ENRR_FE",         "WB_WDI",      "GER",    "GER, Pre-Primary, Female",
  149,  "WB_WDI_SE_PRE_ENRR_MA",         "WB_WDI",      "GER",    "GER, Pre-Primary, Male",
  89,   "WB_WDI_SE_PRM_ENRR",            "WB_WDI",      "GER",    "GER, Primary, Total",
  110,  "WB_WDI_SE_PRM_ENRR_FE",         "WB_WDI",      "GER",    "GER, Primary, Female",
  63,   "WB_WDI_SE_PRM_ENRR_MA",         "WB_WDI",      "GER",    "GER, Primary, Male",
  77,   "WB_WDI_SE_SEC_ENRR",            "WB_WDI",      "GER",    "GER, Secondary, Total",
  138,  "WB_WDI_SE_SEC_ENRR_FE",         "WB_WDI",      "GER",    "GER, Secondary, Female",
  125,  "WB_WDI_SE_SEC_ENRR_MA",         "WB_WDI",      "GER",    "GER, Secondary, Male",
  116,  "WB_WDI_SE_TER_ENRR",            "WB_WDI",      "GER",    "GER, Tertiary, Total",
  104,  "WB_WDI_SE_TER_ENRR_FE",         "WB_WDI",      "GER",    "GER, Tertiary, Female",
  126,  "WB_WDI_SE_TER_ENRR_MA",         "WB_WDI",      "GER",    "GER, Tertiary, Male",
  # NER
  168,  "WB_WDI_SE_PRM_NENR",            "WB_WDI",      "NER",    "NER, Primary, Total",
  186,  "WB_WDI_SE_PRM_NENR_FE",         "WB_WDI",      "NER",    "NER, Primary, Female",
  191,  "WB_WDI_SE_PRM_NENR_MA",         "WB_WDI",      "NER",    "NER, Primary, Male",
  152,  "WB_WDI_SE_SEC_NENR",            "WB_WDI",      "NER",    "NER, Secondary, Total",
  158,  "WB_WDI_SE_SEC_NENR_FE",         "WB_WDI",      "NER",    "NER, Secondary, Female",
  171,  "WB_WDI_SE_SEC_NENR_MA",         "WB_WDI",      "NER",    "NER, Secondary, Male",
  # Repetition
  162,  "WB_WDI_SE_PRM_REPT_ZS",         "WB_WDI",      "REP",    "Repetition Rate, Primary, Total",
  167,  "WB_WDI_SE_PRM_REPT_FE_ZS",      "WB_WDI",      "REP",    "Repetition Rate, Primary, Female",
  161,  "WB_WDI_SE_PRM_REPT_MA_ZS",      "WB_WDI",      "REP",    "Repetition Rate, Primary, Male",
  # Persistence
  117,  "WB_WDI_SE_PRM_PRSL_ZS",         "WB_WDI",      "PERS",   "Persistence to Last Grade, Primary, Total",
  73,   "WB_WDI_SE_PRM_PRSL_FE_ZS",      "WB_WDI",      "PERS",   "Persistence to Last Grade, Primary, Female",
  56,   "WB_WDI_SE_PRM_PRSL_MA_ZS",      "WB_WDI",      "PERS",   "Persistence to Last Grade, Primary, Male"
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

# ── Fetch students indicators ──
all_students <- list()
for (i in seq_len(nrow(students_indicators))) {
  row <- students_indicators[i, ]
  df  <- fetch_one(row$id, row$source_code, row$database_id, row$label)
  if (!is.null(df)) {
    df$plot <- row$plot
    all_students[[row$source_code]] <- df
  }
  Sys.sleep(0.5)
}

students_combined <- bind_rows(all_students)
write_csv(students_combined, file.path(RAW_OUT, "students_raw.csv"))
message("Students rows: ", nrow(students_combined))
