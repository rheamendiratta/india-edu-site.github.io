# fetch_hci.R
# Fetches World Bank HCI (overall, female, male) via WB REST API.
# HTS/LAYS/EYS are reused from students_wb.rds; HDI from map_raw.csv.
# Output: data/raw/wb/hci_raw.csv

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(fs)

RAW_OUT <- "data/raw/wb"
dir_create(RAW_OUT)

WB_BASE <- "https://api.worldbank.org/v2/country/all/indicator"

INDICATORS <- tribble(
  ~source_code,        ~wb_code,          ~plot,  ~level,   ~gender,
  "WB_HCI_OVRL",      "HD.HCI.OVRL",     "HCI",  "Total",  "Total",
  "WB_HCI_OVRL_FE",   "HD.HCI.OVRL.FE",  "HCI",  "Total",  "Female",
  "WB_HCI_OVRL_MA",   "HD.HCI.OVRL.MA",  "HCI",  "Total",  "Male"
)

fetch_indicator <- function(wb_code, source_code, plot, level, gender) {
  url <- paste0(WB_BASE, "/", wb_code, "?format=json&per_page=10000&mrv=35")
  message("Fetching ", wb_code, " ...")

  body <- tryCatch(
    fromJSON(url, flatten = TRUE),
    error = function(e) { message("  Error: ", e$message); NULL }
  )
  if (is.null(body) || length(body) < 2 || is.null(body[[2]])) return(NULL)

  df <- as_tibble(body[[2]]) |>
    filter(!is.na(value)) |>
    transmute(
      source_code  = source_code,
      plot         = plot,
      level        = level,
      gender       = gender,
      country_iso3 = countryiso3code,
      year         = as.integer(date),
      value        = as.numeric(value)
    ) |>
    filter(nchar(country_iso3) == 3)

  message("  -> ", nrow(df), " obs | ", n_distinct(df$country_iso3),
          " countries | India: ", any(df$country_iso3 == "IND"))
  df
}

rows <- mapply(
  fetch_indicator,
  wb_code     = INDICATORS$wb_code,
  source_code = INDICATORS$source_code,
  plot        = INDICATORS$plot,
  level       = INDICATORS$level,
  gender      = INDICATORS$gender,
  SIMPLIFY    = FALSE
)

hci_raw <- bind_rows(rows)
message("\nTotal rows: ", nrow(hci_raw))

write_csv(hci_raw, file.path(RAW_OUT, "hci_raw.csv"))
message("Saved: data/raw/wb/hci_raw.csv")
