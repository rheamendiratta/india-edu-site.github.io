# fetch_finance.R
# Fetches World Bank WDI finance/expenditure indicators via WB REST API.
# Output: data/raw/wb/finance_raw.csv

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(fs)

RAW_OUT <- "data/raw/wb"
dir_create(RAW_OUT)

WB_BASE <- "https://api.worldbank.org/v2/country/all/indicator"

INDICATORS <- tribble(
  ~source_code,                   ~wb_code,              ~plot,         ~level,       ~gender,
  "WB_WDI_SE_XPD_TOTL_GD_ZS",   "SE.XPD.TOTL.GD.ZS",  "EXP_GDP",     "Total",      "Total",
  "WB_WDI_SE_XPD_TOTL_GB_ZS",   "SE.XPD.TOTL.GB.ZS",  "EXP_GOVTBDG", "Total",      "Total",
  "WB_WDI_SE_XPD_PRIM_PC_ZS",   "SE.XPD.PRIM.PC.ZS",  "EXP_PER_STU", "Primary",    "Total",
  "WB_WDI_SE_XPD_SECO_PC_ZS",   "SE.XPD.SECO.PC.ZS",  "EXP_PER_STU", "Secondary",  "Total",
  "WB_WDI_SE_XPD_TERT_PC_ZS",   "SE.XPD.TERT.PC.ZS",  "EXP_PER_STU", "Tertiary",   "Total",
  "WB_WDI_SE_XPD_PRIM_ZS",      "SE.XPD.PRIM.ZS",     "EXP_SHARE",   "Primary",    "Total",
  "WB_WDI_SE_XPD_SECO_ZS",      "SE.XPD.SECO.ZS",     "EXP_SHARE",   "Secondary",  "Total",
  "WB_WDI_SE_XPD_TERT_ZS",      "SE.XPD.TERT.ZS",     "EXP_SHARE",   "Tertiary",   "Total"
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

finance_raw <- bind_rows(rows)
message("\nTotal rows: ", nrow(finance_raw))

write_csv(finance_raw, file.path(RAW_OUT, "finance_raw.csv"))
message("Saved: data/raw/wb/finance_raw.csv")
