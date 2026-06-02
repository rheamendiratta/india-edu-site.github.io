# fetch_schools.R
# Fetches World Bank indicators for the Schools & System app.
# Uses WB REST API directly (httr2) — fast, no WDI package latency.
#
# Indicators:
#   SE.PRE.DURS   Pre-primary education duration (years)
#   SE.PRM.DURS   Primary education duration (years)
#   SE.SEC.DURS   Secondary education duration (years)
#   SE.COM.DURS   Compulsory education duration (years)
#   IT.NET.EDUC.ZS  Schools connected to the internet (%)
#
# Output: data/raw/wb/schools_raw.csv

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(fs)

RAW_OUT <- "data/raw/wb"
dir_create(RAW_OUT)

WB_BASE <- "https://api.worldbank.org/v2/country/all/indicator"

INDICATORS <- tribble(
  ~source_code,             ~wb_code,           ~plot,        ~level,         ~gender,
  "WB_WDI_SE_PRE_DURS",    "SE.PRE.DURS",      "SCHED_DUR",  "Pre-primary",  "Total",
  "WB_WDI_SE_PRM_DURS",    "SE.PRM.DURS",      "SCHED_DUR",  "Primary",      "Total",
  "WB_WDI_SE_SEC_DURS",    "SE.SEC.DURS",      "SCHED_DUR",  "Secondary",    "Total",
  "WB_WDI_SE_COM_DURS",    "SE.COM.DURS",      "COMP_DUR",   "Compulsory",   "Total",
  "WB_WDI_IT_NET_EDUC_ZS", "IT.NET.EDUC.ZS",  "INTERNET_SCH","Primary",     "Total"
)

fetch_indicator <- function(wb_code, source_code, plot, level, gender) {
  # Use jsonlite::fromJSON with URL — handles paging via per_page=10000
  url <- paste0(WB_BASE, "/", wb_code, "?format=json&per_page=10000&mrv=25")
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
      country_name = country.value,
      year         = as.integer(date),
      value        = as.numeric(value)
    ) |>
    filter(nchar(country_iso3) == 3)

  n <- nrow(df)
  ctry <- n_distinct(df$country_iso3)
  ind <- any(df$country_iso3 == "IND")
  message("  -> ", n, " obs | ", ctry, " countries | India: ", ind)
  df
}

rows <- mapply(fetch_indicator,
               wb_code    = INDICATORS$wb_code,
               source_code = INDICATORS$source_code,
               plot       = INDICATORS$plot,
               level      = INDICATORS$level,
               gender     = INDICATORS$gender,
               SIMPLIFY   = FALSE)

schools_raw <- bind_rows(rows)
message("\nTotal rows: ", nrow(schools_raw))

write_csv(schools_raw, file.path(RAW_OUT, "schools_raw.csv"))
message("Saved: data/raw/wb/schools_raw.csv")
