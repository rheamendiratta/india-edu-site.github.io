# fetch_extras.R
# Fetches additional WDI indicators not in students_raw.csv:
#   - Enrolment numbers (absolute)
#   - Extended tertiary attainment (short-cycle, master's, doctoral)
#   - HCI health components (stunting, adult survival)
# Output: data/raw/wb/extras_raw.csv

library(WDI)
library(dplyr)
library(readr)
library(tidyr)
library(fs)

RAW_OUT <- "data/raw/wb"
dir_create(RAW_OUT)

extras_indicators <- tribble(
  ~id,  ~source_code,                       ~plot,       ~level,              ~gender,
  # Enrolment numbers (Tab 1)
  NA,   "WB_WDI_SE_PRM_ENRL",               "ENRL_NUM",  "Primary",           "Total",
  NA,   "WB_WDI_SE_PRM_ENRL_FE_ZS",         "ENRL_PCT_F","Primary",           "Total",
  NA,   "WB_WDI_SE_SEC_ENRL",               "ENRL_NUM",  "Secondary",         "Total",
  NA,   "WB_WDI_SE_SEC_ENRL_FE_ZS",         "ENRL_PCT_F","Secondary",         "Total",
  NA,   "WB_WDI_SE_SEC_ENRL_GC",            "ENRL_NUM",  "Secondary General", "Total",
  # Extended tertiary attainment (Tab 4)
  NA,   "WB_WDI_SE_TER_CUAT_ST_ZS",         "ATTAIN_ST", "Primary",           "Total",
  NA,   "WB_WDI_SE_TER_CUAT_MS_ZS",         "ATTAIN_MS", "Primary",           "Total",
  NA,   "WB_WDI_SE_TER_CUAT_MS_FE_ZS",      "ATTAIN_MS", "Primary",           "Female",
  NA,   "WB_WDI_SE_TER_CUAT_MS_MA_ZS",      "ATTAIN_MS", "Primary",           "Male",
  NA,   "WB_WDI_SE_TER_CUAT_DO_ZS",         "ATTAIN_DO", "Primary",           "Total",
  NA,   "WB_WDI_SE_TER_CUAT_DO_FE_ZS",      "ATTAIN_DO", "Primary",           "Female",
  NA,   "WB_WDI_SE_TER_CUAT_DO_MA_ZS",      "ATTAIN_DO", "Primary",           "Male",
  # HCI health components (Human Capital app)
  NA,   "WB_WDI_HD_HCI_STNT",               "HCI_STNT",  "Total",             "Total",
  NA,   "WB_WDI_HD_HCI_AMRT",               "HCI_AMRT",  "Total",             "Total"
)

extras_indicators <- extras_indicators |>
  mutate(wdi_code = gsub("_", ".", sub("^WB_WDI_", "", source_code)))

message("Fetching ", nrow(extras_indicators), " extras indicators via WDI...")

wdi_raw <- WDI(
  country   = "all",
  indicator = extras_indicators$wdi_code,
  start     = 1990,
  end       = 2025,
  extra     = FALSE,
  language  = "en"
)

iso2_to_iso3 <- WDI::WDI_data$country |>
  as_tibble() |>
  select(iso2c, iso3c)

code_meta <- extras_indicators |> select(source_code, wdi_code, plot, level, gender)

extras_combined <- wdi_raw |>
  as_tibble() |>
  pivot_longer(
    cols      = any_of(extras_indicators$wdi_code),
    names_to  = "wdi_code",
    values_to = "value"
  ) |>
  filter(!is.na(value), !is.na(year), !is.na(iso2c)) |>
  inner_join(code_meta, by = "wdi_code") |>
  transmute(source_code, plot, level, gender,
            country_iso3 = iso2c, year = as.integer(year), value) |>
  left_join(iso2_to_iso3, by = c("country_iso3" = "iso2c")) |>
  mutate(country_iso3 = coalesce(iso3c, country_iso3)) |>
  select(-iso3c) |>
  filter(nchar(country_iso3) == 3) |>
  distinct(source_code, country_iso3, year, .keep_all = TRUE)

message("Rows fetched: ", nrow(extras_combined))
message("Plots: ", paste(unique(extras_combined$plot), collapse = ", "))
message("India: ", any(extras_combined$country_iso3 == "IND"))

write_csv(extras_combined, file.path(RAW_OUT, "extras_raw.csv"))
message("Saved: data/raw/wb/extras_raw.csv")
