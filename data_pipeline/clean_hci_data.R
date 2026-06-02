# clean_hci_data.R
# Combines WB HCI (fetched), HTS/LAYS/EYS (from students_wb), HDI (from map_raw)
# Requires: data/raw/wb/hci_raw.csv   (run fetch_hci.R first)
#           data/clean/students_wb.rds (run clean_students_data.R first)
#           data/raw/wb/map_raw.csv    (run fetch_wb.R first)
# Output:   data/clean/hci_wb.rds
#           shiny/world/human-capital/data/hci_wb.rds

library(dplyr)
library(readr)
library(fs)

CLEAN_OUT <- "data/clean"
APP_DATA  <- "shiny/world/human-capital/data"
dir_create(CLEAN_OUT)
dir_create(APP_DATA)

WB_AGGREGATES <- c(
  "WLD", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF",
  "AFE", "AFW", "ARB", "CEB", "EAP", "ECA", "EMU",
  "HIC", "HPC", "IBD", "IBT", "IDA", "IDX", "LAC",
  "LDC", "LIC", "LMC", "LMY", "LTE", "MIC", "MNA",
  "OED", "OSS", "PRE", "PSS", "PST", "SSA",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC"
)

# ── HCI (total/female/male) ───────────────────────────────────────────────────
message("Loading HCI raw data...")
hci_raw <- read_csv("data/raw/wb/hci_raw.csv", show_col_types = FALSE)

hci_data <- hci_raw |>
  filter(!country_iso3 %in% WB_AGGREGATES) |>
  filter(!is.na(value), !is.na(year)) |>
  distinct(source_code, country_iso3, year, .keep_all = TRUE)

message("HCI rows: ", nrow(hci_data), " | countries: ", n_distinct(hci_data$country_iso3))

# ── HTS / LAYS / EYS (from students_wb) ──────────────────────────────────────
message("Loading students_wb (HTS/LAYS/EYS)...")
students_wb <- readRDS(file.path(CLEAN_OUT, "students_wb.rds"))

hts_lays_eys <- students_wb |>
  filter(plot %in% c("HTS", "LAYS", "EYS"))

message("HTS/LAYS/EYS rows: ", nrow(hts_lays_eys))

# ── HDI (from map_raw) ────────────────────────────────────────────────────────
message("Loading map_raw (HDI)...")
map_raw <- read_csv("data/raw/wb/map_raw.csv", show_col_types = FALSE)

hdi_data <- map_raw |>
  filter(source_code == "WB_SSGD_HDI_INDEX") |>
  filter(!country_iso3 %in% WB_AGGREGATES, !is.na(value)) |>
  transmute(
    source_code,
    plot         = "HDI",
    level        = "Total",
    gender       = "Total",
    country_iso3,
    year,
    value
  ) |>
  distinct(source_code, country_iso3, year, .keep_all = TRUE)

message("HDI rows: ", nrow(hdi_data), " | countries: ", n_distinct(hdi_data$country_iso3))

# ── HCI health components from extras_raw (stunting + adult survival) ────────
extras_path <- "data/raw/wb/extras_raw.csv"
hci_health <- if (file.exists(extras_path)) {
  message("Loading HCI health components from extras_raw...")
  read_csv(extras_path, show_col_types = FALSE) |>
    filter(plot %in% c("HCI_STNT", "HCI_AMRT")) |>
    filter(!country_iso3 %in% WB_AGGREGATES, !is.na(value)) |>
    distinct(source_code, country_iso3, year, .keep_all = TRUE)
} else {
  message("extras_raw.csv not found — HCI health components skipped")
  tibble()
}
message("HCI health rows: ", nrow(hci_health))

# ── Combine ───────────────────────────────────────────────────────────────────
hci_wb <- bind_rows(hci_data, hts_lays_eys, hdi_data, hci_health) |>
  arrange(plot, level, gender, country_iso3, year)

message("\nCombined rows: ", nrow(hci_wb))
message("Plots: ", paste(unique(hci_wb$plot), collapse = ", "))
message("India: ", any(hci_wb$country_iso3 == "IND"))

saveRDS(hci_wb, file.path(CLEAN_OUT, "hci_wb.rds"))
file_copy(file.path(CLEAN_OUT, "hci_wb.rds"),
          file.path(APP_DATA,  "hci_wb.rds"),
          overwrite = TRUE)

message("\nSaved: ", file.path(CLEAN_OUT, "hci_wb.rds"))
message("Saved: ", file.path(APP_DATA,  "hci_wb.rds"))
