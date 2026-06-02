# clean_finance_data.R
# Cleans fetched finance indicators into app-ready RDS.
# Requires: data/raw/wb/finance_raw.csv (run fetch_finance.R first)
# Output:   data/clean/finance_wb.rds
#           shiny/world/finance/data/finance_wb.rds

library(dplyr)
library(readr)
library(fs)

CLEAN_OUT <- "data/clean"
APP_DATA  <- "shiny/world/finance/data"
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

message("Loading finance raw data...")
finance_raw <- read_csv("data/raw/wb/finance_raw.csv", show_col_types = FALSE)

finance_wb <- finance_raw |>
  filter(!country_iso3 %in% WB_AGGREGATES) |>
  filter(!is.na(value), !is.na(year), !is.na(country_iso3)) |>
  distinct(source_code, country_iso3, year, .keep_all = TRUE) |>
  arrange(plot, level, gender, country_iso3, year)

message("Rows: ", nrow(finance_wb))
message("Countries: ", n_distinct(finance_wb$country_iso3))
message("India: ", any(finance_wb$country_iso3 == "IND"))
message("Plots: ", paste(unique(finance_wb$plot), collapse = ", "))

saveRDS(finance_wb, file.path(CLEAN_OUT, "finance_wb.rds"))
file_copy(file.path(CLEAN_OUT, "finance_wb.rds"),
          file.path(APP_DATA,  "finance_wb.rds"),
          overwrite = TRUE)

message("\nSaved: ", file.path(CLEAN_OUT, "finance_wb.rds"))
message("Saved: ", file.path(APP_DATA,  "finance_wb.rds"))
