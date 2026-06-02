# clean_schools_data.R
# Cleans WB schools indicators for the Schools & System app.
# Input:  data/raw/wb/schools_raw.csv
# Output: data/clean/schools_wb.rds
#
# Schema: source_code · plot · level · gender · country_iso3 · year · value

library(dplyr)
library(readr)

CLEAN_OUT <- "data/clean"

WB_AGGREGATES <- c(
  "WLD","EAS","ECS","LCN","MEA","NAC","SAS","SSF",
  "AFE","AFW","ARB","CEB","EAP","ECA","EMU",
  "HIC","HPC","IBD","IBT","IDA","IDX","LAC",
  "LDC","LIC","LMC","LMY","LTE","MIC","MNA",
  "OED","OSS","PRE","PSS","PST","SSA",
  "SST","TEA","TEC","TLA","TMN","TSA","TSS","UMC"
)

raw <- read_csv("data/raw/wb/schools_raw.csv", show_col_types = FALSE)
message("Raw rows: ", nrow(raw))

n_raw <- nrow(raw)
raw <- raw |> distinct(source_code, country_iso3, year, .keep_all = TRUE)
if (nrow(raw) < n_raw)
  message("  Removed ", n_raw - nrow(raw), " duplicate rows")

schools_wb <- raw |>
  filter(!country_iso3 %in% WB_AGGREGATES,
         !is.na(value), !is.na(year), !is.na(country_iso3)) |>
  select(source_code, plot, level, gender, country_iso3, year, value) |>
  arrange(plot, level, country_iso3, year)

# Add private enrolment share (PRIV) from the already-clean students_wb
# rather than re-fetching — the data is identical to what the students app uses
priv <- readRDS("data/clean/students_wb.rds") |> filter(plot == "PRIV")
schools_wb <- bind_rows(schools_wb, priv) |>
  arrange(plot, level, country_iso3, year)

message("Clean rows: ", nrow(schools_wb))
message("Countries: ", n_distinct(schools_wb$country_iso3))
message("India: ", any(schools_wb$country_iso3 == "IND"))

message("\nCoverage by plot/level:")
schools_wb |>
  group_by(plot, level) |>
  summarise(
    countries = n_distinct(country_iso3),
    min_yr = min(year), max_yr = max(year),
    india = any(country_iso3 == "IND"),
    .groups = "drop"
  ) |> print()

saveRDS(schools_wb, file.path(CLEAN_OUT, "schools_wb.rds"))
message("Saved: data/clean/schools_wb.rds")
