library(dplyr)
library(readr)
library(arrow)
library(jsonlite)
library(fs)


CLEAN_OUT <- "data/clean"
dir_create(CLEAN_OUT)

# Load raw data 

message("Reading raw data...")
raw <- read_csv("data/raw/wb/map_raw.csv")

#  Load registry for indicator metadata 

map_ids <- c(195, 16, 57, 18, 24, 168, 96, 107, 101, 15)

registry <- read_json("data_pipeline/indicator_registry.json", simplifyVector = TRUE) |>
  filter(id %in% map_ids) |>
  select(id, source_code, label)
# Clean 

message("Cleaning...")

map_clean <- raw |>
  # Keep only country-level rows (filter out aggregates like regions)
  filter(
    !country_iso3 %in% c(
      "WLD", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF",  # WB regions
      "AFE", "AFW", "ARB", "CEB", "EAP", "ECA", "EMU",          # more aggregates
      "HIC", "HPC", "IBD", "IBT", "IDA", "IDX", "LAC",
      "LDC", "LIC", "LMC", "LMY", "LTE", "MIC", "MNA",
      "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA",
      "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC"
    )
  ) |>
  # Join registry to get label and confirm map indicators only
  inner_join(registry, by = c("source_code", "indicator_id" = "id")) |>
  # Keep only what the app needs
  select(
    indicator_id,
    source_code,
    label,
    country_iso3,
    year,
    value
  ) |>
  # Remove any remaining NAs
  filter(!is.na(value), !is.na(country_iso3), !is.na(year))

message("Rows after cleaning: ", nrow(map_clean))
message("Countries: ", n_distinct(map_clean$country_iso3))
message("Indicators: ", n_distinct(map_clean$source_code))

# Check India is present 

india_check <- map_clean |>
  filter(country_iso3 == "IND") |>
  group_by(label) |>
  summarise(years = n(), min_year = min(year), max_year = max(year))

message("\nIndia coverage:")
print(india_check)

# Save parquet

write_parquet(map_clean, file.path(CLEAN_OUT, "map.parquet"))
message("\nSaved to data/clean/map.parquet")

# Also save a coverage summary for the bar chart 

coverage <- map_clean |>
  group_by(source_code, label, year) |>
  summarise(
    n_countries  = n_distinct(country_iso3),
    india_has_data = any(country_iso3 == "IND"),
    .groups = "drop"
  )

write_parquet(coverage, file.path(CLEAN_OUT, "map_coverage.parquet"))
message("Saved to data/clean/map_coverage.parquet")
