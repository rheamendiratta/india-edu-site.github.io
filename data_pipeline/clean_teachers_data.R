# clean_teachers_data.R
# Cleans WB teacher indicators for World > Teachers app.
# Input:  data/raw/wb/teachers_raw.csv
# Output: data/clean/teachers_wb.rds

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

message("Loading teachers WB data...")
raw <- read_csv("data/raw/wb/teachers_raw.csv", show_col_types = FALSE)

n_raw <- nrow(raw)
raw <- raw |> distinct(source_code, country_iso3, year, .keep_all = TRUE)
if (nrow(raw) < n_raw)
  message("  Removed ", n_raw - nrow(raw), " duplicate rows")

teachers_wb <- raw |>
  filter(!country_iso3 %in% WB_AGGREGATES,
         !is.na(value), !is.na(year), !is.na(country_iso3)) |>
  select(source_code, plot, level, gender, country_iso3, year, value) |>
  arrange(plot, level, gender, country_iso3, year)

message("WB rows: ", nrow(teachers_wb))
message("WB countries: ", n_distinct(teachers_wb$country_iso3))
message("India in WB: ", any(teachers_wb$country_iso3 == "IND"))

saveRDS(teachers_wb, file.path(CLEAN_OUT, "teachers_wb.rds"))
message("Saved: data/clean/teachers_wb.rds")

message("\nCoverage by plot/level/gender:")
teachers_wb |>
  group_by(plot, level, gender) |>
  summarise(countries = n_distinct(country_iso3),
            min_year  = min(year), max_year = max(year),
            india     = any(country_iso3 == "IND"), .groups = "drop") |>
  print(n = 40)
