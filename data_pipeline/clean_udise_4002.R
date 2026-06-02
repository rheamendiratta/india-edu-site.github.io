# clean_udise_4002.R
# UDISE private share cleaning for World > Students — Tab 1: Enrolment & Access
#
# Indicator 4002: Enrolment by Location, School Category and School Management
#
# Old ag-grid files (2012-13 to 2021-22) crash readxl due to file format issues.
# Only the 2024-25 UDISE+ file is parsed here.
# For a time-series, WB_WDI_SE_PRM_PRIV_ZS / WB_WDI_SE_SEC_PRIV_ZS are used.
#
# Output: data/clean/udise_4002.rds
# Schema:
#   indicator_id  "4002"
#   acad_year     "2024-25"
#   year          2024L
#   level         NEP-normalised level name (Preparatory, Middle, Secondary)
#   wb_level      matchable WB level
#   value         private share (%)
#   label         "Private Share (UDISE)"

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

UDISE_IN  <- "data/raw/udise/4002"
CLEAN_OUT <- "data/clean"

# "Private Unaided (Recognized)" and Madrasa equivalent are the private categories.
PRIVATE_MGMT <- c(
  "Private Unaided (Recognized)",
  "Madrasa Private Unaided (Recognized)"
)

# Map UDISE+ level grouping to NEP-normalised output names.
level_map <- c(
  "Foundational and Preparatory" = "Preparatory",
  "Middle"                       = "Middle",
  "Secondary"                    = "Secondary"
)

wb_level_priv <- c(
  "Preparatory" = "Primary",
  "Middle"      = NA_character_,
  "Secondary"   = "Secondary"
)

parse_4002_new <- function(filepath) {
  acad_year <- str_extract(basename(filepath), "\\d{4}-\\d{2,4}")
  year      <- as.integer(str_extract(acad_year, "^\\d{4}"))

  raw <- read_excel(filepath, sheet = "UDISE+",
                    col_names = FALSE, .name_repair = "minimal")

  # Data rows: row 9 onwards (1-indexed), skipping header rows 1-8.
  # Col 1 = management type, Col 2 = level category, Col 3 = urban/rural
  # Cols 4:ncol alternate Girls/Boys for each class (Balvatika-1 through Class-12)
  n_cols <- ncol(raw)
  data_rows <- raw[9:nrow(raw), ]

  # Sum all numeric enrollment columns (cols 4 to n_cols) — avoid dplyr mutate
  # on unnamed columns by working directly with a numeric matrix.
  enr_cols   <- 4:n_cols
  enr_matrix <- sapply(enr_cols, function(i) {
    suppressWarnings(as.numeric(as.character(data_rows[[i]])))
  })
  enr_total  <- rowSums(enr_matrix, na.rm = TRUE)

  df <- tibble(
    management = as.character(data_rows[[1]]),
    level_raw  = as.character(data_rows[[2]]),
    enrolment  = enr_total
  ) |>
    filter(!is.na(management), management != "Total") |>
    filter(!is.na(level_raw), level_raw != "") |>
    filter(level_raw %in% names(level_map)) |>
    mutate(
      is_private = management %in% PRIVATE_MGMT,
      level      = unname(level_map[level_raw])
    ) |>
    group_by(level) |>
    summarise(
      total_enr   = sum(enrolment, na.rm = TRUE),
      private_enr = sum(enrolment[is_private], na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(total_enr > 0) |>
    mutate(
      indicator_id = "4002",
      acad_year    = acad_year,
      year         = year,
      wb_level     = unname(wb_level_priv[level]),
      value        = round(private_enr / total_enr * 100, 2),
      label        = "Private Share (UDISE)"
    ) |>
    select(indicator_id, acad_year, year, level, wb_level, value, label)

  message("  ✓ ", nrow(df), " rows for ", acad_year)
  df
}

# Find UDISE+ files in 4002 folder
files_new <- list.files(UDISE_IN, pattern = "\\.xlsx$", full.names = TRUE)
udise_plus_files <- files_new[sapply(files_new, function(f) {
  tryCatch(excel_sheets(f) == "UDISE+", error = function(e) FALSE)
})]

message("Found ", length(udise_plus_files), " UDISE+ file(s) in 4002/")

results <- map(sort(udise_plus_files), function(f) {
  message("  Processing: ", basename(f))
  tryCatch(parse_4002_new(f), error = function(e) {
    message("  ✗ ", basename(f), ": ", e$message); NULL
  })
})

udise_4002 <- bind_rows(compact(results))
message("Total rows: ", nrow(udise_4002))

if (nrow(udise_4002) > 0) {
  print(udise_4002)
  saveRDS(udise_4002, file.path(CLEAN_OUT, "udise_4002.rds"))
  message("Saved: ", file.path(CLEAN_OUT, "udise_4002.rds"))
} else {
  message("No data — skipping save.")
}
