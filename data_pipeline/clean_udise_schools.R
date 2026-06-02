# clean_udise_schools.R
# Cleans UDISE indicator 1003 (Schools by School Management and Category).
#
# ag-grid (2012-13 to 2021-22):
#   Row 4 = headers (col1=Location, col2=Management, col13=Total)
#   Rows 5-15 = data by management type
#   Row 16 = Overall (skip)
#
# UDISE+ (2024-25+):
#   Row 6 = headers; last column = Overall
#   Rows 7-27 = data; Row 28 = Total (skip)
#
# Output schema: acad_year · year · category · n_schools · pct_schools

library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(fs)

UDISE_IN  <- "data/raw/udise/1003"
CLEAN_OUT <- "data/clean"

# Files are named {indicator}_{start_year}_{suffix}.xlsx  e.g. 1003_2012_13.xlsx
# Skip the leading indicator ID and grab the academic start year (second group)
extract_acad <- function(fp) str_extract(basename(fp), "(?<=_)\\d{4}[-_]\\d{2,4}")
extract_year <- function(fp) as.integer(str_extract(basename(fp), "(?<=_)\\d{4}(?=_)"))

# Assign management label to broad category (returns NA for excluded rows)
categorise <- function(labels) {
  case_when(
    grepl("department of education|tribal welfare|local body|other.*govt|other state|
           central govt|central tibetan|railway|kendriya|jawahar|sainik|
           social welfare|minority|labour|veda|gurukul|patha|madrasa aided|
           madrasa.*recognised.*aided|ministry of labour",
          tolower(labels))         ~ "Government",
    grepl("government aided|partially.*govt.*aided",
          tolower(labels))         ~ "Aided",
    grepl("private unaided|madrasa private unaided",
          tolower(labels))         ~ "Private Unaided",
    grepl("unrecogni|no response|madrasa unrecogni",
          tolower(labels))         ~ "Other",
    TRUE                           ~ NA_character_
  )
}

# Pull a column by integer position from a tibble (column names may be empty)
col_chr <- function(df, j) as.character(unlist(df[, j]))
col_num <- function(df, j) suppressWarnings(as.numeric(unlist(df[, j])))

parse_old <- function(fp) {
  raw <- read_excel(fp, sheet = "ag-grid", col_names = FALSE, .name_repair = "minimal")
  # Row 4 is the header; last column is "Total"
  # Data rows are 5 through (nrow-1); last data row is "Overall" — exclude it
  n <- nrow(raw)
  data <- raw[5:(n - 1), ]
  tibble(
    management = col_chr(data, 2),
    n_schools  = col_num(data, ncol(raw))
  ) |>
    filter(!is.na(n_schools), !is.na(management),
           management != "",
           !grepl("^(overall|total|\\.)$", tolower(trimws(management))))
}

parse_new <- function(fp) {
  raw <- read_excel(fp, sheet = "UDISE+", col_names = FALSE, .name_repair = "minimal")
  # Row 6 = headers; data rows are 7 through (nrow-1); last row is Total — exclude
  n <- nrow(raw)
  data <- raw[7:(n - 1), ]
  tibble(
    management = col_chr(data, 1),
    n_schools  = col_num(data, ncol(raw))
  ) |>
    filter(!is.na(n_schools), !is.na(management),
           management != "",
           !grepl("^(total|overall|\\.)$", tolower(trimws(management))))
}

files <- sort(dir_ls(UDISE_IN, glob = "*.xlsx"))
message("Parsing ", length(files), " UDISE 1003 files...")

results <- map(files, function(fp) {
  sheets <- tryCatch(excel_sheets(fp), error = function(e) NULL)
  if (is.null(sheets)) { message("  Skip: ", basename(fp)); return(NULL) }

  acad <- extract_acad(fp)
  yr   <- extract_year(fp)

  df <- tryCatch({
    if ("UDISE+" %in% sheets) parse_new(fp)
    else if ("ag-grid" %in% sheets) parse_old(fp)
    else NULL
  }, error = function(e) {
    message("  Error: ", basename(fp), " — ", conditionMessage(e)); NULL
  })
  if (is.null(df) || nrow(df) == 0) { message("  Empty: ", basename(fp)); return(NULL) }
  message("  ", acad, ": ", nrow(df), " management rows")
  df |> mutate(acad_year = acad, year = yr)
})

raw_long <- bind_rows(compact(results))
cat("Total parsed rows:", nrow(raw_long), "\n")

udise_schools <- raw_long |>
  mutate(category = categorise(management)) |>
  filter(!is.na(category)) |>
  group_by(acad_year, year, category) |>
  summarise(n_schools = sum(n_schools, na.rm = TRUE), .groups = "drop") |>
  group_by(acad_year, year) |>
  mutate(
    total       = sum(n_schools),
    pct_schools = round(100 * n_schools / total, 2)
  ) |>
  ungroup() |>
  arrange(year, category)

message("Output rows: ", nrow(udise_schools))
print(udise_schools, n = 40)

saveRDS(udise_schools, file.path(CLEAN_OUT, "udise_schools.rds"))
message("Saved: data/clean/udise_schools.rds")
