# clean_udise_students.R
# Cleans all UDISE files for World > Students app
# Handles 4 formats:
#   - Old GER/NER (2012-13 to 2021-22): sheet "ag-grid", gender = Girls/Boys/Overall
#   - New NEP GER/NER (2022-23 onwards): sheet "UDISE+", gender = Boys/Girls/Overall
#   - 4016 Repetition Rate: levels named "Primary Repetition Rate" etc, gender has class codes
#   - 4033 Retention Rate: two level header rows, level names like "Primary (1 to 5)"
#
# Output: data/raw/udise/udise_students.rds

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(fs)
library(readr)

UDISE_IN  <- "data/raw/udise"
UDISE_OUT <- "data/raw/udise"

# ── Level name mapping -> NEP names ───────────────────────────────────────────
level_map <- c(
  # Old GER/NER format
  "Primary (I-V)"             = "Preparatory",
  "Upper Primary (VI-VIII)"   = "Middle",
  "Elementary (I-VIII)"       = "Elementary",
  "Secondary (IX-X)"          = "Secondary",
  "Higher Secondary (XI-XII)" = "Higher Secondary",
  # 4016 Repetition Rate format
  "Primary Repetition Rate"         = "Preparatory",
  "Upper Primary Repetition Rate"   = "Middle",
  "Secondary Repetition Rate"       = "Secondary",
  # 4033 Retention Rate format
  "Primary (1 to 5)"          = "Preparatory",
  "Elementary (1 to 8)"       = "Elementary",
  "Secondary (1 to 10)"       = "Secondary",
  "Higher Secondary (1 to 12)"= "Higher Secondary",
  # NEP names pass through unchanged
  "Foundational"  = "Foundational",
  "Preparatory"   = "Preparatory",
  "Middle"        = "Middle"
)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

extract_year <- function(filepath) {
  str_extract(path_file(filepath), "\\d{4}-\\d{2,4}")
}

year_numeric <- function(acad_year) {
  as.integer(str_extract(acad_year, "^\\d{4}"))
}

detect_format <- function(filepath) {
  if ("UDISE+" %in% excel_sheets(filepath)) "new" else "old"
}

# ── Parser: OLD GER/NER format ────────────────────────────────────────────────
# Rows: [1]=title, [2]=NIC, [3]=year, [4]=levels, [5]=genders, [6]=values
parse_old_ger_ner <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "ag-grid", col_names = FALSE, .name_repair = "minimal")
  level_row  <- as.character(unlist(raw[4, ]))
  gender_row <- as.character(unlist(raw[5, ]))
  value_row  <- as.character(unlist(raw[6, ]))
  n_cols     <- length(value_row)
  results    <- list()
  current_level <- NA_character_
  
  for (col_i in 2:n_cols) {
    lv <- level_row[col_i]
    if (!is.na(lv) && nchar(trimws(lv)) > 0) current_level <- trimws(lv)
    gender <- trimws(gender_row[col_i])
    val    <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(current_level) || !gender %in% c("Girls", "Boys", "Overall")) next
    if (is.na(val)) next
    if (str_detect(current_level, "^(SC|ST) ")) next  # skip SC/ST breakdowns
    
    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_map[current_level] %||% current_level,
      gender       = case_when(
        gender == "Girls"   ~ "Female",
        gender == "Boys"    ~ "Male",
        gender == "Overall" ~ "Total"
      ),
      value = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Parser: NEW/NEP GER/NER format ────────────────────────────────────────────
# Rows: [7]=levels, [8]=genders (Boys/Girls/Overall), [9]=values
parse_new_ger_ner <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "UDISE+", col_names = FALSE, .name_repair = "minimal")
  level_row  <- as.character(unlist(raw[7, ]))
  gender_row <- as.character(unlist(raw[8, ]))
  value_row  <- as.character(unlist(raw[9, ]))
  n_cols     <- length(value_row)
  results    <- list()
  current_level <- NA_character_
  
  for (col_i in 2:n_cols) {
    lv <- level_row[col_i]
    if (!is.na(lv) && nchar(trimws(lv)) > 0) current_level <- trimws(lv)
    gender <- trimws(gender_row[col_i])
    val    <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(current_level) || !gender %in% c("Girls", "Boys", "Overall")) next
    if (is.na(val)) next
    
    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_map[current_level] %||% current_level,
      gender       = case_when(
        gender == "Girls"   ~ "Female",
        gender == "Boys"    ~ "Male",
        gender == "Overall" ~ "Total"
      ),
      value = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Parser: 4016 Repetition Rate ──────────────────────────────────────────────
# Rows: [4]=levels ("Primary Repetition Rate" etc), [5]=genders with class codes
# Gender labels like "(10/13) Girls (19)" — extract Girls/Boys/Overall via regex
parse_repetition <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "ag-grid", col_names = FALSE, .name_repair = "minimal")
  level_row  <- as.character(unlist(raw[4, ]))
  gender_row <- as.character(unlist(raw[5, ]))
  value_row  <- as.character(unlist(raw[6, ]))
  n_cols     <- length(value_row)
  results    <- list()
  current_level <- NA_character_
  
  for (col_i in 2:n_cols) {
    lv <- level_row[col_i]
    if (!is.na(lv) && nchar(trimws(lv)) > 0) current_level <- trimws(lv)
    # Extract Girls/Boys/Overall from labels like "(10/13) Girls (19)"
    gender_raw <- trimws(gender_row[col_i])
    gender <- str_extract(gender_raw, "Girls|Boys|Overall")
    val    <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(current_level) || is.na(gender)) next
    if (is.na(val)) next
    
    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_map[trimws(current_level)] %||% trimws(current_level),
      gender       = case_when(
        gender == "Girls"   ~ "Female",
        gender == "Boys"    ~ "Male",
        gender == "Overall" ~ "Total"
      ),
      value = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Parser: 4033 Retention Rate ───────────────────────────────────────────────
# Rows: [5]=levels ("Primary (1 to 5)" etc), [6]=genders (Boys/Girls/Overall), [7]=values
# Data row label is "ALL INDIA" (uppercase)
parse_retention <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "ag-grid", col_names = FALSE, .name_repair = "minimal")
  level_row  <- as.character(unlist(raw[5, ]))
  gender_row <- as.character(unlist(raw[6, ]))
  value_row  <- as.character(unlist(raw[7, ]))
  n_cols     <- length(value_row)
  results    <- list()
  current_level <- NA_character_
  
  for (col_i in 2:n_cols) {
    lv <- level_row[col_i]
    if (!is.na(lv) && nchar(trimws(lv)) > 0) current_level <- trimws(lv)
    gender <- trimws(gender_row[col_i])
    val    <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(current_level) || !gender %in% c("Girls", "Boys", "Overall")) next
    if (is.na(val)) next
    
    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_map[current_level] %||% current_level,
      gender       = case_when(
        gender == "Girls"   ~ "Female",
        gender == "Boys"    ~ "Male",
        gender == "Overall" ~ "Total"
      ),
      value = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Process one indicator folder ──────────────────────────────────────────────
process_indicator <- function(indicator_id, parser_fn) {
  folder <- file.path(UDISE_IN, indicator_id)
  if (!dir_exists(folder)) { warning("Folder not found: ", folder); return(NULL) }
  files <- dir_ls(folder, glob = "*.xlsx")
  if (length(files) == 0) { warning("No xlsx files in: ", folder); return(NULL) }
  
  message("\nProcessing indicator ", indicator_id, " (", length(files), " files)...")
  
  results <- map(files, function(f) {
    acad_year <- extract_year(f)
    fmt       <- detect_format(f)
    message("  ", acad_year, " [", fmt, "] ", path_file(f))
    tryCatch(
      parser_fn(f, indicator_id),
      error = function(e) { warning("  ✗ Error in ", path_file(f), ": ", e$message); NULL }
    )
  })
  
  out <- bind_rows(compact(results))
  message("  ✓ ", nrow(out), " rows across ", n_distinct(out$year), " years")
  out
}

# ── Run all 4 indicators ───────────────────────────────────────────────────────
all_udise <- bind_rows(
  process_indicator("4010", function(f, id) {
    if (detect_format(f) == "new") parse_new_ger_ner(f, id) else parse_old_ger_ner(f, id)
  }),
  process_indicator("4011", function(f, id) {
    if (detect_format(f) == "new") parse_new_ger_ner(f, id) else parse_old_ger_ner(f, id)
  }),
  process_indicator("4016", function(f, id) parse_repetition(f, id)),
  process_indicator("4033", function(f, id) parse_retention(f, id))
)

# ── Labels ────────────────────────────────────────────────────────────────────
indicator_labels <- c(
  "4010" = "GER (UDISE)",
  "4011" = "NER (UDISE)",
  "4016" = "Repetition Rate (UDISE)",
  "4033" = "Retention Rate (UDISE)"
)

all_udise <- all_udise |>
  mutate(
    indicator_id = as.character(indicator_id),
    label        = indicator_labels[indicator_id]
  )

# ── Save ───────────────────────────────────────────────────────────────────────
saveRDS(all_udise, file.path(UDISE_OUT, "udise_students.rds"))
message("\n── UDISE cleaning complete ──")
message("Total rows: ", nrow(all_udise))
message("Indicators: ", paste(unique(all_udise$indicator_id), collapse = ", "))

# ── Coverage summary ───────────────────────────────────────────────────────────
all_udise |>
  group_by(indicator_id, label, level, gender) |>
  summarise(
    years    = n_distinct(year),
    min_year = min(year),
    max_year = max(year),
    .groups  = "drop"
  ) |>
  arrange(indicator_id, level, gender) |>
  print(n = 80)