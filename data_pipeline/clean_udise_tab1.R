# clean_udise_tab1.R
# UDISE cleaning for World > Students — Tab 1: Enrolment & Access
#
# Indicators:
#   4010  GER by Gender and Level
#   4011  NER by Gender and Level
#   4012  Adjusted NER by Gender and Level   (ag-grid only; ends 2021-22)
#   4032  GPI of GER by Level                (no gender; ends 2021-22, resumes 2024-25)
#
# Dispatcher: route by sheet name × indicator_id.
#   ag-grid + 4010/4011/4012  → parse_old_ger_ner
#   UDISE+  + 4010/4011       → parse_nep_ger_ner
#   ag-grid + 4032            → parse_gpi_old
#   UDISE+  + 4032            → parse_gpi_new
#   anything else             → stop and report
#
# Output: data/clean/udise_tab1.rds
#
# Schema:
#   indicator_id  chr   "4010" / "4011" / "4012" / "4032"
#   acad_year     chr   "2012-13"  (NA for gap-break rows)
#   year          int   2012       (gap-break rows carry the gap year)
#   level         chr   NEP-normalised level name
#   wb_level      chr   matchable WB level, NA if no clean WB counterpart
#   gender        chr   "Female" / "Male" / "Total"  (NA for GPI — it is the ratio)
#   value         dbl   (NA for gap-break rows — forces visible line break in Plotly)
#   label         chr   human-readable series label

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(fs)

UDISE_IN  <- "data/raw/udise"
CLEAN_OUT <- "data/clean"

# ── Shared utilities ──────────────────────────────────────────────────────────

extract_year <- function(filepath) {
  str_extract(path_file(filepath), "\\d{4}-\\d{2,4}")
}

year_numeric <- function(acad_year) {
  as.integer(str_extract(acad_year, "^\\d{4}"))
}

# ── Level maps ────────────────────────────────────────────────────────────────

# Old UDISE names → NEP-normalised output names.
# "Elementary (I-VIII)" maps to "Elementary" then is dropped via DROP_LEVELS_T1
# (it is the combined I-VIII aggregate; no clean NEP one-to-one).
level_map_t1 <- c(
  "Primary (I-V)"             = "Preparatory",
  "Upper Primary (VI-VIII)"   = "Middle",
  "Elementary (I-VIII)"       = "Elementary",
  "Secondary (IX-X)"          = "Secondary",
  "Higher Secondary (XI-XII)" = "Higher Secondary",
  # NEP names pass through unchanged
  "Foundational"              = "Foundational",
  "Preparatory"               = "Preparatory",
  "Middle"                    = "Middle",
  "Secondary"                 = "Secondary",
  "Higher Secondary"          = "Higher Secondary"
)

DROP_LEVELS_T1 <- "Elementary"

# WB-matchable level for GER / NER / Adjusted NER (4010, 4011, 4012).
# Higher Secondary has no standalone WB GER/NER level (WB secondary = full IX-XII).
wb_level_enr <- c(
  "Foundational"     = NA_character_,
  "Preparatory"      = "Primary",
  "Middle"           = NA_character_,
  "Secondary"        = "Secondary",
  "Higher Secondary" = NA_character_
)

# WB-matchable level for GPI (4032).
# WB #146 SE.ENR.SECO.FM.ZS spans full secondary (ISCED 2+3 = IX-XII).
# Higher Secondary therefore folds into the Secondary match.
wb_level_gpi <- c(
  "Foundational"     = NA_character_,
  "Preparatory"      = "Primary",
  "Middle"           = NA_character_,
  "Secondary"        = "Secondary",
  "Higher Secondary" = "Secondary"
)

indicator_labels <- c(
  "4010" = "GER (UDISE)",
  "4011" = "NER (UDISE)",
  "4012" = "Adjusted NER (UDISE)",
  "4032" = "GPI of GER (UDISE)"
)

# ── Parser: old ag-grid GER/NER/Adj NER  (cols start at 2; row 4/5/6) ────────
# Used for: 4010 ag-grid, 4011 ag-grid, 4012 ag-grid (all identical layout)
# Col 1 is the "Location" label — skip it.
parse_old_ger_ner <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "ag-grid",
                           col_names = FALSE, .name_repair = "minimal")
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

    if (is.na(current_level))                                    next
    if (!gender %in% c("Girls", "Boys", "Overall"))              next
    if (is.na(val))                                              next
    if (str_detect(current_level, "^(SC|ST) "))                  next

    level_nep <- unname(level_map_t1[current_level])
    if (is.na(level_nep) || level_nep %in% DROP_LEVELS_T1)      next

    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_nep,
      wb_level     = unname(wb_level_enr[level_nep]),
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

# ── Parser: new UDISE+ GER/NER  (cols start at 1; rows 7/8/9) ────────────────
# Used for: 4010 UDISE+, 4011 UDISE+
# No leading Location column — level names begin at col 1 (sparse: cols 1,4,7,10).
# Fixes the col-2 start bug in the original pipeline (which silently dropped Foundational).
parse_nep_ger_ner <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "UDISE+",
                           col_names = FALSE, .name_repair = "minimal")
  level_row  <- as.character(unlist(raw[7, ]))
  gender_row <- as.character(unlist(raw[8, ]))
  value_row  <- as.character(unlist(raw[9, ]))
  n_cols     <- length(value_row)
  results    <- list()
  current_level <- NA_character_

  for (col_i in 1:n_cols) {
    lv <- level_row[col_i]
    if (!is.na(lv) && nchar(trimws(lv)) > 0) current_level <- trimws(lv)
    gender <- trimws(gender_row[col_i])
    val    <- suppressWarnings(as.numeric(value_row[col_i]))

    if (is.na(current_level))                                    next
    if (!gender %in% c("Girls", "Boys", "Overall"))              next
    if (is.na(val))                                              next

    level_nep <- unname(level_map_t1[current_level])
    if (is.na(level_nep) || level_nep %in% DROP_LEVELS_T1)      next

    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_nep,
      wb_level     = unname(wb_level_enr[level_nep]),
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

# ── Parser: old ag-grid GPI  (cols start at 2; row 4 headers / row 5 data) ───
# Used for: 4032 ag-grid
# Col 1 is "Location" label; data cols 2+ map directly to level names in row 4.
# No gender dimension — GPI is already the F/M ratio.
parse_gpi_old <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "ag-grid",
                           col_names = FALSE, .name_repair = "minimal")
  header_row <- as.character(unlist(raw[4, ]))
  value_row  <- as.character(unlist(raw[5, ]))
  results    <- list()

  for (col_i in 2:length(header_row)) {
    level_raw <- trimws(header_row[col_i])
    if (is.na(level_raw) || nchar(level_raw) == 0)               next
    val <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(val))                                               next

    level_nep <- unname(level_map_t1[level_raw])
    if (is.na(level_nep) || level_nep %in% DROP_LEVELS_T1)       next

    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_nep,
      wb_level     = unname(wb_level_gpi[level_nep]),
      gender       = NA_character_,
      value        = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Parser: new UDISE+ GPI  (cols start at 1; row 6 headers / row 7 data) ────
# Used for: 4032 UDISE+
# No leading Location column — levels begin at col 1 (contiguous, one per column).
parse_gpi_new <- function(filepath, indicator_id) {
  acad_year  <- extract_year(filepath)
  raw        <- read_excel(filepath, sheet = "UDISE+",
                           col_names = FALSE, .name_repair = "minimal")
  header_row <- as.character(unlist(raw[6, ]))
  value_row  <- as.character(unlist(raw[7, ]))
  results    <- list()

  for (col_i in seq_along(header_row)) {
    level_raw <- trimws(header_row[col_i])
    if (is.na(level_raw) || nchar(level_raw) == 0)               next
    val <- suppressWarnings(as.numeric(value_row[col_i]))
    if (is.na(val))                                               next

    level_nep <- unname(level_map_t1[level_raw])
    if (is.na(level_nep) || level_nep %in% DROP_LEVELS_T1)       next

    results[[length(results) + 1]] <- tibble(
      indicator_id = indicator_id,
      acad_year    = acad_year,
      year         = year_numeric(acad_year),
      level        = level_nep,
      wb_level     = unname(wb_level_gpi[level_nep]),
      gender       = NA_character_,
      value        = val
    )
  }
  if (length(results) == 0) { warning("No data: ", filepath); return(NULL) }
  bind_rows(results)
}

# ── Dispatcher ────────────────────────────────────────────────────────────────
dispatch_file <- function(filepath, indicator_id) {
  sheets <- excel_sheets(filepath)

  if (length(sheets) != 1 || !sheets %in% c("ag-grid", "UDISE+")) {
    stop(sprintf(
      "Unexpected sheet(s) in %s: [%s] — manual inspection required",
      basename(filepath), paste(sheets, collapse = ", ")
    ))
  }

  sheet  <- sheets[1]
  is_gpi <- indicator_id == "4032"

  if      (sheet == "ag-grid" && !is_gpi) parse_old_ger_ner(filepath, indicator_id)
  else if (sheet == "ag-grid" &&  is_gpi) parse_gpi_old(filepath, indicator_id)
  else if (sheet == "UDISE+"  && !is_gpi) parse_nep_ger_ner(filepath, indicator_id)
  else if (sheet == "UDISE+"  &&  is_gpi) parse_gpi_new(filepath, indicator_id)
}

# ── Process one indicator folder ──────────────────────────────────────────────
process_indicator_t1 <- function(indicator_id) {
  folder <- file.path(UDISE_IN, indicator_id)
  if (!dir_exists(folder)) { warning("Folder not found: ", folder); return(NULL) }
  files  <- dir_ls(folder, glob = "*.xlsx")
  if (length(files) == 0) { warning("No xlsx in: ", folder); return(NULL) }

  message("\nProcessing ", indicator_id, " (", length(files), " files) ...")

  results <- map(sort(files), function(f) {
    yr <- extract_year(f)
    message("  ", yr, "  ", path_file(f))
    tryCatch(
      dispatch_file(f, indicator_id),
      error = function(e) { message("  ✗ ", path_file(f), ": ", e$message); NULL }
    )
  })

  out <- bind_rows(compact(results))
  message("  ✓ ", nrow(out), " rows across ", n_distinct(out$year), " years")
  out
}

# ── Run all four indicators ───────────────────────────────────────────────────
raw_4010 <- process_indicator_t1("4010")
raw_4011 <- process_indicator_t1("4011")
raw_4012 <- process_indicator_t1("4012")
raw_4032 <- process_indicator_t1("4032")

# ── Insert gap-break rows for 4032 (2022-23 and 2023-24 files missing) ────────
# The 4032 series runs 2012-2021 then jumps to 2024.  A Plotly line chart would
# connect the gap with a straight segment — NA rows for 2022 and 2023 prevent
# that and show an honest visible break.  Only levels present on both sides of
# the gap receive gap rows; levels that terminate or begin within the gap
# (Higher Secondary ends at 2021; Foundational begins at 2024) get no gap rows.
if (!is.null(raw_4032) && nrow(raw_4032) > 0) {
  levels_before <- raw_4032 |> filter(year == 2021) |> pull(level) |> unique()
  levels_after  <- raw_4032 |> filter(year == 2024) |> pull(level) |> unique()
  gap_levels    <- intersect(levels_before, levels_after)

  if (length(gap_levels) > 0) {
    gap_rows <- expand_grid(level = gap_levels, year = c(2022L, 2023L)) |>
      mutate(
        indicator_id = "4032",
        acad_year    = NA_character_,
        wb_level     = unname(wb_level_gpi[level]),
        gender       = NA_character_,
        value        = NA_real_
      )
    raw_4032 <- bind_rows(raw_4032, gap_rows) |> arrange(level, year)
    message("\nInserted ", nrow(gap_rows), " gap-break rows for 4032 ",
            "(years 2022-2023, levels: ", paste(gap_levels, collapse = ", "), ")")
  }
}

# ── Add labels and combine ────────────────────────────────────────────────────
all_tab1 <- bind_rows(raw_4010, raw_4011, raw_4012, raw_4032) |>
  mutate(
    indicator_id = as.character(indicator_id),
    label        = unname(indicator_labels[indicator_id])
  ) |>
  arrange(indicator_id, level, gender, year)

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(all_tab1, file.path(CLEAN_OUT, "udise_tab1.rds"))
message("\n── Tab 1 UDISE cleaning complete ──")
message("Rows (incl. gap-break rows): ", nrow(all_tab1))
message("Rows with data:              ", sum(!is.na(all_tab1$value)))

# ── Coverage summary (data rows only) ────────────────────────────────────────
all_tab1 |>
  filter(!is.na(value)) |>
  group_by(indicator_id, label, level, gender) |>
  summarise(
    n_years  = n_distinct(year),
    min_year = min(year),
    max_year = max(year),
    .groups  = "drop"
  ) |>
  arrange(indicator_id, level, gender) |>
  print(n = 80)
