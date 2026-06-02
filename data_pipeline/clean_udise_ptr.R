# clean_udise_ptr.R
# Cleans UDISE 2007 (Pupil-Teacher Ratio) for the World > Teachers app.
#
# Old files (ag-grid): row 4 = level headers, row 5 = All India values
#   Levels: Primary (I-V), Upper Primary (VI-VIII), Secondary (IX-X), Higher Secondary (XI-XII)
# New files (UDISE+): row 6 = level headers, row 7 = All India values
#   Levels: Foundational, Preparatory, Middle, Secondary
#
# Output: data/clean/udise_ptr.rds
# Schema: acad_year, year, level (NEP-normalised), wb_level, value

library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(fs)

UDISE_IN  <- "data/raw/udise/2007"
CLEAN_OUT <- "data/clean"

level_map <- c(
  "Primary (I-V)"             = "Preparatory",
  "Upper Primary (VI-VIII)"   = "Middle",
  "Secondary (IX-X)"          = "Secondary",
  "Higher Secondary (XI-XII)" = "Higher Secondary",
  "Foundational"              = "Foundational",
  "Preparatory"               = "Preparatory",
  "Middle"                    = "Middle",
  "Secondary"                 = "Secondary"
)

wb_level_ptr <- c(
  "Foundational"   = "Pre-primary",
  "Preparatory"    = "Primary",
  "Middle"         = "Secondary",
  "Secondary"      = "Secondary",
  "Higher Secondary" = "Secondary"
)

extract_year <- function(fp) as.integer(str_extract(basename(fp), "^\\d{4}"))
extract_acad <- function(fp) str_extract(basename(fp), "\\d{4}[-_]\\d{2,4}")

parse_ptr_old <- function(fp) {
  raw        <- read_excel(fp, sheet = "ag-grid", col_names = FALSE,
                           .name_repair = "minimal")
  header_row <- as.character(unlist(raw[4, ]))
  value_row  <- as.character(unlist(raw[5, ]))
  results    <- list()
  for (j in seq_along(header_row)) {
    lv  <- trimws(header_row[j])
    val <- suppressWarnings(as.numeric(value_row[j]))
    if (is.na(lv) || lv == "" || is.na(val)) next
    nep <- unname(level_map[lv])
    if (is.na(nep)) next
    results[[length(results) + 1]] <- tibble(level = nep, value = val)
  }
  bind_rows(results)
}

parse_ptr_new <- function(fp) {
  raw        <- read_excel(fp, sheet = "UDISE+", col_names = FALSE,
                           .name_repair = "minimal")
  header_row <- as.character(unlist(raw[6, ]))
  value_row  <- as.character(unlist(raw[7, ]))
  results    <- list()
  for (j in seq_along(header_row)) {
    lv  <- trimws(header_row[j])
    val <- suppressWarnings(as.numeric(value_row[j]))
    if (is.na(lv) || lv == "" || is.na(val)) next
    nep <- unname(level_map[lv])
    if (is.na(nep)) next
    results[[length(results) + 1]] <- tibble(level = nep, value = val)
  }
  bind_rows(results)
}

files <- dir_ls(UDISE_IN, glob = "*.xlsx")
message("Parsing ", length(files), " PTR files...")

results <- map(sort(files), function(fp) {
  sheets <- tryCatch(excel_sheets(fp), error = function(e) NULL)
  if (is.null(sheets)) { message("  Skip: ", basename(fp)); return(NULL) }
  acad <- extract_acad(fp)
  year <- extract_year(fp)
  df <- tryCatch({
    if ("UDISE+" %in% sheets) parse_ptr_new(fp)
    else if ("ag-grid" %in% sheets) parse_ptr_old(fp)
    else NULL
  }, error = function(e) { message("  Error: ", basename(fp), " - ", e$message); NULL })
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df |> mutate(acad_year = acad, year = year,
               wb_level = unname(wb_level_ptr[level]))
})

udise_ptr <- bind_rows(compact(results)) |>
  select(acad_year, year, level, wb_level, value) |>
  arrange(level, year)

message("PTR rows: ", nrow(udise_ptr))
print(udise_ptr |> count(level, wb_level))

saveRDS(udise_ptr, file.path(CLEAN_OUT, "udise_ptr.rds"))
message("Saved: data/clean/udise_ptr.rds")
