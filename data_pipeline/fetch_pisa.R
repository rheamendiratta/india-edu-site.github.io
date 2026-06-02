# fetch_pisa.R
# Downloads PISA student microdata via the learningtower R package and computes
# survey-weighted country means per cycle × domain.
#
# Simplification note: learningtower exposes Plausible Value 1 for each domain
# (labelled "Plausible Value 1 in Mathematics/Reading/Science"). The standard
# OECD method averages across all 10 PVs and applies balanced-repeated-replication
# (BRR) for SEs; using PV1 alone slightly overstates sampling variance and can
# introduce small bias, but the country-mean point estimates are close (< 1 point
# difference for most countries). This simplification is appropriate for a
# dashboard bar/line chart.
#
# Saves: data/raw/pisa/pisa_wt_means_{year}.rds  (one per cycle, ~3 KB each)
#        data/raw/pisa/pisa_wt_means_all.rds      (bound table, ~200 rows)
#
# India is not in any PISA cycle and will be absent from all outputs — correct,
# not a bug. The India learning story comes from PARAKH/NAS on the India page.

library(learningtower)
library(dplyr)
library(fs)

PISA_OUT <- "data/raw/pisa"
dir_create(PISA_OUT)

PISA_CYCLES <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022)

aggregate_year <- function(yr) {
  out_path <- file.path(PISA_OUT, paste0("pisa_wt_means_", yr, ".rds"))
  if (file.exists(out_path)) {
    message("  Skipping ", yr, " (already cached at ", out_path, ")")
    return(readRDS(out_path))
  }

  message("Fetching ", yr, " student data...")
  df <- load_student(yr)
  message("  -> ", nrow(df), " students, ", n_distinct(df$country), " countries")

  agg <- df |>
    filter(!is.na(stu_wgt), stu_wgt > 0) |>
    group_by(year, country) |>
    summarise(
      mean_math = weighted.mean(math,    stu_wgt, na.rm = TRUE),
      mean_read = weighted.mean(read,    stu_wgt, na.rm = TRUE),
      mean_sci  = weighted.mean(science, stu_wgt, na.rm = TRUE),
      n_students = n(),
      .groups = "drop"
    ) |>
    # Flag domains with fewer than 10 non-missing students as NA.
    # (Some cycles had domain-rotated booklets; countries with very few valid
    # scores for a domain should not appear as if they have full coverage.)
    left_join(
      df |>
        filter(!is.na(stu_wgt), stu_wgt > 0) |>
        group_by(year, country) |>
        summarise(
          n_math = sum(!is.na(math)),
          n_read = sum(!is.na(read)),
          n_sci  = sum(!is.na(science)),
          .groups = "drop"
        ),
      by = c("year", "country")
    ) |>
    mutate(
      mean_math = if_else(n_math < 10, NA_real_, mean_math),
      mean_read = if_else(n_read < 10, NA_real_, mean_read),
      mean_sci  = if_else(n_sci  < 10, NA_real_, mean_sci)
    ) |>
    select(year, country, mean_math, mean_read, mean_sci, n_students)

  saveRDS(agg, out_path)
  message("  Saved: ", out_path, " (", nrow(agg), " country rows)")
  agg
}

all_years <- lapply(PISA_CYCLES, aggregate_year)
pisa_all  <- bind_rows(all_years)

message("\nAll cycles combined: ", nrow(pisa_all), " rows")
message("India present: ", any(as.character(pisa_all$country) == "IND"))
message("Unique PISA country codes: ", n_distinct(pisa_all$country))

saveRDS(pisa_all, file.path(PISA_OUT, "pisa_wt_means_all.rds"))
message("Saved: data/raw/pisa/pisa_wt_means_all.rds")
