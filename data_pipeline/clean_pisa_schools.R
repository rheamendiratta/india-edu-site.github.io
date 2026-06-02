# clean_pisa_schools.R
# Pre-computes survey-weighted country means for PISA school-level quantitative
# indicators using the `learningtower` package's built-in `school` dataset.
#
# Metrics (all weighted by sch_wgt):
#   SCH_STR         Student-teacher ratio (stratio)
#   SCH_STAFF_SHORT Staff shortage index (staff_shortage)
#   SCH_FUND_GOV    % of school funding from government (fund_gov)
#   SCH_PCT_PUBLIC  % of schools that are public (public_private == "public")
#
# Country codes are mapped to ISO3 using the same rules as pisa_means.rds.
# India is not in any PISA cycle — correctly absent from output.
#
# Output: data/clean/pisa_schools.rds
# Schema: plot · country_iso3 · year · value

library(learningtower)
library(dplyr)

CLEAN_OUT <- "data/clean"

PISA_REMAP <- c(
  "TAP" = "TWN",
  "KSV" = "XKX",
  "ROM" = "ROU"
)
PISA_SUBNATIONAL <- c(
  "QCN","QHP","QTN","QVE","QRS","QCH","QES","QAR",
  "QUC","QUE","QUD","QAZ","QCI","QMR","QRT","QUR","YUG"
)

df <- school |>
  filter(!is.na(sch_wgt), sch_wgt > 0) |>
  mutate(country_code = as.character(country)) |>
  filter(!country_code %in% PISA_SUBNATIONAL) |>
  mutate(
    country_iso3 = recode(country_code, !!!PISA_REMAP, .default = country_code),
    is_public    = as.integer(public_private == "public")
  )

wt_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 5) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

agg <- df |>
  group_by(year, country_iso3) |>
  summarise(
    SCH_STR         = wt_mean(stratio,      sch_wgt),
    SCH_STAFF_SHORT = wt_mean(staff_shortage, sch_wgt),
    SCH_FUND_GOV    = wt_mean(fund_gov,     sch_wgt),
    SCH_PCT_PUBLIC  = wt_mean(is_public,    sch_wgt) * 100,
    .groups = "drop"
  )

pisa_schools <- agg |>
  tidyr::pivot_longer(
    cols      = c(SCH_STR, SCH_STAFF_SHORT, SCH_FUND_GOV, SCH_PCT_PUBLIC),
    names_to  = "plot",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(year = as.integer(year)) |>
  arrange(plot, country_iso3, year)

message("Rows: ", nrow(pisa_schools))
message("India present: ", any(pisa_schools$country_iso3 == "IND"))
message("\nCoverage by plot:")
pisa_schools |>
  group_by(plot) |>
  summarise(countries = n_distinct(country_iso3), cycles = n_distinct(year), .groups = "drop") |>
  print()

saveRDS(pisa_schools, file.path(CLEAN_OUT, "pisa_schools.rds"))
message("Saved: data/clean/pisa_schools.rds")
