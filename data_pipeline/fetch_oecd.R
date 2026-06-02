# fetch_oecd.R
# Fetches 5 OECD Education at a Glance indicators via SDMX REST API.
# Agency: OECD.EDU.IMEP  Base: https://sdmx.oecd.org/public/rest/data
#
# Confirmed dataflows and dimension orders (derived from DSD, not guessed):
#
#  #276 DF_LSO_NEAC_ALL      (17 dims): REF_AREA·SEX·AGE·ATTAINMENT_LEV·EDUCATION_FIELD·
#                                        MEASURE·INCOME·BIRTH_PLACE·MIGRATION_AGE·EDU_STATUS·
#                                        LABOUR_FORCE_STATUS·DURATION_UNEMP·UNIT_MEASURE·
#                                        STATISTICAL_OPERATION·WORK_TIME_ARNGMNT·QUESTIONNAIRE·FREQ
#         India absent — NEAC is national LFS data; India does not report.
#
#  #277 DF_UOE_INDIC_FIN_PERSTUD (9 dims): REF_AREA·MEASURE·EDUCATION_LEV·EXP_SOURCE·
#                                           EXP_DESTINATION·EXPENDITURE_TYPE·PRICE_BASE·
#                                           UNIT_MEASURE·Q_SHEET
#         India present: 385 non-null obs (S13=government sector), 2010-2021.
#
#  #278 DF_TCH_REL              (8 dims): REF_AREA·MEASURE·UNIT_MEASURE·INST_TYPE_EDU·
#                                          EDUCATION_LEV·AGE·SEX·PERS_TYPE
#         India absent — all rows OBS_STATUS=L (not collected). 34 OECD members only.
#
#  #279 DF_EAG_IT_ISCED         (7 dims): REF_AREA·MEASURE·UNIT_MEASURE·INST_TYPE_EDU·
#                                          EDUCATION_LEV·AGE·SUBJ_TYPE
#         India absent — all rows OBS_STATUS=Not applicable. 125 countries (UOE reporting).
#
#  #280 DF_UOE_NF_PERS_STR     (15 dims): REF_AREA·EDUCATION_LEV·MEASURE·EDUCATION_TYPE·
#                                           INTENSITY·EDUCATION_FIELD·GRADE·FREQ·ORIGIN·
#                                           DESTINATION·INST_TYPE_EDU·MOBILITY·UNIT_MEASURE·
#                                           SEX·AGE
#         India absent — NoRecordsFound. 45 OECD+partner countries.
#
# Output: data/raw/oecd/{276,277,278,279,280}_*.csv

library(httr2)
library(readr)
library(dplyr)
library(fs)

OECD_BASE <- "https://sdmx.oecd.org/public/rest/data"
OECD_OUT  <- "data/raw/oecd"
dir_create(OECD_OUT)

fetch_flow <- function(flow_id, key, outfile, label) {
  url <- paste0(OECD_BASE, "/OECD.EDU.IMEP,", flow_id, "/", key)
  message("Fetching #", label, " (", flow_id, ") ...")

  resp <- tryCatch(
    request(url) |>
      req_url_query(
        startPeriod              = "2010",
        dimensionAtObservation   = "AllDimensions",
        format                   = "csvfilewithlabels"
      ) |>
      req_timeout(300) |>
      req_perform(),
    error = function(e) { message("  HTTP error: ", e$message); NULL }
  )

  if (is.null(resp)) return(invisible(NULL))

  body <- resp_body_string(resp)
  if (nchar(body) < 10 || startsWith(trimws(body), "NoRecords")) {
    message("  No data returned.")
    return(invisible(NULL))
  }

  write_file(body, outfile)
  df <- suppressMessages(read_csv(outfile, show_col_types = FALSE))
  n   <- sum(!is.na(df$OBS_VALUE))
  ctry <- n_distinct(df$REF_AREA[!is.na(df$OBS_VALUE)])
  ind  <- any(df$REF_AREA[!is.na(df$OBS_VALUE)] == "IND", na.rm = TRUE)
  message("  Saved: ", outfile)
  message("  -> ", n, " non-NA obs | ", ctry, " countries | India present: ", ind)
  invisible(df)
}

# ── #276  Tertiary attainment, adults 25–34 ────────────────────────────────────
# Key: all countries, SEX=_T (total), AGE=Y25T34, ATTAINMENT_LEV=ISCED11A_6T8,
#      all other dims wildcarded. Filter in clean to MEASURE=POP, STATISTICAL_OPERATION=OBS.
# NOTE: India absent (no NEAC reporting).
fetch_flow(
  flow_id = "DSD_EAG_LSO_EA@DF_LSO_NEAC_ALL",
  key     = "._T.Y25T34.ISCED11A_6T8.................",
  outfile = file.path(OECD_OUT, "276_attainment_ter.csv"),
  label   = "276"
)

# ── #277  Expenditure per student (USD PPP) ───────────────────────────────────
# Key: all countries, MEASURE=FIN_PERSTUD, 4 main ISCED levels, all EXP_SOURCE,
#      EXP_DESTINATION=INST_EDU, EXPENDITURE_TYPE=DIR_EXP, UNIT_MEASURE=USD_PPP_ST.
# Filter in clean to EXP_SOURCE=S13 (government) — the only source available for India.
fetch_flow(
  flow_id = "DSD_EAG_UOE_FIN@DF_UOE_INDIC_FIN_PERSTUD",
  key     = ".FIN_PERSTUD.ISCED11_1+ISCED11_2+ISCED11_34+ISCED11_5T8...INST_EDU.DIR_EXP..USD_PPP_ST._Z",
  outfile = file.path(OECD_OUT, "277_exp_perstud.csv"),
  label   = "277"
)

# ── #278  Teachers' actual salaries relative to similarly-educated workers ────
# Key: all countries, MEASURE=SAL_ACT_REL_SIM, all units, public institutions,
#      all ISCED levels, AGE=Y25T64, SEX=_T, PERS_TYPE=TE (teacher).
# NOTE: India absent (not collected).
fetch_flow(
  flow_id = "DSD_EAG_SAL_ACT@DF_TCH_REL",
  key     = ".SAL_ACT_REL_SIM..INST_EDU_PUB..Y25T64._T.TE",
  outfile = file.path(OECD_OUT, "278_sal_tchr.csv"),
  label   = "278"
)

# ── #279  Instruction time (hours per year) ───────────────────────────────────
# Key: all countries, MEASURE=INT_TIME, UNIT_MEASURE=H_Y, public institutions,
#      all ISCED levels, AGE=_Z (not applicable for compulsory levels), SUBJ_TYPE=_T.
# NOTE: India absent (not applicable).
fetch_flow(
  flow_id = "DSD_EAG_IT@DF_EAG_IT_ISCED",
  key     = ".INT_TIME.H_Y.INST_EDU_PUB...._Z._T",
  outfile = file.path(OECD_OUT, "279_instr_time.csv"),
  label   = "279"
)

# ── #280  Student-teacher ratio (public institutions) ─────────────────────────
# Key: all countries, all ISCED levels, MEASURE=STU_PERS, formal education,
#      total intensity, no field filter, no grade, annual, public institutions,
#      UNIT_MEASURE=ST_TCHR (students per teacher), SEX=_T, AGE=_T.
# NOTE: India absent (not in UOE personnel dataset).
fetch_flow(
  flow_id = "DSD_EAG_UOE_NON_FIN_PERS@DF_UOE_NF_PERS_STR",
  key     = "..STU_PERS.FE._T._Z._Z.A._Z._Z.INST_EDU_PUB._Z.ST_TCHR._T._T",
  outfile = file.path(OECD_OUT, "280_str.csv"),
  label   = "280"
)

message("\nDone. Raw files in data/raw/oecd/")
