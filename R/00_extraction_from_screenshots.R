# 00_extraction_from_screenshots.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Independent re-extraction of pre/post means, SDs and n for the 14 primary
# studies, read from the author's 2020 screenshots of the primary-study tables
# (project folder screenshots/). One row per study x instrument.
# Writes data/extraction_reconciled.csv. Reconciliation notes and the
# per-study comparison with the corrigendum and the 2020 Rmd are in
# data/extraction_notes.md.
#
# Conventions
#   e = eCBT arm (electronic / remote delivery), c = face-to-face arm
#   values are copied verbatim from the screenshot; derived values are
#   computed below and marked in value_type
#   adj_diff = adjusted between-group difference at post, eCBT minus f2f,
#   in raw scale units (positive = eCBT higher score = favours face-to-face)

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# ---------------------------------------------------------------------------
# 1. verbatim screenshot values
# ---------------------------------------------------------------------------

raw <- tribble(
  ~study, ~instrument, ~rater, ~primary_instrument, ~ecbt_arm, ~f2f_arm, ~delivery,
  ~n_e, ~n_c, ~n_e_post, ~n_c_post,
  ~pre_e_m, ~pre_e_sd, ~pre_c_m, ~pre_c_sd,
  ~post_e_m, ~post_e_sd, ~post_c_m, ~post_c_sd,
  ~change_e_m, ~change_e_sd, ~change_c_m, ~change_c_sd,
  ~adj_diff, ~adj_diff_lb, ~adj_diff_ub, ~adj_ci_level,
  ~post_timepoint, ~sample, ~value_type, ~source_screenshot,

  # Luxton 2016, Table 3, ITT panel only (per-protocol panel not in screenshot)
  "Luxton 2016", "BDI-II", "self", TRUE, "in-home videoconference BATD", "in-person BATD", "videoconference",
  62, 59, 45, 42,
  27.60, 10.45, 29.71, 11.33,
  13.82, 12.02, 11.74, 12.08,
  NA, NA, NA, NA,
  4.23, 0.66, 7.81, 0.90,
  "post-treatment", "ITT frame, observed cases at each time point", "reported", "luxton_es.png",

  # Mohr 2012, Table 2, model-based means (95% CI), multiply imputed ITT
  "Mohr 2012", "Ham-D", "clinician", TRUE, "telephone CBT", "face-to-face CBT", "telephone",
  163, 162, 152, 141,
  22.83, NA, 22.83, NA,
  13.58, NA, 12.51, NA,
  NA, NA, NA, NA,
  1.07, -0.63, 2.76, 0.95,
  "end of treatment (week 18)", "ITT, mixed-model means with multiple imputation", "model-based mean, SD derived from 95% CI", "mohr.png",

  "Mohr 2012", "PHQ-9", "self", FALSE, "telephone CBT", "face-to-face CBT", "telephone",
  163, 162, 150, 136,
  16.76, NA, 16.76, NA,
  6.65, NA, 6.74, NA,
  NA, NA, NA, NA,
  -0.09, -1.35, 1.17, 0.95,
  "end of treatment (week 18)", "ITT, mixed-model means with multiple imputation", "model-based mean, SD derived from 95% CI", "mohr.png",

  # Choi 2014, Table 3, predicted means (SE); n from corrigendum (not in screenshot)
  "Choi 2014", "HAMD", "clinician", TRUE, "tele-PST (videoconference)", "in-person PST", "videoconference",
  56, 63, NA, NA,
  23.54, NA, 27.75, NA,
  13.68, NA, 14.08, NA,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "12-week follow-up (first post-baseline assessment)", "mixed-model predicted means, ITT", "model-based mean, SD = SE * sqrt(n)", "choi_table3.png",

  # Wagner 2014, Table 2; n not in screenshot, taken from corrigendum/2020 Rmd (agree)
  "Wagner 2014", "BDI", "self", TRUE, "internet CBT", "face-to-face CBT", "internet",
  32, 30, NA, NA,
  22.96, 6.07, 23.41, 7.63,
  12.41, 10.03, 12.33, 8.77,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "not stated in screenshot", "reported", "wagner.png",

  # Sethi 2013, Table 2; n not in screenshot (both sources 23/21)
  "Sethi 2013", "depression subscale (name not visible)", "self", TRUE, "MoodGYM", "face-to-face CBT", "internet",
  23, 21, NA, NA,
  17.65, 5.03, 21.42, 6.80,
  12.17, 3.12, 7.80, 3.09,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "not stated in screenshot", "reported", "sethi_2013_table2.png",

  # Sethi 2010, Table 1; n not in screenshot (both sources 9/10)
  "Sethi 2010", "depression subscale (name not visible)", "self", TRUE, "MoodGYM (online)", "face-to-face CBT", "internet",
  9, 10, NA, NA,
  16.4, 9.2, 19.0, 5.1,
  15.7, 4.2, 7.2, 3.1,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "not stated in screenshot", "reported", "sethi_2010_table1.png",

  # Wright 2005, Table 1: baseline + change scores (positive change = benefit);
  # NO post-treatment means or SDs are reported. Three change columns exist:
  # week 4 (completer), week 8 (completer), endpoint (ITT). Default row is
  # endpoint ITT; week 8 completer is a multiverse branch; week 4 is recorded
  # because the corrigendum mistook the week-4 BDI change for post-test means.
  "Wright 2005", "BDI", "self", TRUE, "computer-assisted CT", "standard CT", "computer-assisted (in clinic)",
  15, 15, NA, NA,
  31.4, 8.2, 24.4, 6.8,
  NA, NA, NA, NA,
  17.5, 10.8, 14.7, 8.0,
  NA, NA, NA, NA,
  "endpoint (ITT)", "ITT", "reported change scores; post mean derived as pre - change, post SD borrowed from change SD (imputed)", "wright pre.png, wright_post.png",

  "Wright 2005", "HAM-D", "clinician", FALSE, "computer-assisted CT", "standard CT", "computer-assisted (in clinic)",
  15, 15, NA, NA,
  16.6, 3.2, 17.1, 5.4,
  NA, NA, NA, NA,
  7.5, 6.3, 8.3, 6.6,
  NA, NA, NA, NA,
  "endpoint (ITT)", "ITT (df 2,41: one participant missing, arm unknown)", "reported change scores; post mean derived as pre - change, post SD borrowed from change SD (imputed)", "wright pre.png, wright_post.png",

  # Himelhoch 2013, Table 2
  "Himelhoch 2013", "HAM-D", "clinician", TRUE, "telephone CBT", "face-to-face CBT", "telephone",
  16, 18, 16, 18,
  22.6, 5.2, 24.6, 5.3,
  16.3, 8.6, 15.9, 7.2,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "n = 16/18 at every time point in the table", "reported", "himelhoch.png",

  "Himelhoch 2013", "QIDS-SR", "self", FALSE, "telephone CBT", "face-to-face CBT", "telephone",
  16, 18, 16, 18,
  16.3, 4.1, 17.0, 3.5,
  10.8, 5.5, 9.2, 3.7,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "n = 16/18 at every time point in the table", "reported", "himelhoch.png",

  # Poppelaars 2016, Table 2: T0 = pre-intervention, T9 = immediately post, T12 = 12-month FU
  "Poppelaars 2016", "RADS-2", "self", TRUE, "SPARX (computerised)", "OVK (school-based group CBT)", "computer game",
  51, 50, NA, NA,
  69.33, 8.37, 66.94, 7.09,
  57.88, 12.57, 59.33, 13.27,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "T9, immediately after the 8-week intervention", "descriptives; observed vs imputed not stated in screenshot", "reported", "poppelaars.png",

  # Glueckauf 2012, Table 3
  "Glueckauf 2012", "depression (instrument not visible)", "self", TRUE, "telephone CBT", "face-to-face CBT", "telephone",
  6, 5, 6, 5,
  12.67, 8.91, 11.80, 7.40,
  4.67, 2.07, 9.00, 5.70,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-treatment", "completers (pilot)", "reported", "glueckauf.png",

  # Kalapatapu 2014, Tables 1 and 2 (secondary analysis of Mohr 2012 subsample)
  "Kalapatapu 2014", "HAM-D", "clinician", TRUE, "telephone CBT", "face-to-face CBT", "telephone",
  50, 53, NA, NA,
  22.2, 4.7, 21.8, 3.8,
  12.8, 9.2, 11.8, 7.2,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "end of treatment (week 18)", "footnotes on week-18 n not visible in screenshot", "reported", "Kalapatapu_pre.png, Kalapatapu_post.png",

  "Kalapatapu 2014", "PHQ-9", "self", FALSE, "telephone CBT", "face-to-face CBT", "telephone",
  50, 53, NA, NA,
  16.8, 5.1, 16.0, 5.1,
  6.9, 7.2, 5.9, 5.4,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "end of treatment (week 18)", "footnotes on week-18 n not visible in screenshot", "reported", "Kalapatapu_pre.png, Kalapatapu_post.png",

  # Nelson 2003, text: 'calculated mean' from the group x time interaction
  "Nelson 2003", "depression (instrument not visible)", "self", TRUE, "videoconference CBT", "face-to-face CBT", "videoconference",
  14, 14, 14, 14,
  14.36, 9.85, 13.57, 8.75,
  6.71, 4.78, 11.64, 11.63,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post-test", "completers (Wilks lambda df 1,26 implies N = 28)", "reported (calculated means from ANOVA)", "nelson.png",

  # Kay-Lambkin 2009, Table 1: completers of all assessments; per-arm n not in screenshot
  "Kay-Lambkin 2009", "BDI-II", "self", TRUE, "computer-delivered CBT/MI (SHADE)", "therapist-delivered CBT/MI", "computer (clinic-based)",
  32, 35, NA, NA,
  28.57, 9.89, 34.91, 9.70,
  17.09, 12.14, 13.04, 10.51,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "3 months (end of 10-session treatment)", "completers of all assessments (table n = 67 across three arms); per-arm n 32/35 taken from corrigendum and are not verifiable from the screenshot", "reported", "kay-lambkin.png",

  # Andersson 2013, Table 2: observed means; per-arm n not in screenshot except HRSD post
  "Andersson 2013", "MADRS-S", "self", TRUE, "internet CBT", "group CBT", "internet",
  33, 36, NA, NA,
  23.6, 4.8, 24.1, 5.0,
  13.6, 9.8, 17.1, 8.0,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post (week 9)", "observed cases (total n 69 pre, 65 post); per-arm post split not in screenshot", "reported", "andersson.png",

  "Andersson 2013", "BDI", "self", FALSE, "internet CBT", "group CBT", "internet",
  33, 36, NA, NA,
  24.0, 7.0, 25.3, 6.6,
  13.6, 10.1, 17.9, 8.8,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post (week 9)", "observed cases (total n 69 pre, 65 post); per-arm post split not in screenshot", "reported", "andersson.png",

  "Andersson 2013", "HRSD", "clinician", FALSE, "internet CBT", "group CBT", "internet",
  33, 36, 29, 30,
  15.4, 3.8, 16.4, 5.0,
  6.3, 5.0, 8.1, 4.9,
  NA, NA, NA, NA,
  NA, NA, NA, NA,
  "post (week 9)", "observed cases; post n 29/30 given in table footnote", "reported", "andersson.png"
)

# Wright 2005 alternative time points (completer change scores), same
# baseline values. Kept in a separate table so the main table stays one row
# per study x instrument; 03_multiverse.R swaps them in for the
# wright_timepoint branch.
wright_alt <- tribble(
  ~study, ~instrument, ~timepoint, ~sample, ~change_e_m, ~change_e_sd, ~change_c_m, ~change_c_sd,
  "Wright 2005", "BDI",   "week 8 (completer)", "completer", 19.0, 10.1, 15.2, 7.1,
  "Wright 2005", "HAM-D", "week 8 (completer)", "completer",  8.2,  6.4,  8.4, 6.3,
  "Wright 2005", "BDI",   "week 4 (completer)", "completer", 10.7,  8.1,  9.7, 8.5,
  "Wright 2005", "HAM-D", "week 4 (completer)", "completer",  5.3,  4.8,  4.7, 5.8
)
write_csv(wright_alt, here("data", "wright_2005_change_by_timepoint.csv"))

# ---------------------------------------------------------------------------
# 2. Mohr 2012: 95% CIs of the model-based means (screenshot) -> SD
#    SD = sqrt(n) * (upper - lower) / (2 * qnorm(.975)). Baseline means are
#    constrained equal across arms, so the baseline SD is derived with the
#    total N; post SDs with the randomised (ITT) n of each arm.
# ---------------------------------------------------------------------------

mohr_ci <- tribble(
  ~instrument, ~pre_lb, ~pre_ub, ~post_e_lb, ~post_e_ub, ~post_c_lb, ~post_c_ub,
  "Ham-D", 22.34, 23.33, 12.42, 14.74, 11.22, 13.81,
  "PHQ-9", 16.24, 17.29, 5.72, 7.58, 5.74, 7.73
)

sd_from_ci <- function(lb, ub, n) sqrt(n) * (ub - lb) / (2 * qnorm(0.975))

mohr_sd <- mohr_ci |>
  mutate(
    study = "Mohr 2012",
    pre_sd = sd_from_ci(pre_lb, pre_ub, 163 + 162),
    post_e_sd_itt = sd_from_ci(post_e_lb, post_e_ub, 163),
    post_c_sd_itt = sd_from_ci(post_c_lb, post_c_ub, 162)
  )

# ---------------------------------------------------------------------------
# 3. Choi 2014: SE (screenshot) -> SD = SE * sqrt(n)
# ---------------------------------------------------------------------------

choi_se <- tibble(
  study = "Choi 2014",
  pre_e_se = 0.86, pre_c_se = 0.83, post_e_se = 1.00, post_c_se = 0.94
)

# ---------------------------------------------------------------------------
# 4. assemble
# ---------------------------------------------------------------------------

extraction <- raw |>
  left_join(mohr_sd |> select(study, instrument, pre_sd, post_e_sd_itt, post_c_sd_itt),
            by = c("study", "instrument")) |>
  left_join(choi_se, by = "study") |>
  mutate(
    pre_e_sd = case_when(
      study == "Mohr 2012" ~ round(pre_sd, 2),
      study == "Choi 2014" ~ round(pre_e_se * sqrt(n_e), 2),
      TRUE ~ pre_e_sd
    ),
    pre_c_sd = case_when(
      study == "Mohr 2012" ~ round(pre_sd, 2),
      study == "Choi 2014" ~ round(pre_c_se * sqrt(n_c), 2),
      TRUE ~ pre_c_sd
    ),
    post_e_sd = case_when(
      study == "Mohr 2012" ~ round(post_e_sd_itt, 2),
      study == "Choi 2014" ~ round(post_e_se * sqrt(n_e), 2),
      TRUE ~ post_e_sd
    ),
    post_c_sd = case_when(
      study == "Mohr 2012" ~ round(post_c_sd_itt, 2),
      study == "Choi 2014" ~ round(post_c_se * sqrt(n_c), 2),
      TRUE ~ post_c_sd
    ),
    # Wright: post mean derived from reported change; post SD borrowed from the
    # change-score SD and flagged as imputed (extraction rule adopted 2026-09-10)
    post_e_m = if_else(study == "Wright 2005", pre_e_m - change_e_m, post_e_m),
    post_c_m = if_else(study == "Wright 2005", pre_c_m - change_c_m, post_c_m),
    post_e_sd = if_else(study == "Wright 2005", change_e_sd, post_e_sd),
    post_c_sd = if_else(study == "Wright 2005", change_c_sd, post_c_sd),
    post_sd_imputed = study == "Wright 2005",
    # usability flags for the multiverse
    usable_post_smd = !is.na(post_e_sd) & !is.na(post_c_sd),
    usable_post_smd_reported_sd = usable_post_smd & !post_sd_imputed,
    usable_smc = !is.na(pre_e_sd) & (!is.na(post_e_m) | !is.na(change_e_m)),
    usable_change_smd_reported_sd = !is.na(change_e_sd),
    usable_adjusted = !is.na(adj_diff)
  ) |>
  select(-pre_sd, -post_e_sd_itt, -post_c_sd_itt, -ends_with("_se"))

# corrigendum Table 1 comparison for the primary instruments
table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)

extraction <- extraction |>
  left_join(
    table1 |> transmute(
      study,
      corr_n_e = n_e, corr_n_c = n_c,
      corr_pre_e_m = pre_e_m, corr_pre_e_sd = pre_e_sd, corr_pre_c_m = pre_c_m, corr_pre_c_sd = pre_c_sd,
      corr_post_e_m = post_e_m, corr_post_e_sd = post_e_sd, corr_post_c_m = post_c_m, corr_post_c_sd = post_c_sd
    ),
    by = "study"
  ) |>
  mutate(
    matches_corrigendum = case_when(
      !primary_instrument ~ NA,
      TRUE ~ near(n_e, corr_n_e) & near(n_c, corr_n_c) &
        near(pre_e_m, corr_pre_e_m, 0.011) & near(pre_e_sd, corr_pre_e_sd, 0.06) &
        near(pre_c_m, corr_pre_c_m, 0.011) & near(pre_c_sd, corr_pre_c_sd, 0.06) &
        near(post_e_m, corr_post_e_m, 0.011) & near(post_e_sd, corr_post_e_sd, 0.06) &
        near(post_c_m, corr_post_c_m, 0.011) & near(post_c_sd, corr_post_c_sd, 0.06)
    ),
    matches_corrigendum = replace_na(matches_corrigendum, FALSE) & primary_instrument
  ) |>
  select(-starts_with("corr_"))

write_csv(extraction, here("data", "extraction_reconciled.csv"), na = "")

cat("Rows:", nrow(extraction), " studies:", n_distinct(extraction$study), "\n")
print(extraction |> filter(primary_instrument) |>
        select(study, instrument, matches_corrigendum, usable_post_smd, usable_smc), n = Inf)
