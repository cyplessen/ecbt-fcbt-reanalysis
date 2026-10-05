# 05_report_numbers.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Writes report/numbers.tex: every number quoted in the JRR Robustness Report
# as a LaTeX macro, read from the output tables of scripts 01 to 03. The
# manuscript (report/robustness_report.tex) uses only these macros, so the
# text cannot drift from the analysis. Re-run after any change to 00 to 03.
#
# Sign convention: positive favours face-to-face CBT (as in 02 and 03).

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

f2 <- function(x) sprintf("%.2f", x)
neg <- function(x) sub("^-", "$-$", x)  # typographic minus outside math

mv <- read_csv(here("output", "tables", "03_multiverse_results.csv"), show_col_types = FALSE)
pooled02 <- read_csv(here("output", "tables", "02_pooled_estimates.csv"), show_col_types = FALSE)
rep01 <- read_csv(here("output", "tables", "01_reproduction_pooled.csv"), show_col_types = FALSE)
sesd <- read_csv(here("output", "tables", "01_se_as_sd_check.csv"), show_col_types = FALSE)
arm_d <- c(sesd$fig3_sd_e - sesd$se_change_e, sesd$fig3_sd_c - sesd$se_change_c)
published <- read_csv(here("output", "tables", "03_published_estimates_on_our_axis.csv"), show_col_types = FALSE)

pick02 <- function(analysis, model, subset = "all 14") {
  pooled02 |> filter(.data$analysis == !!analysis, .data$model == !!model, .data$subset == !!subset)
}
t1_reml <- pick02("post-test Hedges g", "REML")
t1_dl <- pick02("post-test Hedges g", "DL")
no_mg_t1 <- pooled02 |> filter(analysis == "post-test Hedges g", model == "DL", subset == "without Sethi 2010 and Sethi 2013")
stopifnot(nrow(no_mg_t1) == 1)
stopifnot(nrow(t1_reml) == 1)

primary <- mv |> filter(data_source == "reconciled", es_metric == "post_smd", instrument == "primary",
                        inclusion == "all_14", ma_method == "reml", wright_post == "derived_borrowed_sd",
                        wright_timepoint == "endpoint_itt")
primary_hksj <- mv |> filter(data_source == "reconciled", es_metric == "post_smd", instrument == "primary",
                             inclusion == "all_14", ma_method == "reml_hksj", wright_post == "derived_borrowed_sd",
                             wright_timepoint == "endpoint_itt")
stopifnot(nrow(primary) == 1, nrow(primary_hksj) == 1)

no_mg <- mv |> filter(inclusion == "no_moodgym")
by_factor_spread <- map_dbl(c("data_source", "es_metric", "instrument", "ma_method", "wright_post", "wright_timepoint"),
                            \(v) mv |> group_by(.data[[v]]) |> summarise(m = median(estimate)) |> pull(m) |> range() |> diff())

rev01 <- rep01 |> filter(model == "DL, RevMan variance")
orig <- rev01 |> filter(input == "original_fig3_2020")
corr <- rev01 |> filter(input == "corrigendum_fig3_2021")
addm <- rev01 |> filter(input == "original_fig3_2020_without_choi")
stopifnot(nrow(orig) == 1, nrow(corr) == 1, nrow(addm) == 1)

kr <- read_csv(here("output", "tables", "07_kriston_reproduction.csv"), show_col_types = FALSE)
kr_dl <- kr |> filter(input == "Fig 1 as printed", variance == "RevMan", estimator == "DL")
kr_hk <- kr |> filter(input == "Fig 1 as printed", estimator == "REML + HKSJ")
stopifnot(nrow(kr_dl) == 1, nrow(kr_hk) == 1)
kr_in_mv <- mv |> filter(data_source == "kriston", es_metric == "chg_smd_r0.3", instrument == "primary", inclusion == "all_14", ma_method == "dl")
stopifnot(nrow(kr_in_mv) == 1)
t1_dl_i2 <- pooled02 |> filter(analysis == "post-test Hedges g", model == "DL", subset == "all 14") |> pull(i2_q)
n_ci_excl_f2f <- sum(mv$ci_lb > 0)
src <- read_csv(here("data", "source_comparison.csv"), show_col_types = FALSE)

mine_kr <- mv |> filter(data_source == "reconciled", es_metric == "chg_smd_r0.3", instrument == "primary", inclusion == "all_14", ma_method == "dl", wright_timepoint == "endpoint_itt")
mine_post <- mv |> filter(data_source == "reconciled", es_metric == "post_smd", instrument == "primary", inclusion == "all_14", ma_method == "dl", wright_post == "derived_borrowed_sd", wright_timepoint == "endpoint_itt")
stopifnot(nrow(mine_kr) == 1, nrow(mine_post) == 1)

macros <- c(
  # Kriston et al. 2022 (their sign: positive favours eCBT; flipped where marked)
  krPub = "-0.20", krPubLb = "-0.44", krPubUb = "0.05",
  krRep = f2(kr_dl$estimate_kriston_sign), krRepLb = f2(kr_dl$ci_lb_kriston_sign), krRepUb = f2(kr_dl$ci_ub_kriston_sign),
  krRepQ = sprintf("%.2f", kr_dl$q), krRepItwo = sprintf("%.0f", kr_dl$i2_q),
  krHkLb = f2(-kr_hk$ci_ub_kriston_sign), krHkUb = f2(-kr_hk$ci_lb_kriston_sign),  # our axis
  krOurAxis = f2(-kr_dl$estimate_kriston_sign), krOurAxisLb = f2(-kr_dl$ci_ub_kriston_sign), krOurAxisUb = f2(-kr_dl$ci_lb_kriston_sign),
  krInMv = f2(kr_in_mv$estimate), krInMvLb = f2(kr_in_mv$ci_lb), krInMvUb = f2(kr_in_mv$ci_ub),
  nSrcRows = as.character(nrow(src)),
  nKrAgreePost = as.character(sum(src$agree_post_kris_corr, na.rm = TRUE)),
  nKrAgreeN = as.character(sum(src$agree_n_kris_corr, na.rm = TRUE)),
  nOursAgreeKrPre = as.character(sum(src$agree_pre_ours_kris, na.rm = TRUE)),
  nOursAgreeKrPost = as.character(sum(src$agree_post_ours_kris, na.rm = TRUE)),
  tOneDlItwo = sprintf("%.0f", t1_dl_i2),
  nCiExclFtf = format(n_ci_excl_f2f, big.mark = ""),
  mvRange = f2(diff(range(mv$estimate))), mvIqr = f2(diff(quantile(mv$estimate, c(.25, .75)))),
  mdCorr = f2(median(mv$estimate[mv$data_source == "corrigendum"])),
  mdOurs = f2(median(mv$estimate[mv$data_source == "reconciled"])),
  mdKris = f2(median(mv$estimate[mv$data_source == "kriston"])),
  dsMaxShift = f2(diff(range(tapply(mv$estimate, mv$data_source, median)))),
  mvMaxAbs = f2(max(abs(mv$estimate))),
  nWithinHalfCorr = as.character(sum(abs(mv$estimate - abs(published$published_value[4])) < 0.5)),
  # my re-extraction, primary instrument, all 14, Wright endpoint ITT
  mineKr = f2(mine_kr$estimate), mineKrLb = f2(mine_kr$ci_lb), mineKrUb = f2(mine_kr$ci_ub),
  minePost = f2(mine_post$estimate), minePostLb = f2(mine_post$ci_lb), minePostUb = f2(mine_post$ci_ub),
  nArms = as.character(length(arm_d)),
  nArmsMatch = as.character(sum(abs(arm_d) <= 0.1)),
  nSpecs = format(nrow(mv), big.mark = ""),
  nCiZero = format(sum(!mv$ci_excludes_zero), big.mark = ""),
  nFavourEcbt = format(sum(mv$ci_ub < 0), big.mark = ""),
  mvMin = f2(min(mv$estimate)), mvMax = f2(max(mv$estimate)),
  mvMedian = f2(median(mv$estimate)),
  mvQa = f2(quantile(mv$estimate, .25)), mvQb = f2(quantile(mv$estimate, .75)),
  noMgMedian = f2(median(no_mg$estimate)), noMgMin = f2(min(no_mg$estimate)), noMgMax = f2(max(no_mg$estimate)),
  otherFactorMaxShift = f2(max(by_factor_spread)),
  tOneEst = f2(t1_reml$estimate), tOneLb = f2(t1_reml$ci_lb), tOneUb = f2(t1_reml$ci_ub),
  tOneDlEst = f2(t1_dl$estimate), tOneDlLb = f2(t1_dl$ci_lb), tOneDlUb = f2(t1_dl$ci_ub),
  noMgItwo = sprintf("%.0f", no_mg_t1$i2_q),
  pctCiZero = sprintf("%.1f", 100 * mean(!mv$ci_excludes_zero)),
  primaryEst = f2(primary$estimate), primaryLb = f2(primary$ci_lb), primaryUb = f2(primary$ci_ub),
  primaryHkLb = f2(primary_hksj$ci_lb), primaryHkUb = f2(primary_hksj$ci_ub),
  # reproduction of the published values (Luo's sign, as printed)
  repOrig = f2(orig$estimate), repOrigLb = f2(orig$ci_lb), repOrigUb = f2(orig$ci_ub), repOrigItwo = sprintf("%.0f", orig$i2),
  repCorr = f2(corr$estimate), repCorrLb = f2(corr$ci_lb), repCorrUb = f2(corr$ci_ub), repCorrItwo = sprintf("%.0f", corr$i2),
  repAddm = f2(addm$estimate), repAddmLb = f2(addm$ci_lb), repAddmUb = f2(addm$ci_ub),
  # published values as printed
  pubOrig = f2(published$published_value[1]), pubOrigLb = f2(published$pub_lb[1]), pubOrigUb = f2(published$pub_ub[1]),
  pubAddm = f2(published$published_value[3]), pubAddmLb = f2(published$pub_lb[3]), pubAddmUb = f2(published$pub_ub[3]),
  pubCorr = f2(published$published_value[4]), pubCorrLb = f2(published$pub_lb[4]), pubCorrUb = f2(published$pub_ub[4])
)

lines <- c(
  "% numbers.tex: generated by R/05_report_numbers.R from output/tables/. Do not edit.",
  sprintf("%% generated %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("\\newcommand{\\%s}{%s}", names(macros), unname(macros))
)
writeLines(lines, here("report", "numbers.tex"))
cat(length(macros), "macros written to report/numbers.tex\n")
print(macros[c("nSpecs", "mvMin", "mvMax", "mvMedian", "primaryEst", "noMgMedian", "otherFactorMaxShift", "repOrig", "repCorr", "repAddm")])
