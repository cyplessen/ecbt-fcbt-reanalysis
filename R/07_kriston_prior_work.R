# 07_kriston_prior_work.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Prior work: Kriston, Liebherz, Koehnen (2022), eClinicalMedicine 54:101763,
# doi 10.1016/j.eclinm.2022.101763. Their supplement (pdf/kriston_supplement.xlsx)
# is transcribed verbatim to data/kriston_supplement_s1_extraction.csv (S1:
# per-parameter re-extraction by two reviewers, consensus, corrigendum value,
# deviation, source page) and data/kriston_supplement_s4_analysis.csv (S4:
# arm-level inputs and per-study effect sizes). Their Fig 1 inputs as printed
# are in data/kriston_fig1_as_printed.csv.
#
# This script
#   1. reproduces their pooled estimate from (a) their Fig 1 inputs and
#      (b) their S4 per-study columns, under DL and REML, with RevMan's and
#      metafor's SMD variance; writes output/tables/07_kriston_reproduction.csv
#   2. builds data/source_comparison.csv: one row per trial x arm, the five
#      data sources side by side (corrigendum Table 1; Kriston S4; our reconciled
#      extraction; original Fig 3 implied; corrected Fig 3 implied), per-cell
#      agreement flags and a reason column
#   3. writes report/table_sources.tex, a compact version for the report
#
# Kriston et al.'s effect size: change-score SMD, change = pre - post per arm,
# SD_change = sqrt(SD_pre^2 + SD_post^2 - 2 r SD_pre SD_post) with r = 0.3,
# pooled SD, J = 1 - 3/(4N - 9), variance N/(n1 n2) + g^2/(2(N - 3.94))
# (RevMan), eCBT minus face-to-face so POSITIVE FAVOURS eCBT. Everything in
# THIS script's outputs is flipped to our convention: positive favours
# face-to-face CBT.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
})

sign_convention <- "positive favours face-to-face CBT"

# ---------------------------------------------------------------------------
# 1. reproduce Kriston et al. Fig 1: -0.20 [-0.44, 0.05], tau2 0.14,
#    Chi2 41.88, df 13, I2 69%, Z 1.54 (their sign: positive favours eCBT)
# ---------------------------------------------------------------------------

fig1 <- read_csv(here("data", "kriston_fig1_as_printed.csv"), show_col_types = FALSE)
s4 <- read_csv(here("data", "kriston_supplement_s4_analysis.csv"), show_col_types = FALSE)

revman_g <- function(m1, sd1, n1, m2, sd2, n2) {
  N <- n1 + n2
  sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (N - 2))
  g <- (m1 - m2) / sp * (1 - 3 / (4 * N - 9))
  list(g = g, v = N / (n1 * n2) + g^2 / (2 * (N - 3.94)))
}
row_of <- function(fit, input, es, variance) tibble(
  input = input, effect_size = es, variance = variance, estimator = fit$method,
  k = fit$k, estimate_kriston_sign = as.numeric(coef(fit)), ci_lb_kriston_sign = fit$ci.lb, ci_ub_kriston_sign = fit$ci.ub,
  tau2 = fit$tau2, q = fit$QE, i2_q = 100 * max(0, (fit$QE - (fit$k - 1)) / fit$QE), z = fit$zval
)

rm1 <- with(fig1, revman_g(m_e, sd_e, n_e, m_c, sd_c, n_c))
es_mf <- escalc("SMD", m1i = m_e, sd1i = sd_e, n1i = n_e, m2i = m_c, sd2i = sd_c, n2i = n_c, data = fig1)
reproduction <- bind_rows(
  row_of(rma(rm1$g, rm1$v, method = "DL"), "Fig 1 as printed", "RevMan J = 1 - 3/(4N-9)", "RevMan"),
  row_of(rma(rm1$g, rm1$v, method = "REML"), "Fig 1 as printed", "RevMan J", "RevMan"),
  row_of(rma(rm1$g, rm1$v, method = "REML", test = "knha"), "Fig 1 as printed", "RevMan J", "RevMan") |> mutate(estimator = "REML + HKSJ"),
  row_of(rma(es_mf$yi, es_mf$vi, method = "DL"), "Fig 1 as printed", "metafor exact J", "metafor LS"),
  row_of(rma(s4$es_g, s4$es_se^2, method = "DL"), "S4 per-study g and SE (full precision)", "RevMan J", "RevMan"),
  row_of(rma(s4$es_g, s4$es_se^2, method = "REML"), "S4 per-study g and SE (full precision)", "RevMan J", "RevMan")
) |>
  mutate(published = "-0.20 [-0.44, 0.05], tau2 0.14, Chi2 41.88, I2 69%, Z 1.54",
         reproduces_at_2dp = round(estimate_kriston_sign, 2) == -0.20 & round(ci_lb_kriston_sign, 2) == -0.44 &
           round(ci_ub_kriston_sign, 2) == 0.05 & round(q, 2) == 41.88)
write_csv(reproduction, here("output", "tables", "07_kriston_reproduction.csv"))

per_study_check <- tibble(study = fig1$study, printed = c(0.34, -0.47, 0.58, -0.29, -0.07, -0.81, -0.31, -0.13, 0.50, 0.29, -1.48, -1.37, -0.05, 0.29),
                          recomputed = round(rm1$g, 2)) |> mutate(match = printed == recomputed)
stopifnot(all(per_study_check$match))

cat("== Kriston et al. 2022 reproduction (their sign, positive favours eCBT) ==\n")
print(reproduction |> transmute(input, variance, estimator, est = round(estimate_kriston_sign, 4), lb = round(ci_lb_kriston_sign, 4),
                               ub = round(ci_ub_kriston_sign, 4), tau2 = round(tau2, 4), q = round(q, 2), i2 = round(i2_q), reproduces_at_2dp),
      width = Inf)
cat("per-study printed vs recomputed: all 14 match at 2 dp\n")

# ---------------------------------------------------------------------------
# 2. source comparison, one row per trial x arm
# ---------------------------------------------------------------------------

table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)
recon <- read_csv(here("data", "extraction_reconciled.csv"), show_col_types = FALSE) |> filter(primary_instrument)
fig3_orig <- read_csv(here("data", "luo_fig3_original_2020.csv"), show_col_types = FALSE)
fig3_corr <- read_csv(here("data", "luo_fig3_corrigendum.csv"), show_col_types = FALSE)

long_arms <- function(d, src, n_e, n_c, pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd) {
  bind_rows(
    d |> transmute(study, arm = "eCBT", source = src, n = {{ n_e }}, pre_m = {{ pre_e_m }}, pre_sd = {{ pre_e_sd }}, post_m = {{ post_e_m }}, post_sd = {{ post_e_sd }}),
    d |> transmute(study, arm = "face-to-face", source = src, n = {{ n_c }}, pre_m = {{ pre_c_m }}, pre_sd = {{ pre_c_sd }}, post_m = {{ post_c_m }}, post_sd = {{ post_c_sd }})
  )
}
src_corr <- long_arms(table1, "corrigendum", n_e, n_c, pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd) |>
  mutate(timepoint = "post-treatment as extracted by the authors", sample = "randomised n")
src_kris <- long_arms(s4, "kriston", n_e, n_c, pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd) |>
  mutate(timepoint = "post-treatment; Wright: post derived from endpoint ITT change, SD back-solved with r = 0.3",
         sample = "analysed n where reported (completers), else randomised")
src_ours <- long_arms(recon, "reconciled", n_e, n_c, pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd) |>
  left_join(recon |> transmute(study, timepoint = post_timepoint, sample), by = "study")
fig3_long <- function(f, src) bind_rows(
  f |> transmute(study, arm = "eCBT", source = src, n = ecbt_n, change_m = ecbt_mean, sem_change = ecbt_sd),
  f |> transmute(study, arm = "face-to-face", source = src, n = cbt_n, change_m = cbt_mean, sem_change = cbt_sd))
src_f3o <- fig3_long(fig3_orig, "fig3_original")
src_f3c <- fig3_long(fig3_corr, "fig3_corrigendum")

wide <- src_corr |> select(study, arm, n_corr = n, pre_m_corr = pre_m, pre_sd_corr = pre_sd, post_m_corr = post_m, post_sd_corr = post_sd) |>
  full_join(src_kris |> select(study, arm, n_kris = n, pre_m_kris = pre_m, pre_sd_kris = pre_sd, post_m_kris = post_m, post_sd_kris = post_sd), by = c("study", "arm")) |>
  full_join(src_ours |> select(study, arm, n_ours = n, pre_m_ours = pre_m, pre_sd_ours = pre_sd, post_m_ours = post_m, post_sd_ours = post_sd,
                               timepoint_ours = timepoint, sample_ours = sample), by = c("study", "arm")) |>
  full_join(src_f3o |> select(study, arm, n_fig3_orig = n, change_m_fig3_orig = change_m, sem_fig3_orig = sem_change), by = c("study", "arm")) |>
  full_join(src_f3c |> select(study, arm, n_fig3_corr = n, change_m_fig3_corr = change_m, sem_fig3_corr = sem_change), by = c("study", "arm")) |>
  mutate(change_m_corr = pre_m_corr - post_m_corr, change_m_kris = pre_m_kris - post_m_kris, change_m_ours = pre_m_ours - post_m_ours,
         sem_change_corr = sqrt(pre_sd_corr^2 / n_corr + post_sd_corr^2 / n_corr))

near2 <- function(a, b, tol) ifelse(is.na(a) | is.na(b), NA, abs(a - b) <= tol)
flags <- wide |> transmute(
  study, arm,
  agree_n_kris_corr = near2(n_kris, n_corr, 0), agree_n_ours_corr = near2(n_ours, n_corr, 0), agree_n_ours_kris = near2(n_ours, n_kris, 0),
  agree_pre_kris_corr = near2(pre_m_kris, pre_m_corr, 0.011) & near2(pre_sd_kris, pre_sd_corr, 0.06),
  agree_pre_ours_corr = near2(pre_m_ours, pre_m_corr, 0.011) & near2(pre_sd_ours, pre_sd_corr, 0.06),
  agree_pre_ours_kris = near2(pre_m_ours, pre_m_kris, 0.011) & near2(pre_sd_ours, pre_sd_kris, 0.06),
  agree_post_kris_corr = near2(post_m_kris, post_m_corr, 0.011) & near2(post_sd_kris, post_sd_corr, 0.06),
  agree_post_ours_corr = near2(post_m_ours, post_m_corr, 0.011) & near2(post_sd_ours, post_sd_corr, 0.06),
  agree_post_ours_kris = near2(post_m_ours, post_m_kris, 0.011) & near2(post_sd_ours, post_sd_kris, 0.06),
  agree_fig3corr_vs_table1_change = near2(change_m_fig3_corr, change_m_corr, 0.06),
  agree_fig3corr_vs_table1_sem = near2(sem_fig3_corr, sem_change_corr, 0.06)
)

# reasons, hand-coded from the primary studies, the corrigendum footnotes and Kriston S1 source notes
reasons <- tribble(
  ~study, ~arm, ~reason,
  "Luxton 2016", "eCBT", "n: corrigendum randomised 62, Kriston and ours analysed 45 (ITT panel, observed cases). Pre/post: corrigendum values not in the ITT panel of Luxton Table 3 (probably per-protocol panel); Kriston and ours use the ITT panel.",
  "Luxton 2016", "face-to-face", "n: randomised 59 vs analysed 42. Pre SD 11.74 (corrigendum) vs 11.33 (ITT panel).",
  "Mohr 2012", "eCBT", "n: randomised 163 vs completers 152. Means are model-based (multiple imputation); SDs derived from 95% CIs: corrigendum and ours with ITT n, Kriston with completer n (7.55 vs 7.30 vs 7.30). Kriston pre mean 22.9 vs 22.83 (rounding in JAMA Table 2).",
  "Mohr 2012", "face-to-face", "n: 162 vs 141. Post SD from CI with ITT n (8.41) vs completer n (7.85).",
  "Choi 2014", "eCBT", "n: randomised 56 (corrigendum, ours) vs completers 49 (Kriston). SDs = SE x sqrt(n) so they scale with the n chosen (7.48 vs 7.00). Arm selection: tele-PST arm of a three-arm trial, all sources agree.",
  "Choi 2014", "face-to-face", "n: 63 vs 54; SD = SE x sqrt(n) (7.46 vs 6.91).",
  "Wagner 2014", "eCBT", "n: randomised 32 vs completers 25 (Kriston). Means and SDs agree.",
  "Wagner 2014", "face-to-face", "n: 30 vs 28. Means and SDs agree.",
  "Sethi 2013", "eCBT", "All sources agree. Fig 3 vs Table 1: agree.",
  "Sethi 2013", "face-to-face", "All sources agree on inputs. Corrected Fig 3 SEM 1.3 vs Table 1 SEM 1.63 (transfer error, also noted by Kriston et al.).",
  "Sethi 2010", "eCBT", "All sources agree.",
  "Sethi 2010", "face-to-face", "All sources agree.",
  "Wright 2005", "eCBT", "Post mean: corrigendum 10.7 is the BDI week-4 completer CHANGE score read as a post mean; Kriston and ours derive post = pre - endpoint ITT change = 13.9 (pre 31.4 vs corrigendum 31.1). Post SD: no post SD exists; corrigendum 8.1 is the week-4 change SD; Kriston back-solve from the change SD with r = 0.3 (9.91); ours borrow the change SD (10.8). Original Fig 3 used the correct ITT change 17.5.",
  "Wright 2005", "face-to-face", "Post mean 9.7 = 24.4 - 14.7 in all sources (coincidence for the corrigendum, whose 9.7 is also the week-4 change). Post SD: corrigendum 8.5 (week-4 change SD), Kriston 6.72 (back-solved, r = 0.3), ours 8.0 (borrowed change SD).",
  "Himelhoch 2013", "eCBT", "n: randomised 16 vs completers 14 (Kriston). Means and SDs agree.",
  "Himelhoch 2013", "face-to-face", "n: 18 vs 17. Means and SDs agree.",
  "Poppelaars 2016", "eCBT", "Pre: corrigendum used T1 (first weekly assessment, 62.61 (11.97)); Kriston and ours use T0 pre-intervention (69.33 (8.37)). Post T9 agrees. Arm: SPARX (eCBT) vs OVK (face-to-face) of a four-arm trial, all sources agree.",
  "Poppelaars 2016", "face-to-face", "Pre: T1 63.35 (10.39) vs T0 66.94 (7.09). Post agrees.",
  "Glueckauf 2012", "eCBT", "n: corrigendum 7 vs 6 (Kriston, ours). Means and SDs agree. Corrected Fig 3 change 9.4 vs Table 1 8.0 (transfer error, also noted by Kriston et al.).",
  "Glueckauf 2012", "face-to-face", "n: 7 vs 5. Means and SDs agree.",
  "Kalapatapu 2014", "eCBT", "All sources agree. Secondary analysis of the Mohr 2012 trial (alcohol subgroup); not independent of Mohr.",
  "Kalapatapu 2014", "face-to-face", "All sources agree.",
  "Nelson 2003", "eCBT", "All sources agree.",
  "Nelson 2003", "face-to-face", "All sources agree.",
  "Kay-Lambkin 2009", "eCBT", "n: randomised 32 (corrigendum, ours) vs completers 23 (Kriston, from proportion assessed). Means and SDs agree. Arm: computer-delivered SHADE arm of a three-arm trial.",
  "Kay-Lambkin 2009", "face-to-face", "n: 35 vs 23. Post mean: corrigendum 16.65 (10.63) is the computer arm's 6-month value (column slip); Kriston and ours 13.04 (10.51) at 3 months. Also noted by Kriston et al. (28% deviation).",
  "Andersson 2013", "eCBT", "n: corrigendum 33 vs 32 (Kriston, ours). Means and SDs agree.",
  "Andersson 2013", "face-to-face", "n: 36 vs 33. Means and SDs agree."
)

source_comparison <- wide |>
  left_join(flags, by = c("study", "arm")) |>
  left_join(reasons, by = c("study", "arm")) |>
  arrange(study, desc(arm == "eCBT")) |>
  mutate(sign_convention_note = "arm-level inputs only; no sign convention applies")
write_csv(source_comparison, here("data", "source_comparison.csv"), na = "")

cat("\n== source comparison: agreement counts over", nrow(source_comparison), "trial x arm rows ==\n")
agree_summary <- flags |> summarise(across(starts_with("agree_"), ~sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "flag", values_to = "n_agree") |> mutate(n_total = nrow(flags))
print(agree_summary, n = Inf)

# ---------------------------------------------------------------------------
# 3. compact LaTeX table for the report: per trial, both arms on one line
#    n (corr / Kriston / ours), pre and post as mean (SD) from the corrigendum
#    and, where a source disagrees, that source's value; reason keyword
# ---------------------------------------------------------------------------

kw <- tribble(
  ~study, ~keyword,
  "Andersson 2013", "n", "Choi 2014", "n; SD = SE$\\sqrt{n}$", "Glueckauf 2012", "n; Fig.~3 transfer", "Himelhoch 2013", "n",
  "Kalapatapu 2014", "subsample of Mohr", "Kay-Lambkin 2009", "n; column slip (6-mo value)", "Luxton 2016", "n; ITT vs PP panel",
  "Mohr 2012", "n; SD from CI", "Nelson 2003", "", "Poppelaars 2016", "pre = T1 vs T0", "Sethi 2010", "", "Sethi 2013", "Fig.~3 transfer",
  "Wagner 2014", "n", "Wright 2005", "week-4 change as post"
)
f1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x))
cell <- function(m_c, s_c, m_k, s_k, m_o, s_o) {
  base <- sprintf("%s (%s)", f1(m_c), f1(s_c))
  k <- ifelse(near2(m_k, m_c, 0.011) & near2(s_k, s_c, 0.06), "", sprintf("; K %s (%s)", f1(m_k), f1(s_k)))
  o <- ifelse(near2(m_o, m_c, 0.011) & near2(s_o, s_c, 0.06), "", ifelse(near2(m_o, m_k, 0.011) & near2(s_o, s_k, 0.06), "; P = K", sprintf("; P %s (%s)", f1(m_o), f1(s_o))))
  paste0(base, k, o)
}
ncell <- function(nc, nk, no) ifelse(nk == nc & no == nc, sprintf("%d", nc), sprintf("%d / %d / %d", nc, nk, no))
tex_body <- source_comparison |>
  left_join(kw, by = "study") |>
  mutate(n_cell = ncell(n_corr, n_kris, n_ours),
         pre_cell = cell(pre_m_corr, pre_sd_corr, pre_m_kris, pre_sd_kris, pre_m_ours, pre_sd_ours),
         post_cell = cell(post_m_corr, post_sd_corr, post_m_kris, post_sd_kris, post_m_ours, post_sd_ours),
         arm_short = ifelse(arm == "eCBT", "e", "f"),
         row = sprintf("%s & %s & %s & %s & %s & %s \\\\", ifelse(arm == "eCBT", study, ""), arm_short, n_cell, pre_cell, post_cell, ifelse(arm == "eCBT", keyword, ""))) |>
  pull(row)
writeLines(c(
  "% Generated by R/07_kriston_prior_work.R from data/source_comparison.csv. Do not edit.",
  "\\begin{tabular}{@{}l c l l l l@{}}",
  "\\hline\\hline",
  "Study & Arm & $n$ (C / K / P) & Pre: mean (SD) & Post: mean (SD) & Disagreement \\\\",
  "\\hline", tex_body, "\\hline\\hline", "\\end{tabular}"), here("report", "table_sources.tex"))

cat("\n07 done: output/tables/07_kriston_reproduction.csv, data/source_comparison.csv, report/table_sources.tex\n")
