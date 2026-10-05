# 99_check_port_against_python.R
# One-off verification that the metafor port (02_correct_effects.R) reproduces
# the hand-coded Python reference (data/reference/reanalysis_check.py), whose
# rounded output (data/reference/python_reference_output.txt) is the 2020 reference table.
#
# Run after 01 and 02. Not part of the analysis pipeline.
#
# Known, expected differences (not errors):
#   - CI critical value: Python uses 1.96, metafor uses qnorm(0.975).
#   - I2 for REML fits: Python uses Q-based (Q - df) / Q, metafor uses
#     tau2 / (tau2 + s2). Compare the metafor i2_q column to Python.
#   - HKSJ: Python truncates the adjustment at 1 (metafor test = "adhoc").
#   - Small-sample correction J: Python uses 1 - 3 / (4 m - 1); metafor uses
#     the exact gamma-function form. Per-study g differs by up to 2.6e-4
#     (Sethi 2010, m = 17); verified to machine precision that this is the
#     whole difference. Pooled estimates are unaffected at 1e-5.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(jsonlite)
})

ref <- read_json(here("data", "reference", "python_reference_values.json"))

pooled <- read_csv(here("output", "tables", "02_pooled_estimates.csv"), show_col_types = FALSE)
per_study <- read_csv(here("output", "tables", "02_per_study_post_g.csv"), show_col_types = FALSE)
baseline <- read_csv(here("output", "tables", "02_baseline_imbalance.csv"), show_col_types = FALSE)
fig3 <- read_csv(here("output", "tables", "01_reproduction_pooled.csv"), show_col_types = FALSE)

# map Python keys to rows of the R outputs ----------------------------------

pick <- function(analysis_val, model_val, r_val = NA_real_, subset_val = "all 14") {
  r_match <- if (is.na(r_val)) is.na(pooled$r) else
    !is.na(pooled$r) & abs(pooled$r - r_val) < 1e-9
  out <- pooled[pooled$analysis == analysis_val & pooled$model == model_val &
                  pooled$subset == subset_val & r_match, ]
  stopifnot(nrow(out) == 1)
  out
}

r_rows <- bind_rows(
  fig3 |> filter(input == "corrigendum_fig3_2021", model == "DL, metafor LS variance") |>
    transmute(key = "fig3_DL", estimate, ci_lb, ci_ub, tau2, i2_q = i2),
  fig3 |> filter(input == "corrigendum_fig3_2021", model == "REML, metafor LS variance") |>
    transmute(key = "fig3_REML", estimate, ci_lb, ci_ub, tau2,
              i2_q = 100 * pmax(0, (q - (k - 1)) / q)),
  pick("post-test Hedges g", "DL") |> mutate(key = "post_DL"),
  pick("post-test Hedges g", "REML") |> mutate(key = "post_REML"),
  pick("post-test Hedges g", "REML + HKSJ (adhoc, truncated)") |> mutate(key = "post_REML_HKSJ"),
  pick("post-test Hedges g", "REML", subset_val = "without Sethi 2010 and Sethi 2013") |>
    mutate(key = "post_REML_noSethi"),
  map(c(0.3, 0.5, 0.7, 0.9), \(r) bind_rows(
    pick("SMCR difference (Morris-type)", "DL", r) |> mutate(key = paste0("smcr_r", r, "_DL")),
    pick("SMCR difference (Morris-type)", "REML", r) |> mutate(key = paste0("smcr_r", r, "_REML"))
  )) |> list_rbind(),
  map(c(0.5, 0.7), \(r) bind_rows(
    pick("SMD of change scores (SD_change imputed)", "DL", r) |>
      mutate(key = paste0("smd_change_r", r, "_DL")),
    pick("SMD of change scores (SD_change imputed)", "REML", r) |>
      mutate(key = paste0("smd_change_r", r, "_REML"))
  )) |> list_rbind()
) |>
  select(key, r_est = estimate, r_lo = ci_lb, r_hi = ci_ub, r_tau2 = tau2, r_i2_q = i2_q)

py_rows <- imap(ref$pooled, \(v, k) tibble(
  key = k, py_est = v$est, py_lo = v$lo, py_hi = v$hi, py_tau2 = v$tau2, py_i2 = v$i2
)) |> list_rbind()

pooled_cmp <- inner_join(py_rows, r_rows, by = "key") |>
  mutate(
    d_est = r_est - py_est,
    d_lo = r_lo - py_lo,
    d_hi = r_hi - py_hi,
    d_tau2 = r_tau2 - py_tau2,
    d_i2 = r_i2_q - py_i2,
    # estimate and tau2 should agree to ~1e-5 (REML optimiser tolerance);
    # CI bounds differ by (qnorm(.975) - 1.96) * se, i.e. < 1e-4
    same_at_2dp = round(r_est, 2) == round(py_est, 2) &
      round(r_lo, 2) == round(py_lo, 2) & round(r_hi, 2) == round(py_hi, 2),
    ok_tight = abs(d_est) < 1e-4 & abs(d_lo) < 5e-4 & abs(d_hi) < 5e-4 &
      abs(d_tau2) < 1e-3 & abs(d_i2) < 0.1
  )

stopifnot(nrow(pooled_cmp) == length(ref$pooled))

per_study_cmp <- imap(ref$per_study_post_g, \(v, s) tibble(study = s, py_g = v$g, py_v = v$v)) |>
  list_rbind() |>
  inner_join(per_study |> select(study, r_g = g, r_v = v), by = "study") |>
  mutate(d_g = r_g - py_g, d_v = r_v - py_v, ok = abs(d_g) < 5e-4 & abs(d_v) < 1e-4)

baseline_cmp <- imap(ref$baseline_imbalance, \(v, s) tibble(study = s, py_g = v)) |>
  list_rbind() |>
  inner_join(baseline |> select(study, r_g = baseline_g), by = "study") |>
  mutate(d_g = r_g - py_g, ok = abs(d_g) < 5e-4)

write_csv(pooled_cmp, here("output", "tables", "99_check_pooled_vs_python.csv"))
write_csv(per_study_cmp, here("output", "tables", "99_check_per_study_vs_python.csv"))
write_csv(baseline_cmp, here("output", "tables", "99_check_baseline_vs_python.csv"))

cat("\n== Pooled: metafor vs Python (differences) ==\n")
print(pooled_cmp |>
        transmute(key, r_est = round(r_est, 4), py_est = round(py_est, 4),
                  d_est = signif(d_est, 2), d_lo = signif(d_lo, 2), d_hi = signif(d_hi, 2),
                  d_tau2 = signif(d_tau2, 2), d_i2 = signif(d_i2, 2), same_at_2dp, ok_tight),
      n = Inf, width = Inf)
cat("\nPer-study post-test g: max |diff| g =", signif(max(abs(per_study_cmp$d_g)), 3),
    ", v =", signif(max(abs(per_study_cmp$d_v)), 3), "; all ok:", all(per_study_cmp$ok), "\n")
cat("Baseline imbalance: max |diff| =", signif(max(abs(baseline_cmp$d_g)), 3),
    "; all ok:", all(baseline_cmp$ok), "\n")
cat("Pooled: all same at 2 dp:", all(pooled_cmp$same_at_2dp),
    "; all within tight tolerance:", all(pooled_cmp$ok_tight), "\n")
if (!all(pooled_cmp$ok_tight)) {
  cat("Rows outside tight tolerance:\n")
  print(pooled_cmp |> filter(!ok_tight) |> select(key, d_est, d_lo, d_hi, d_tau2, d_i2))
}
