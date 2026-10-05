# 02_correct_effects.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Correct effect sizes from the corrigendum's own Table 1 (Sanger et al. 2021),
# using metafor. Ports reanalysis_check.py (data/reference/).
#
# SIGN CONVENTION FOR EVERYTHING IN THIS SCRIPT AND ITS OUTPUTS:
#   positive favours face-to-face CBT
#   (eCBT arm has the higher depression score at post-test, or shows less
#   pre-to-post improvement). This is the OPPOSITE of Luo's Fig 3.
#
# Analyses
#   a. post-test Hedges' g (escalc SMD): DL, REML, REML + HKSJ,
#      REML without the two MoodGYM trials (Sethi 2010, Sethi 2013)
#   b. difference in standardized mean change (escalc SMCR, Becker 1988 variance),
#      pre-post r in {0.3, 0.5, 0.7, 0.9}, DL and REML
#   c. SMD of change scores with SD_change imputed from r (Cochrane Handbook
#      6.5.2.8), r in {0.5, 0.7}, DL and REML
#   d. baseline imbalance (pre eCBT minus pre face-to-face, pooled pre-SD units)
#
# Note on HKSJ: metafor test = "knha" does NOT truncate the adjustment factor
# at 1; test = "adhoc" does (truncated HKSJ, as in the Python reference).
# Both are reported.
# Note on I2: metafor reports I2 = tau2 / (tau2 + s2) with s2 the "typical"
# sampling variance (Higgins and Thompson 2002). The Python reference used the
# Q-based I2 = (Q - df) / Q, which is what RevMan prints and what the DL
# estimator implies. Both are reported (i2 = metafor; i2_q = Q-based).

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
})

table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)

sign_convention <- "positive favours face-to-face CBT"

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

i2_q_based <- function(fit) {
  100 * max(0, (fit$QE - (fit$k - fit$p)) / fit$QE)
}

fit_summary <- function(fit, analysis, model, r = NA_real_, subset = "all 14") {
  tibble(
    analysis = analysis,
    model = model,
    r = r,
    subset = subset,
    k = fit$k,
    estimate = as.numeric(coef(fit)),
    se = fit$se,
    ci_lb = fit$ci.lb,
    ci_ub = fit$ci.ub,
    tau2 = fit$tau2,
    i2 = fit$I2,
    i2_q = i2_q_based(fit),
    q = fit$QE,
    q_p = fit$QEp,
    sign_convention = sign_convention
  )
}

fit_three <- function(es, analysis, r = NA_real_, subset = "all 14") {
  bind_rows(
    fit_summary(rma(yi, vi, data = es, method = "DL"), analysis, "DL", r, subset),
    fit_summary(rma(yi, vi, data = es, method = "REML"), analysis, "REML", r, subset),
    fit_summary(rma(yi, vi, data = es, method = "REML", test = "knha"),
                analysis, "REML + HKSJ (knha, untruncated)", r, subset),
    fit_summary(rma(yi, vi, data = es, method = "REML", test = "adhoc"),
                analysis, "REML + HKSJ (adhoc, truncated)", r, subset)
  )
}

# ---------------------------------------------------------------------------
# a. post-test Hedges' g
#    m1 = eCBT post, m2 = face-to-face post: positive = eCBT higher score
# ---------------------------------------------------------------------------

es_post <- escalc(
  measure = "SMD",
  m1i = post_e_m, sd1i = post_e_sd, n1i = n_e,
  m2i = post_c_m, sd2i = post_c_sd, n2i = n_c,
  data = table1, slab = study
)

per_study_post_g <- es_post |>
  as_tibble() |>
  transmute(
    study, n_e, n_c,
    g = yi, v = vi, se = sqrt(vi),
    ci_lb = g - qnorm(0.975) * se,
    ci_ub = g + qnorm(0.975) * se,
    sign_convention = sign_convention
  )

pooled_post <- bind_rows(
  fit_three(es_post, "post-test Hedges g"),
  fit_three(
    es_post |> filter(!study %in% c("Sethi 2010", "Sethi 2013")),
    "post-test Hedges g",
    subset = "without Sethi 2010 and Sethi 2013"
  )
)

# ---------------------------------------------------------------------------
# b. difference in standardized mean change (SMCR), r grid
#    per arm: (post - pre) / sd_pre, Becker 1988 variance 2(1-r)/n + g^2/(2n)
#    difference: eCBT change minus face-to-face change (less negative in eCBT
#    = less improvement = positive = favours face-to-face)
# ---------------------------------------------------------------------------

r_grid_smcr <- c(0.3, 0.5, 0.7, 0.9)

smcr_diff <- function(dat, r) {
  arm_e <- escalc(measure = "SMCR", m1i = post_e_m, m2i = pre_e_m,
                  sd1i = pre_e_sd, ni = n_e, ri = rep(r, nrow(dat)), data = dat)
  arm_c <- escalc(measure = "SMCR", m1i = post_c_m, m2i = pre_c_m,
                  sd1i = pre_c_sd, ni = n_c, ri = rep(r, nrow(dat)), data = dat)
  tibble(
    study = dat$study,
    r = r,
    smcr_e = as.numeric(arm_e$yi), v_e = arm_e$vi,
    smcr_c = as.numeric(arm_c$yi), v_c = arm_c$vi,
    yi = smcr_e - smcr_c,
    vi = v_e + v_c
  )
}

per_study_smcr <- map(r_grid_smcr, \(r) smcr_diff(table1, r)) |>
  list_rbind() |>
  mutate(sign_convention = sign_convention)

pooled_smcr <- map(r_grid_smcr, \(r) {
  es <- per_study_smcr |> filter(r == !!r)
  fit_three(es, "SMCR difference (Morris-type)", r = r)
}) |>
  list_rbind()

# ---------------------------------------------------------------------------
# c. SMD of change scores, SD_change imputed from r (Cochrane 6.5.2.8)
#    m1 = face-to-face change (pre - post), m2 = eCBT change:
#    positive = face-to-face improved more
# ---------------------------------------------------------------------------

r_grid_change <- c(0.3, 0.5, 0.6, 0.7)  # 0.3 Kriston et al. 2022; 0.6 Luxton 2016 reported ICC

smd_change <- function(dat, r) {
  d <- dat |>
    mutate(
      change_e = pre_e_m - post_e_m,
      change_c = pre_c_m - post_c_m,
      sd_change_e = sqrt(pre_e_sd^2 + post_e_sd^2 - 2 * r * pre_e_sd * post_e_sd),
      sd_change_c = sqrt(pre_c_sd^2 + post_c_sd^2 - 2 * r * pre_c_sd * post_c_sd)
    )
  escalc(
    measure = "SMD",
    m1i = change_c, sd1i = sd_change_c, n1i = n_c,
    m2i = change_e, sd2i = sd_change_e, n2i = n_e,
    data = d, slab = study
  ) |>
    as_tibble() |>
    transmute(study, r = r, change_e, change_c, sd_change_e, sd_change_c, yi, vi)
}

per_study_smd_change <- map(r_grid_change, \(r) smd_change(table1, r)) |>
  list_rbind() |>
  mutate(sign_convention = sign_convention)

pooled_smd_change <- map(r_grid_change, \(r) {
  es <- per_study_smd_change |> filter(r == !!r)
  fit_three(es, "SMD of change scores (SD_change imputed)", r = r)
}) |>
  list_rbind()

# ---------------------------------------------------------------------------
# d. baseline imbalance: pre eCBT minus pre face-to-face, Hedges' g
# ---------------------------------------------------------------------------

baseline_imbalance <- escalc(
  measure = "SMD",
  m1i = pre_e_m, sd1i = pre_e_sd, n1i = n_e,
  m2i = pre_c_m, sd2i = pre_c_sd, n2i = n_c,
  data = table1, slab = study
) |>
  as_tibble() |>
  transmute(study, baseline_g = yi, baseline_v = vi,
            direction = "positive = eCBT arm more depressed at baseline")

# ---------------------------------------------------------------------------
# write and print
# ---------------------------------------------------------------------------

pooled_all <- bind_rows(pooled_post, pooled_smcr, pooled_smd_change)

write_csv(per_study_post_g, here("output", "tables", "02_per_study_post_g.csv"))
write_csv(per_study_smcr, here("output", "tables", "02_per_study_smcr.csv"))
write_csv(per_study_smd_change, here("output", "tables", "02_per_study_smd_change.csv"))
write_csv(baseline_imbalance, here("output", "tables", "02_baseline_imbalance.csv"))
write_csv(pooled_all, here("output", "tables", "02_pooled_estimates.csv"))

round_tbl <- function(x, digits = 2) {
  x |> mutate(across(c(any_of(c("estimate", "ci_lb", "ci_ub", "tau2", "g", "se",
                                  "baseline_g"))), \(v) round(v, digits)),
              across(any_of(c("i2", "i2_q")), \(v) round(v)))
}

cat("\nSIGN CONVENTION: ", sign_convention, "\n")
cat("\n== a. Post-test Hedges' g per study ==\n")
print(per_study_post_g |> select(study, g, ci_lb, ci_ub) |> round_tbl(), n = Inf)
cat("\n== Pooled estimates ==\n")
print(pooled_all |> select(analysis, model, r, subset, estimate, ci_lb, ci_ub, tau2, i2, i2_q) |>
        round_tbl(), n = Inf, width = Inf)
cat("\n== d. Baseline imbalance (pre eCBT minus pre f2f, pooled pre-SD units) ==\n")
print(baseline_imbalance |> select(study, baseline_g) |> round_tbl(), n = Inf)

cat("\n02 done. Outputs in output/tables/02_*.csv\n")
