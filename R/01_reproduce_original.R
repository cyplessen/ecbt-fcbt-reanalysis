# 01_reproduce_original.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Purpose
#   1. Show that the "SD" column the authors entered into RevMan (corrected
#      Fig 3, Sanger et al. 2021 corrigendum) is the standard error of the
#      change score under r = 0, sqrt(sd_pre^2 / n + sd_post^2 / n), and not a
#      standard deviation. Recomputed from the corrigendum's own Table 1.
#   2. Reproduce the published pooled estimates from the Fig 3 inputs:
#      corrigendum -0.92 [-1.76, -0.09] (DL, RevMan) and the original 2020
#      value -1.73 [-2.72, -0.74] (inputs read from screenshots/luo.png).
#      RevMan's SMD sampling variance is 1/n1 + 1/n2 + g^2 / (2 (N - 3.94));
#      metafor's default (vtype = "LS") uses 2 N. Both are reported; the
#      RevMan form reproduces the published values exactly.
#
# Sign convention in THIS script: Luo's. yi = eCBT "change" minus face-to-face
# "change" divided by the pooled "SD". Arithmetically, negative = less change
# in eCBT = favours face-to-face. The ORIGINAL paper nevertheless read -1.73 as
# eCBT being MORE effective; the addendum reversed that reading (same number,
# now "less change for eCBT"); the corrigendum kept the addendum's direction.
# The numbers themselves keep this sign throughout.
# Scripts 02 onwards use the opposite convention (positive favours
# face-to-face), stated there.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
})

table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)
fig3_corr <- read_csv(here("data", "luo_fig3_corrigendum.csv"), show_col_types = FALSE)
fig3_orig <- read_csv(here("data", "luo_fig3_original_2020.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# 1. Does the corrigendum "SD" reproduce as SE of the change score (r = 0)?
# ---------------------------------------------------------------------------

se_as_sd_check <- table1 |>
  mutate(
    change_e = pre_e_m - post_e_m,
    change_c = pre_c_m - post_c_m,
    se_change_e = sqrt(pre_e_sd^2 / n_e + post_e_sd^2 / n_e),
    se_change_c = sqrt(pre_c_sd^2 / n_c + post_c_sd^2 / n_c)
  ) |>
  left_join(fig3_corr, by = "study") |>
  transmute(
    study,
    change_e, fig3_mean_e = ecbt_mean, se_change_e, fig3_sd_e = ecbt_sd,
    change_c, fig3_mean_c = cbt_mean, se_change_c, fig3_sd_c = cbt_sd,
    mean_mismatch = abs(change_e - fig3_mean_e) >= 0.06 |
      abs(change_c - fig3_mean_c) >= 0.06,
    sd_mismatch = abs(se_change_e - fig3_sd_e) >= 0.11 |
      abs(se_change_c - fig3_sd_c) >= 0.11
  )

write_csv(se_as_sd_check, here("output", "tables", "01_se_as_sd_check.csv"))

cat("\n== 1. Corrigendum 'SD' vs SE of change recomputed from Table 1 ==\n")
print(
  se_as_sd_check |>
    mutate(across(where(is.numeric), \(x) round(x, 2))),
  n = Inf, width = Inf
)
cat("\nMean mismatches: ",
    paste(se_as_sd_check$study[se_as_sd_check$mean_mismatch], collapse = ", "),
    "\n'SD' mismatches (tolerance 0.1): ",
    paste(se_as_sd_check$study[se_as_sd_check$sd_mismatch], collapse = ", "),
    "\n")

# ---------------------------------------------------------------------------
# 2. Reproduce the published pooled estimates from the Fig 3 inputs
#    (i.e. feed the "SD" column to escalc exactly as RevMan did)
# ---------------------------------------------------------------------------

pool_fig3 <- function(dat, label) {
  es <- escalc(
    measure = "SMD",
    m1i = ecbt_mean, sd1i = ecbt_sd, n1i = ecbt_n,
    m2i = cbt_mean, sd2i = cbt_sd, n2i = cbt_n,
    data = dat, slab = study
  )
  es$vi_revman <- 1 / es$ecbt_n + 1 / es$cbt_n +
    es$yi^2 / (2 * (es$ecbt_n + es$cbt_n - 3.94))

  fit_dl_revman <- rma(yi, vi_revman, data = es, method = "DL")
  fit_dl <- rma(yi, vi, data = es, method = "DL")
  fit_reml <- rma(yi, vi, data = es, method = "REML")
  fit_reml_hksj <- rma(yi, vi, data = es, method = "REML", test = "knha")

  per_study <- es |>
    as_tibble() |>
    transmute(input = label, study, yi, vi, vi_revman, se = sqrt(vi))

  fits <- list(
    "DL, RevMan variance" = fit_dl_revman,
    "DL, metafor LS variance" = fit_dl,
    "REML, metafor LS variance" = fit_reml,
    "REML + HKSJ, metafor LS variance" = fit_reml_hksj
  )
  pooled <- imap(fits, \(f, nm) tibble(
    input = label, model = nm, estimate = as.numeric(coef(f)),
    ci_lb = f$ci.lb, ci_ub = f$ci.ub, tau2 = f$tau2, i2 = f$I2, q = f$QE, k = f$k
  )) |> list_rbind()
  list(per_study = per_study, pooled = pooled, fits = fits)
}

rep_corr <- pool_fig3(fig3_corr, "corrigendum_fig3_2021")
rep_orig <- pool_fig3(fig3_orig, "original_fig3_2020")

# Addendum: "without outlier" -0.79 [-1.60, 0.02]. The original names the
# outlier (Section 2.5.8: "removing the outlier (Choi et al., 2014)";
# Appendix I, Fig. 8). Removing Choi 2014 (-17.39) from the ORIGINAL inputs
# gives -0.79 [-1.61, 0.02] with the RevMan variance.
rep_orig_no_choi <- pool_fig3(
  fig3_orig |> filter(study != "Choi 2014"),
  "original_fig3_2020_without_choi"
)

reproduction_per_study <- bind_rows(
  rep_corr$per_study, rep_orig$per_study, rep_orig_no_choi$per_study
)
reproduction_pooled <- bind_rows(
  rep_corr$pooled, rep_orig$pooled, rep_orig_no_choi$pooled
) |>
  mutate(
    published = case_when(
      input == "corrigendum_fig3_2021" & model == "DL, RevMan variance" ~ "-0.92 [-1.76, -0.09], I2 97%",
      input == "original_fig3_2020" & model == "DL, RevMan variance" ~ "-1.73 [-2.72, -0.74], tau2 3.32, Q 548.36, I2 98%",
      input == "original_fig3_2020_without_choi" & model == "DL, RevMan variance" ~ "addendum: -0.79 [-1.60, 0.02] (outlier unnamed)",
      TRUE ~ NA_character_
    ),
    sign_convention = "Luo: negative favours face-to-face"
  )

write_csv(reproduction_per_study, here("output", "tables", "01_reproduction_per_study.csv"))
write_csv(reproduction_pooled, here("output", "tables", "01_reproduction_pooled.csv"))

cat("\n== 2. Published pooled estimates reproduced from Fig 3 inputs ==\n")
cat("Sign: Luo's convention, negative favours face-to-face.\n")
print(
  reproduction_pooled |>
    mutate(across(c(estimate, ci_lb, ci_ub, tau2), \(x) round(x, 2)),
           i2 = round(i2), q = round(q, 1)),
  n = Inf, width = Inf
)

cat("\n== Per-study 'SMD' from the corrigendum inputs (the giveaway values) ==\n")
print(
  rep_corr$per_study |>
    mutate(across(c(yi, vi, vi_revman, se), \(x) round(x, 2))) |>
    arrange(yi),
  n = Inf
)

cat("\n01 done. Outputs in output/tables/01_*.csv\n")
