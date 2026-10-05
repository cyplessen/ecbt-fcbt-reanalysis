# 06_corrected_table.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Corrected version of the corrigendum's Table 1 (Sanger et al. 2021): the
# authors' own per-arm inputs, the quantity they entered as "SD" (the standard
# error of the mean change, SEM), the standard deviation of the change score,
# and three per-study effect sizes:
#   published   the value printed in the corrected Fig 3 (mean change / SEM)
#   smd_change  Hedges' g of change scores, SD of change imputed with r = 0.6
#               (the pre-post ICC Luxton 2016 reports; matches the multiverse grid)
#   smd_post    Hedges' g of post-test scores
# plus pooled rows. Written as CSV and as a LaTeX tabular body for the report.
#
# Sign convention in the table: positive favours face-to-face CBT for ALL
# three effect-size columns. The published values are therefore sign-reversed
# relative to the printed figure (which has negative = less change for eCBT).
#
# Data source: corrigendum Table 1 as published (data/luo_corrigendum_table1.csv),
# so that every difference to the published figure is due to the formula, not
# to our re-extraction. The re-extracted values are in 04_per_study_comparison.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
})

r_impute <- 0.6  # pre-post ICC reported by Luxton 2016 for the BDI; matches the multiverse grid
table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)
fig3 <- read_csv(here("data", "luo_fig3_corrigendum.csv"), show_col_types = FALSE)
rep01 <- read_csv(here("output", "tables", "01_reproduction_per_study.csv"), show_col_types = FALSE) |>
  filter(input == "corrigendum_fig3_2021")
post_g <- read_csv(here("output", "tables", "02_per_study_post_g.csv"), show_col_types = FALSE)
chg_g <- read_csv(here("output", "tables", "02_per_study_smd_change.csv"), show_col_types = FALSE) |>
  filter(abs(r - r_impute) < 1e-9)
pooled02 <- read_csv(here("output", "tables", "02_pooled_estimates.csv"), show_col_types = FALSE)
pooled01 <- read_csv(here("output", "tables", "01_reproduction_pooled.csv"), show_col_types = FALSE)

tab <- table1 |>
  mutate(
    change_e = pre_e_m - post_e_m, change_c = pre_c_m - post_c_m,
    sem_e = sqrt(pre_e_sd^2 / n_e + post_e_sd^2 / n_e),
    sem_c = sqrt(pre_c_sd^2 / n_c + post_c_sd^2 / n_c),
    sd_change_e = sqrt(pre_e_sd^2 + post_e_sd^2 - 2 * r_impute * pre_e_sd * post_e_sd),
    sd_change_c = sqrt(pre_c_sd^2 + post_c_sd^2 - 2 * r_impute * pre_c_sd * post_c_sd)
  ) |>
  left_join(fig3 |> transmute(study, entered_sd_e = ecbt_sd, entered_sd_c = cbt_sd), by = "study") |>
  left_join(rep01 |> transmute(study, published_smd = -yi, published_se = sqrt(vi_revman)), by = "study") |>
  left_join(chg_g |> transmute(study, smd_change = yi, smd_change_se = sqrt(vi)), by = "study") |>
  left_join(post_g |> transmute(study, smd_post = g, smd_post_se = se), by = "study") |>
  arrange(study)

stopifnot(nrow(tab) == 14, !anyNA(tab$published_smd), !anyNA(tab$smd_change), !anyNA(tab$smd_post))

# pooled rows --------------------------------------------------------------
pick <- function(analysis_val, model_val, r_val = NA_real_) {
  r_match <- if (is.na(r_val)) is.na(pooled02$r) else !is.na(pooled02$r) & abs(pooled02$r - r_val) < 1e-9
  x <- pooled02[pooled02$analysis == analysis_val & pooled02$model == model_val &
                  pooled02$subset == "all 14" & r_match, ]
  stopifnot(nrow(x) == 1); x
}
pub_pool <- pooled01 |> filter(input == "corrigendum_fig3_2021", model == "DL, RevMan variance")
stopifnot(nrow(pub_pool) == 1)
pooled <- tibble(
  quantity = c("Pooled, DerSimonian-Laird", "Pooled, REML", "Pooled, REML + HKSJ", "$I^2$ (Q-based), DL"),
  published = c(sprintf("%.2f [%.2f, %.2f]", -pub_pool$estimate, -pub_pool$ci_ub, -pub_pool$ci_lb),
                "", "", sprintf("%.0f\\%%", pub_pool$i2)),
  smd_change = c(
    with(pick("SMD of change scores (SD_change imputed)", "DL", r_impute), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("SMD of change scores (SD_change imputed)", "REML", r_impute), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("SMD of change scores (SD_change imputed)", "REML + HKSJ (knha, untruncated)", r_impute), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("SMD of change scores (SD_change imputed)", "DL", r_impute), sprintf("%.0f\\%%", i2_q))),
  smd_post = c(
    with(pick("post-test Hedges g", "DL"), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("post-test Hedges g", "REML"), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("post-test Hedges g", "REML + HKSJ (knha, untruncated)"), sprintf("%.2f [%.2f, %.2f]", estimate, ci_lb, ci_ub)),
    with(pick("post-test Hedges g", "DL"), sprintf("%.0f\\%%", i2_q)))
)

# CSV ----------------------------------------------------------------------
out_csv <- tab |>
  transmute(study, n_e, n_c,
            pre_e_m, pre_e_sd, post_e_m, post_e_sd, change_e, sem_change_e = sem_e, entered_sd_e, sd_change_e,
            pre_c_m, pre_c_sd, post_c_m, post_c_sd, change_c, sem_change_c = sem_c, entered_sd_c, sd_change_c,
            published_smd_sign_reversed = published_smd, published_se,
            smd_change_r0.6 = smd_change, smd_change_se, smd_post, smd_post_se,
            sign_convention = "positive favours face-to-face CBT; published values sign-reversed from the printed figure")
write_csv(out_csv, here("output", "tables", "06_corrected_table1.csv"))
write_csv(pooled |> mutate(across(everything(), ~str_replace_all(.x, fixed("\\"), "") |> str_replace_all(fixed("$"), ""))), here("output", "tables", "06_corrected_table1_pooled.csv"))

# LaTeX body ---------------------------------------------------------------
f1 <- function(x) sprintf("%.1f", x)
f2 <- function(x) sprintf("%.2f", x)
msd <- function(m, s) sprintf("%s (%s)", f1(m), f1(s))
es <- function(g, se) sprintf("%s (%s)", f2(g), f2(se))
tex_rows <- pmap_chr(tab, function(study, n_e, n_c, pre_e_m, pre_e_sd, post_e_m, post_e_sd, change_e, sem_e, sd_change_e,
                                   pre_c_m, pre_c_sd, post_c_m, post_c_sd, change_c, sem_c, sd_change_c,
                                   published_smd, published_se, smd_change, smd_change_se, smd_post, smd_post_se, ...) {
  paste(c(study, sprintf("%d/%d", n_e, n_c),
          msd(pre_e_m, pre_e_sd), msd(post_e_m, post_e_sd), f1(change_e), f1(sem_e), f1(sd_change_e),
          msd(pre_c_m, pre_c_sd), msd(post_c_m, post_c_sd), f1(change_c), f1(sem_c), f1(sd_change_c),
          es(published_smd, published_se), es(smd_change, smd_change_se), es(smd_post, smd_post_se)),
        collapse = " & ") |> paste("\\\\")
})
pooled_rows <- pooled |>
  mutate(row = sprintf("\\multicolumn{12}{l}{\\textit{%s}} & %s & %s & %s \\\\", quantity, published, smd_change, smd_post)) |>
  pull(row)

tex <- c(
  "% Generated by R/06_corrected_table.R from data/luo_corrigendum_table1.csv and output/tables/. Do not edit.",
  "\\begin{tabular}{@{}l c cc r r r cc r r r r r r@{}}",
  "\\hline\\hline",
  " & & \\multicolumn{5}{c}{eCBT} & \\multicolumn{5}{c}{Face-to-face CBT} & \\multicolumn{3}{c}{SMD (SE), positive favours face-to-face} \\\\",
  "\\cline{3-7} \\cline{8-12} \\cline{13-15}",
  "Study & $n$ & Pre (SD) & Post (SD) & $\\Delta$ & SEM$_\\Delta$ & SD$_\\Delta$ & Pre (SD) & Post (SD) & $\\Delta$ & SEM$_\\Delta$ & SD$_\\Delta$ & Published$^a$ & Change$^b$ & Post$^c$ \\\\",
  "\\hline",
  tex_rows,
  "\\hline",
  pooled_rows,
  "\\hline\\hline",
  "\\end{tabular}"
)
writeLines(tex, here("report", "table_corrected.tex"))

cat("06 done: output/tables/06_corrected_table1.csv, 06_corrected_table1_pooled.csv, report/table_corrected.tex\n")
print(pooled)
