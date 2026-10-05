# 04_per_study_comparison.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Sanity-check table: one row per study x source x instrument, with the
# arm-level inputs each source used and the per-study effect sizes they imply.
#
# Sources
#   original_2020     Fig 3 of Luo et al. 2020 (screenshots/luo.png): change
#                     mean and the "SD" column (which is the SE of the change
#                     score) per arm, n. No pre/post means were published.
#   addendum_2020     same inputs as original_2020; the addendum only removed
#                     one study (Choi 2014) from the pool. Kept as its own
#                     block so the four sources line up.
#   corrigendum_2021  Sanger et al. 2021: Table 1 (pre/post mean, SD, n) plus
#                     the corrected Fig 3 inputs (change mean, "SD" = SE).
#   extraction_2026   data/extraction_reconciled.csv, re-extracted from the
#                     primary-study screenshots (all instruments).
#   rmd_2020          data/luo_yves_2020_extraction.csv, reconstruction of the
#                     author's unpublished 2020 extraction table (record only).
#
# Effect sizes, all on OUR sign: positive favours face-to-face CBT.
#   smd_post_g        Hedges' g from post means and post SDs
#                     (eCBT minus f2f; positive = eCBT higher score)
#   smd_change_g      Hedges' g of change scores (f2f change minus eCBT change,
#                     positive = f2f improved more). SD of change = reported
#                     where available (Wright), otherwise imputed with r = 0.6
#                     from pre and post SDs. NA where no SDs exist.
#   smd_published     the per-study value as it stands in the source's Fig 3
#                     (change mean divided by the "SD" = SE column), FLIPPED to
#                     our sign. This is the SE-as-SD artefact.
#   smd_published_luo_sign  the same value on Luo's sign, for direct comparison
#                     with the printed forest plots.
# se columns: sd / sqrt(n) for the reported statistic; fig3_sd_entered is the
# value the authors typed into RevMan's SD field.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
})

sign_convention <- "positive favours face-to-face CBT"
r_impute <- 0.6  # pre-post ICC reported by Luxton 2016 for the BDI; matches scripts 03 and 06

table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)
fig3_corr <- read_csv(here("data", "luo_fig3_corrigendum.csv"), show_col_types = FALSE)
fig3_orig <- read_csv(here("data", "luo_fig3_original_2020.csv"), show_col_types = FALSE)
extraction <- read_csv(here("data", "extraction_reconciled.csv"), show_col_types = FALSE)
rmd_2020 <- read_csv(here("data", "luo_yves_2020_extraction.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# 1. bring each source to a common arm-level layout
# ---------------------------------------------------------------------------

empty_cols <- function(d, cols) {
  for (col in cols) if (!col %in% names(d)) d[[col]] <- NA_real_
  d
}

fig3_to_rows <- function(f, source) {
  f |>
    transmute(
      study, source,
      instrument = "as published (one per study)",
      timepoint = "post-treatment as pooled by the authors (not stated per study)",
      sample = "not stated",
      n_e = ecbt_n, n_c = cbt_n,
      change_e_m = ecbt_mean, change_c_m = cbt_mean,
      fig3_sd_entered_e = ecbt_sd, fig3_sd_entered_c = cbt_sd
    )
}

rows_orig <- fig3_to_rows(fig3_orig, "original_2020")
rows_add <- fig3_to_rows(fig3_orig, "addendum_2020") |>
  mutate(note = if_else(study == "Choi 2014", "excluded from the addendum pool as the outlier", NA_character_))

rows_corr <- table1 |>
  left_join(fig3_corr, by = "study") |>
  transmute(
    study, source = "corrigendum_2021",
    instrument = "as published (one per study)",
    timepoint = "post-treatment (not stated per study)",
    sample = "not stated",
    n_e, n_c,
    pre_e_m, pre_e_sd, pre_c_m, pre_c_sd,
    post_e_m, post_e_sd, post_c_m, post_c_sd,
    change_e_m = ecbt_mean, change_c_m = cbt_mean,
    fig3_sd_entered_e = ecbt_sd, fig3_sd_entered_c = cbt_sd
  )

rows_ext <- extraction |>
  transmute(
    study, source = "extraction_2026",
    instrument = paste0(instrument, if_else(primary_instrument, " (primary)", "")),
    timepoint = post_timepoint, sample,
    n_e, n_c, n_e_post, n_c_post,
    pre_e_m, pre_e_sd, pre_c_m, pre_c_sd,
    post_e_m, post_e_sd, post_c_m, post_c_sd,
    change_e_m = coalesce(change_e_m, pre_e_m - post_e_m),
    change_c_m = coalesce(change_c_m, pre_c_m - post_c_m),
    change_e_sd, change_c_sd,
    post_sd_imputed = replace_na(post_sd_imputed, FALSE),
    note = value_type
  )

rows_rmd <- rmd_2020 |>
  transmute(
    study, source = "rmd_2020",
    instrument = "primary (as in 2020 Rmd)",
    timepoint = "post-treatment as used in 2020 (Poppelaars: T12)",
    sample = "not recorded",
    n_e, n_c, n_e_post, n_c_post,
    pre_e_m, pre_e_sd, pre_c_m, pre_c_sd,
    post_e_m, post_e_sd, post_c_m, post_c_sd,
    change_e_m = pre_e_m - post_e_m, change_c_m = pre_c_m - post_c_m,
    note = provenance
  )

all_rows <- bind_rows(rows_orig, rows_add, rows_corr, rows_ext, rows_rmd) |>
  empty_cols(c("pre_e_m", "pre_e_sd", "pre_c_m", "pre_c_sd", "post_e_m", "post_e_sd",
               "post_c_m", "post_c_sd", "change_e_sd", "change_c_sd",
               "fig3_sd_entered_e", "fig3_sd_entered_c", "n_e_post", "n_c_post")) |>
  mutate(post_sd_imputed = replace_na(post_sd_imputed, FALSE))

# ---------------------------------------------------------------------------
# 2. derived quantities and effect sizes
# ---------------------------------------------------------------------------

hedges_g <- function(m1, sd1, n1, m2, sd2, n2) {
  es <- escalc("SMD", m1i = m1, sd1i = sd1, n1i = n1, m2i = m2, sd2i = sd2, n2i = n2)
  tibble(g = as.numeric(es$yi), se = sqrt(as.numeric(es$vi)))
}

comparison <- all_rows |>
  mutate(
    # standard errors of the reported statistics
    pre_e_se = pre_e_sd / sqrt(n_e), pre_c_se = pre_c_sd / sqrt(n_c),
    post_e_se = post_e_sd / sqrt(coalesce(n_e_post, n_e)),
    post_c_se = post_c_sd / sqrt(coalesce(n_c_post, n_c)),
    # what the "SD" column in Fig 3 actually is
    se_change_r0_e = sqrt(pre_e_sd^2 / n_e + post_e_sd^2 / n_e),
    se_change_r0_c = sqrt(pre_c_sd^2 / n_c + post_c_sd^2 / n_c),
    # SD of change: reported where available, else imputed with r = 0.6
    change_e_sd_used = coalesce(change_e_sd,
                                sqrt(pre_e_sd^2 + post_e_sd^2 - 2 * r_impute * pre_e_sd * post_e_sd)),
    change_c_sd_used = coalesce(change_c_sd,
                                sqrt(pre_c_sd^2 + post_c_sd^2 - 2 * r_impute * pre_c_sd * post_c_sd)),
    change_sd_source = case_when(
      !is.na(change_e_sd) ~ "reported",
      !is.na(change_e_sd_used) ~ sprintf("imputed, r = %.1f", r_impute),
      TRUE ~ "not available"
    )
  )

g_post <- with(comparison, hedges_g(post_e_m, post_e_sd, coalesce(n_e_post, n_e),
                                    post_c_m, post_c_sd, coalesce(n_c_post, n_c)))
g_change <- with(comparison, hedges_g(change_c_m, change_c_sd_used, n_c,
                                      change_e_m, change_e_sd_used, n_e))
g_pub <- with(comparison, hedges_g(change_e_m, fig3_sd_entered_e, n_e,
                                   change_c_m, fig3_sd_entered_c, n_c))

comparison <- comparison |>
  mutate(
    smd_post_g = g_post$g, smd_post_se = g_post$se,
    smd_change_g = g_change$g, smd_change_se = g_change$se,
    smd_published_luo_sign = g_pub$g,
    smd_published = -g_pub$g,
    sign_convention = sign_convention
  ) |>
  mutate(source = factor(source, levels = c("original_2020", "addendum_2020", "corrigendum_2021",
                                            "extraction_2026", "rmd_2020"))) |>
  arrange(study, source, instrument) |>
  select(
    study, source, instrument, timepoint, sample,
    n_e, n_c, n_e_post, n_c_post,
    pre_e_m, pre_e_sd, pre_e_se, pre_c_m, pre_c_sd, pre_c_se,
    post_e_m, post_e_sd, post_e_se, post_sd_imputed, post_c_m, post_c_sd, post_c_se,
    change_e_m, change_e_sd_used, change_c_m, change_c_sd_used, change_sd_source,
    fig3_sd_entered_e, se_change_r0_e, fig3_sd_entered_c, se_change_r0_c,
    smd_post_g, smd_post_se, smd_change_g, smd_change_se,
    smd_published, smd_published_luo_sign,
    note, sign_convention
  )

write_csv(comparison, here("output", "tables", "04_per_study_comparison.csv"), na = "")

# ---------------------------------------------------------------------------
# 3. compact view: primary instrument, the four sources side by side
# ---------------------------------------------------------------------------

compact <- comparison |>
  filter(source != "rmd_2020", str_detect(instrument, "primary|as published")) |>
  mutate(across(c(smd_post_g, smd_change_g, smd_published), \(x) round(x, 2))) |>
  select(study, source, smd_post_g, smd_change_g, smd_published) |>
  pivot_wider(names_from = source, values_from = c(smd_post_g, smd_change_g, smd_published),
              names_glue = "{.value}__{source}") |>
  select(study,
         post_corr = smd_post_g__corrigendum_2021, post_ext = smd_post_g__extraction_2026,
         change_corr = smd_change_g__corrigendum_2021, change_ext = smd_change_g__extraction_2026,
         pub_orig = smd_published__original_2020, pub_corr = smd_published__corrigendum_2021)

write_csv(compact, here("output", "tables", "04_per_study_compact.csv"), na = "")

cat("Sign convention:", sign_convention, "\n")
cat("post_* = Hedges g from post means; change_* = Hedges g of change scores (SD reported or imputed r = 0.6);",
    "pub_* = the source's own Fig 3 value (change mean / SE) flipped to our sign\n\n")
print(compact, n = Inf, width = Inf)
cat("\nRows in full table:", nrow(comparison), " (", n_distinct(comparison$study), "studies x sources x instruments )\n")
cat("04 done. Outputs: output/tables/04_per_study_comparison.csv, 04_per_study_compact.csv\n")
