# 03_multiverse.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# Specification-curve multiverse over the pre-specified factors listed in
# README.md, run with metaMultiverse plus the pre/post
# how-factor extension in R/mm_pre_post_extension.R.
#
# Which factors (study selection; metaMultiverse E/U/N types)
#   data_source  N  corrigendum Table 1 as published | Kriston et al. 2022 re-extraction
#                   (supplement S4; completer n; Wright post SD back-solved with r = 0.3) |
#                   reconciled extraction
#   instrument   E  primary | clinician-preferred | self-preferred | all
#                   (all = every instrument, handled by 3-level model or RVE)
#   inclusion    U  all 14 | no Choi (PST) | no MoodGYM (Sethi 2010, 2013) |
#                   no arms with n < 20 | no Kalapatapu (subsample of Mohr)
# How factors (effect size and estimation)
#   es_metric        post-test SMD | SMC with r = 0.3, 0.5, 0.7, 0.9 |
#                    adjusted differences where reported (Mohr, Luxton), else post SMD
#   wright_post      exclude | derived post with borrowed SD | change-score SMD
#                    (reconciled data, post-test metrics only)
#   wright_timepoint endpoint ITT | week 8 completers (reconciled data only)
#   ma_method        DL, REML, PM, each with and without HKSJ; 3-level and RVE
#                    for the all-instruments arm
#
# SIGN CONVENTION: positive favours face-to-face CBT (eCBT arm has the higher
# post-test score or less pre-to-post improvement). Opposite of Luo's Fig 3.

# metaMultiverse is not on CRAN: remotes::install_github("cyplessen/metaMultiverse").
# If it lives in a project-local library, point R at it before loading:
if (nzchar(Sys.getenv("LUO_RLIB"))) .libPaths(c(Sys.getenv("LUO_RLIB"), .libPaths()))

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(metafor)
  library(metaMultiverse)
  library(ggdist)
  library(patchwork)
})
source(here("R", "mm_pre_post_extension.R"))
register_pre_post_estimators()

sign_convention <- "positive favours face-to-face CBT"

# ---------------------------------------------------------------------------
# 1. arm-level data: two data sources stacked
# ---------------------------------------------------------------------------

reconciled <- read_csv(here("data", "extraction_reconciled.csv"), show_col_types = FALSE)
table1 <- read_csv(here("data", "luo_corrigendum_table1.csv"), show_col_types = FALSE)
wright_alt <- read_csv(here("data", "wright_2005_change_by_timepoint.csv"), show_col_types = FALSE) |>
  mutate(timepoint = if_else(timepoint == "week 8 (completer)", "week8_completer", "week4_completer"))

# instrument codes: which rows each instrument rule selects
#   P_C_only  primary, clinician-rated, no self-report alternative
#   P_C_alt   primary, clinician-rated, self-report alternative exists
#   P_S_only  primary, self-report, no clinician alternative
#   P_S_alt   primary, self-report, clinician alternative exists
#   C_alt     non-primary clinician instrument
#   S_alt     non-primary self-report instrument (first one)
#   S_extra   further non-primary self-report instruments (all-instruments only)
#   P         corrigendum row (as published, one instrument)
code_instruments <- function(d) {
  d |>
    group_by(study) |>
    mutate(
      has_clin_alt = any(rater == "clinician" & !primary_instrument),
      has_self_alt = any(rater == "self" & !primary_instrument),
      self_rank = cumsum(rater == "self" & !primary_instrument)
    ) |>
    ungroup() |>
    mutate(instrument_code = case_when(
      primary_instrument & rater == "clinician" & has_self_alt ~ "P_C_alt",
      primary_instrument & rater == "clinician" ~ "P_C_only",
      primary_instrument & rater == "self" & has_clin_alt ~ "P_S_alt",
      primary_instrument & rater == "self" ~ "P_S_only",
      !primary_instrument & rater == "clinician" ~ "C_alt",
      !primary_instrument & rater == "self" & self_rank == 1 ~ "S_alt",
      TRUE ~ "S_extra"
    )) |>
    select(-has_clin_alt, -has_self_alt, -self_rank)
}

arm_reconciled <- reconciled |>
  code_instruments() |>
  mutate(data_source = "reconciled", post_sd_imputed = replace_na(post_sd_imputed, FALSE))

arm_corrigendum <- table1 |>
  mutate(
    data_source = "corrigendum", instrument = "as published", rater = NA_character_,
    primary_instrument = TRUE, instrument_code = "P",
    n_e_post = NA_real_, n_c_post = NA_real_,
    change_e_m = NA_real_, change_e_sd = NA_real_, change_c_m = NA_real_, change_c_sd = NA_real_,
    adj_diff = NA_real_, adj_diff_lb = NA_real_, adj_diff_ub = NA_real_, adj_ci_level = NA_real_,
    post_sd_imputed = FALSE
  )

# Kriston et al. 2022, supplement S4 (data/kriston_supplement_s4_analysis.csv):
# their consensus re-extraction, one primary instrument per study, analysed n.
# Wright: post derived from the endpoint ITT change, post SD back-solved from
# the reported change SD with r = 0.3; flagged post_sd_imputed so the Wright
# handling factor applies. Their change SDs for Wright equal the reported ones.
kriston <- read_csv(here("data", "kriston_supplement_s4_analysis.csv"), show_col_types = FALSE)
arm_kriston <- kriston |>
  transmute(
    study, n_e, n_c, pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd,
    data_source = "kriston", instrument = "as extracted by Kriston et al.", rater = NA_character_,
    primary_instrument = TRUE, instrument_code = "P",
    n_e_post = NA_real_, n_c_post = NA_real_,
    change_e_m = if_else(study == "Wright 2005", change_e_m, NA_real_),
    change_e_sd = if_else(study == "Wright 2005", change_e_sd, NA_real_),
    change_c_m = if_else(study == "Wright 2005", change_c_m, NA_real_),
    change_c_sd = if_else(study == "Wright 2005", change_c_sd, NA_real_),
    adj_diff = NA_real_, adj_diff_lb = NA_real_, adj_diff_ub = NA_real_, adj_ci_level = NA_real_,
    post_sd_imputed = study == "Wright 2005"
  )

arm_data <- bind_rows(arm_reconciled, arm_corrigendum, arm_kriston) |>
  mutate(study_key = study) |>
  select(study, study_key, data_source, instrument, rater, primary_instrument, instrument_code,
         n_e, n_c, n_e_post, n_c_post,
         pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd,
         change_e_m, change_e_sd, change_c_m, change_c_sd,
         adj_diff, adj_diff_lb, adj_diff_ub, adj_ci_level, post_sd_imputed)

write_csv(arm_data, here("output", "tables", "03_arm_data_stacked.csv"), na = "")

# ---------------------------------------------------------------------------
# 2. which factors (built per run: custom groups must only name levels present)
# ---------------------------------------------------------------------------

all_studies <- sort(unique(arm_data$study))
moodgym <- c("Sethi 2010", "Sethi 2013")
small_arms <- arm_data |> filter(pmin(n_e, n_c) < 20) |> distinct(study) |> pull(study)

instrument_groups <- list(
  primary = c("P", "P_C_only", "P_C_alt", "P_S_only", "P_S_alt"),
  clinician_preferred = c("P_C_only", "P_C_alt", "P_S_only", "C_alt"),
  self_preferred = c("P_S_only", "P_S_alt", "P_C_only", "S_alt")
)

which_factors_for <- function(dat) {
  present <- unique(dat$study)
  keep <- function(x) intersect(x, present)
  list(
    data_source = "data_source|N",
    instrument = list(column = "instrument_code", decision = "E",
                      groups = map(instrument_groups, \(g) intersect(g, unique(dat$instrument_code)))),
    inclusion = list(column = "study_key", decision = "U", groups = list(
      all_14 = keep(all_studies),
      no_choi = keep(setdiff(all_studies, "Choi 2014")),
      no_moodgym = keep(setdiff(all_studies, moodgym)),
      no_small_arms = keep(setdiff(all_studies, small_arms)),
      no_kalapatapu = keep(setdiff(all_studies, "Kalapatapu 2014"))
    ))
  )
}

# ---------------------------------------------------------------------------
# 3. how factors
# ---------------------------------------------------------------------------

metrics <- c("post_smd", "chg_smd_r0.3", "chg_smd_r0.6", "smc_r0.3", "smc_r0.5", "smc_r0.7", "smc_r0.9", "adjusted_hybrid")
post_metrics <- c("post_smd", "adjusted_hybrid")

es_grid <- bind_rows(
  # corrigendum: as published, no Wright choices, no adjusted values in Table 1
  tibble(data_source = "corrigendum", es_metric = setdiff(metrics, "adjusted_hybrid"),
         wright_post = if_else(es_metric == "post_smd", "as_published", "not_applicable"),
         wright_timepoint = "as_published"),
  # Kriston: post-test metrics with three Wright handlings at their time point
  # (endpoint ITT, the only one they extracted); SMC metrics as for reconciled
  tibble(data_source = "kriston", es_metric = "post_smd",
         wright_post = c("derived_borrowed_sd", "change_smd", "exclude"),
         wright_timepoint = c("endpoint_itt", "endpoint_itt", "not_applicable")),
  tibble(data_source = "kriston", es_metric = setdiff(metrics, c("adjusted_hybrid", post_metrics)),
         wright_post = "not_applicable", wright_timepoint = "endpoint_itt"),
  # reconciled, post-test metrics: three Wright handlings x two time points
  expand_grid(data_source = "reconciled", es_metric = post_metrics,
              wright_post = c("derived_borrowed_sd", "change_smd"),
              wright_timepoint = c("endpoint_itt", "week8_completer")),
  tibble(data_source = "reconciled", es_metric = post_metrics,
         wright_post = "exclude", wright_timepoint = "not_applicable"),
  # reconciled, SMC metrics: Wright enters via pre SD and derived post mean
  expand_grid(data_source = "reconciled", es_metric = setdiff(metrics, post_metrics),
              wright_post = "not_applicable",
              wright_timepoint = c("endpoint_itt", "week8_completer"))
)

pooled_methods <- c("dl", "reml", "pm", "dl_hksj", "reml_hksj", "pm_hksj")
modeled_methods <- c("three-level", "rve")

spec_filter <- function(specs, es_row, factor_info) {
  wf <- setNames(factor_info$wf_internal, factor_info$label)
  specs |>
    filter(
      .data[[wf[["data_source"]]]] == es_row$data_source,
      # no duplicate of all_14 through the automatic total
      !str_starts(.data[[wf[["inclusion"]]]], "total_"),
      # corrigendum: one instrument, as published
      es_row$data_source == "reconciled" | .data[[wf[["instrument"]]]] == "primary",  # one instrument for corrigendum and kriston
      # all instruments -> modeled dependency only; one per study -> pooled methods
      (str_starts(.data[[wf[["instrument"]]]], "total_") & dependency == "modeled" &
         ma_method %in% modeled_methods) |
        (!str_starts(.data[[wf[["instrument"]]]], "total_") & dependency == "aggregate" &
           ma_method %in% pooled_methods)
    )
}

# ---------------------------------------------------------------------------
# 4. run
# ---------------------------------------------------------------------------

cat("Running", nrow(es_grid), "effect-size variants\n")
mv <- run_pre_post_multiverse(
  arm_data, which_factors_for, es_grid,
  ma_methods = c(pooled_methods, modeled_methods),
  dependencies = c("aggregate", "modeled"),
  spec_filter = spec_filter, wright_alt = wright_alt, k_smallest_ma = 3
)

results <- mv$results |>
  mutate(across(where(is.factor), as.character)) |>
  mutate(
    instrument = if_else(str_starts(instrument, "total_"), "all_instruments", instrument),
    # Wright's handling and time point are only real choices when Wright is in the set
    wright_in_set = str_detect(studies_in_set, "Wright 2005"),
    wright_post = if_else(wright_in_set | wright_post == "as_published", wright_post, "not_in_set"),
    wright_timepoint = if_else(wright_in_set | wright_timepoint == "as_published", wright_timepoint, "not_in_set")
  ) |>
  # identical analyses (same data rows, same estimator, same estimate) count once
  distinct(data_source, es_metric, instrument, inclusion, ma_method, dependency,
           wright_post, wright_timepoint, set, b, ci.lb, ci.ub, .keep_all = TRUE) |>
  mutate(
    ci_excludes_zero = ci.lb > 0 | ci.ub < 0,
    direction = case_when(ci.lb > 0 ~ "favours face-to-face", ci.ub < 0 ~ "favours eCBT",
                          TRUE ~ "CI includes zero"),
    sign_convention = sign_convention
  ) |>
  rename(estimate = b, ci_lb = ci.lb, ci_ub = ci.ub, p_value = pval, k_effect_sizes = k) |>
  select(data_source, es_metric, instrument, inclusion, ma_method, dependency,
         wright_post, wright_timepoint, estimate, ci_lb, ci_ub, p_value,
         k_studies, k_effect_sizes, ci_excludes_zero, direction, studies_in_set, set, sign_convention) |>
  arrange(estimate) |>
  mutate(spec_rank = row_number())

write_csv(results, here("output", "tables", "03_multiverse_results.csv"))
write_csv(mv$specifications, here("output", "tables", "03_multiverse_specifications.csv"))
writeLines(mv$warnings, here("output", "tables", "03_multiverse_warnings.txt"))

cat(sprintf("\nSpecifications attempted: %d; distinct analyses after collapsing Wright-free duplicates: %d; warnings: %d\n",
            mv$n_attempted, nrow(results), length(mv$warnings)))

# ---------------------------------------------------------------------------
# 5. summaries
# ---------------------------------------------------------------------------

summarise_specs <- function(d) {
  d |> summarise(
    n_specs = n(),
    median = median(estimate), q25 = quantile(estimate, .25), q75 = quantile(estimate, .75),
    min = min(estimate), max = max(estimate),
    share_ci_excludes_zero = mean(ci_excludes_zero),
    share_favours_f2f = mean(ci_lb > 0), share_favours_ecbt = mean(ci_ub < 0),
    .groups = "drop"
  )
}

overall <- summarise_specs(results)
by_factor <- bind_rows(
  results |> group_by(factor = "data_source", level = data_source) |> summarise_specs(),
  results |> group_by(factor = "es_metric", level = es_metric) |> summarise_specs(),
  results |> group_by(factor = "instrument", level = instrument) |> summarise_specs(),
  results |> group_by(factor = "inclusion", level = inclusion) |> summarise_specs(),
  results |> group_by(factor = "ma_method", level = ma_method) |> summarise_specs(),
  results |> group_by(factor = "wright_post", level = wright_post) |> summarise_specs(),
  results |> group_by(factor = "wright_timepoint", level = wright_timepoint) |> summarise_specs()
)
write_csv(overall, here("output", "tables", "03_multiverse_summary_overall.csv"))
write_csv(by_factor, here("output", "tables", "03_multiverse_summary_by_factor.csv"))

primary_spec <- results |>
  filter(data_source == "reconciled", es_metric == "post_smd", instrument == "primary",
         inclusion == "all_14", ma_method == "reml", wright_post == "derived_borrowed_sd",
         wright_timepoint == "endpoint_itt")

cat("\nOverall:\n"); print(overall |> mutate(across(where(is.numeric), \(x) round(x, 2))), width = Inf)
cat("\nBy factor:\n")
print(by_factor |> mutate(across(where(is.numeric), \(x) round(x, 2))) |>
        select(factor, level, n_specs, median, min, max, share_ci_excludes_zero), n = Inf)
cat("\nPrimary specification (reconciled, post SMD, primary instrument, REML, all 14):\n")
print(primary_spec |> select(estimate, ci_lb, ci_ub, k_studies) |> mutate(across(where(is.numeric), \(x) round(x, 2))))

# ---------------------------------------------------------------------------
# 6. figures, Our World in Data style
# ---------------------------------------------------------------------------

owid_palette <- c(
  "corrigendum" = "#B13507", "reconciled" = "#3C4E66", "kriston" = "#2C8465",
  "favours face-to-face" = "#883039", "favours eCBT" = "#00847E", "CI includes zero" = "#A5A5A5"
)

theme_owid <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#DDDDDD", linewidth = 0.4),
      axis.ticks = element_blank(), axis.line.x = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 4, colour = "#333333"),
      plot.subtitle = element_text(size = base_size, colour = "#555555", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size - 3, colour = "#777777", hjust = 0, margin = margin(t = 10)),
      plot.title.position = "plot", plot.caption.position = "plot",
      legend.position = "none", strip.text = element_text(hjust = 0, colour = "#333333", face = "bold"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

metric_labels <- c(
  post_smd = "Post-test SMD", chg_smd_r0.3 = "Change-score SMD, r = 0.3", chg_smd_r0.6 = "Change-score SMD, r = 0.6",
  smc_r0.3 = "SMC, r = 0.3", smc_r0.5 = "SMC, r = 0.5",
  smc_r0.7 = "SMC, r = 0.7", smc_r0.9 = "SMC, r = 0.9", adjusted_hybrid = "Adjusted where reported"
)
method_labels <- c(dl = "DL", reml = "REML", pm = "PM", dl_hksj = "DL + HKSJ", reml_hksj = "REML + HKSJ",
                   pm_hksj = "PM + HKSJ", `three-level` = "3-level", rve = "RVE (CR2)")

# Published pooled estimates, placed where each paper's OWN interpretation puts
# them on our axis (positive favours face-to-face CBT):
#   Original 2020: -1.73 [-2.72, -0.74], interpreted as eCBT MORE effective
#     -> favours eCBT -> -1.73 on our axis.
#   Addendum 2020: re-reads the same -1.73 as LESS change for eCBT (direction
#     reversed) -> +1.73 on our axis; without the outlier -0.79 [-1.60, 0.02]
#     -> +0.79 [-0.02, 1.60]; concludes "eCBT is as effective".
#   Corrigendum 2021: -0.92 [-1.76, -0.09], states direction and conclusions
#     unchanged (i.e. the addendum's reading) -> +0.92 [0.09, 1.76].
# All three divide mean differences by standard errors, not SDs (script 01).
published <- tibble(
  label = c("Original 2020, as the paper read it (eCBT more effective)",
            "Addendum 2020, same estimate re-read (less change for eCBT)",
            "Addendum 2020, outlier removed",
            "Corrigendum 2021 (text: 'direction unchanged')"),
  published_value = c(-1.73, -1.73, -0.79, -0.92),
  pub_lb = c(-2.72, -2.72, -1.60, -1.76), pub_ub = c(-0.74, -0.74, 0.02, -0.09),
  favours_as_interpreted = c("eCBT", "face-to-face", "face-to-face", "face-to-face"),
  source_quote = c(
    "Luo 2020: 'eCBT was more effective than face-to-face CBT at reducing depression symptom severity (SMD 1.73; 95% CI 2.72, 0.74)'",
    "Addendum 2020: 'the average amount of change was less for eCBT compared to face to face CBT ... estimated mean change of 1.73 (95% CI 2.72, 0.74)'",
    "Addendum 2020: without outlier -0.79 [-1.60, 0.02]",
    "Corrigendum 2021: 'results continued to be consistent and in the same direction as in the review ... revised analysis (-0.92, 95% CI -1.76, -0.09) ... did not change the results, direction, or the conclusions'"
  ),
  placement_note = c(
    "placed on the eCBT side because the paper interpreted it so",
    "placed on the face-to-face side per the addendum's re-reading of the same number",
    "as above",
    "same arithmetic sign as the original and the corrigendum's own Fig. 3 labels the axis 'eCBT (less mean change) / fCBT (more mean change)' with the note 'favoring fCBT', so it carries the addendum's reading; the corrigendum text nevertheless says the review's direction and conclusions are unchanged"
  )
) |>
  mutate(
    estimate = if_else(favours_as_interpreted == "eCBT", published_value, -published_value),
    ci_lb = if_else(favours_as_interpreted == "eCBT", pub_lb, -pub_ub),
    ci_ub = if_else(favours_as_interpreted == "eCBT", pub_ub, -pub_lb),
    text = sprintf("%s: %.2f here (printed as %.2f [%.2f, %.2f])",
                   label, estimate, published_value, pub_lb, pub_ub)
  )
write_csv(published, here("output", "tables", "03_published_estimates_on_our_axis.csv"))
published_colour <- "#6D3E91"

# 6a. specification curve with factor dashboard
source_labels <- c(corrigendum = "Corrigendum Table 1 as published", kriston = "Kriston et al. 2022 re-extraction", reconciled = "Reconciled extraction")
curve_df <- results |> mutate(data_source_label = unname(source_labels[as.character(data_source)]))
med_all <- median(curve_df$estimate)
y_top <- max(curve_df$ci_ub); y_bot <- min(curve_df$ci_lb)

p_curve <- ggplot(curve_df, aes(x = spec_rank, y = estimate)) +
  geom_hline(yintercept = 0, colour = "#333333", linewidth = 0.4) +
  geom_linerange(aes(ymin = ci_lb, ymax = ci_ub, colour = data_source), alpha = 0.18, linewidth = 0.25) +
  geom_point(aes(colour = data_source), size = 0.8) +
  geom_hline(yintercept = med_all, colour = "#555555", linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = nrow(curve_df) * 0.02, y = med_all, label = sprintf("median %.2f", med_all),
           hjust = 0, vjust = -0.5, size = 3.1, colour = "#555555") +
  annotate("text", x = nrow(curve_df) * 0.02, y = 0.42, label = "above zero: favours face-to-face CBT",
           hjust = 0, vjust = 0, size = 3.1, colour = "#777777") +
  annotate("text", x = nrow(curve_df) * 0.02, y = -0.42, label = "below zero: favours eCBT",
           hjust = 0, vjust = 1, size = 3.1, colour = "#777777") +
  # direct labels for the two data sources, stacked at the top left
  # data sources are identified by the coloured row labels of the dashboard's
  # "Data source" block, not by a legend inside the plotting area
  geom_hline(data = published, aes(yintercept = estimate), colour = published_colour, linewidth = 0.5, linetype = "dotted") +
  geom_text(data = published |> mutate(vj = if_else(estimate < 0 | abs(estimate - 0.79) < 0.01, 1.4, -0.5)),
            aes(x = nrow(curve_df) * 0.02, y = estimate, label = text, vjust = vj),
            hjust = 0, size = 3, colour = published_colour, inherit.aes = FALSE) +
  scale_colour_manual(values = owid_palette) +
  scale_x_continuous(limits = c(0, nrow(curve_df) + 1), expand = expansion(mult = 0.01)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.5), limits = c(min(published$estimate) - 0.2, max(published$estimate) + 0.15)) +
  labs(
    title = "eCBT versus face-to-face CBT: every reasonable analysis of the same 14 trials",
    subtitle = sprintf("%d distinct specifications, sorted by estimate. Pooled Hedges' g with 95%% CI. Positive favours face-to-face CBT.\nDotted lines: the published pooled estimates, each placed as its own paper interpreted it; all divided mean differences by standard errors instead of SDs.",
                       nrow(curve_df)),
    x = NULL, y = "Pooled effect (Hedges' g)"
  ) +
  theme_owid() + theme(axis.text.x = element_blank())

wright_post_labels <- c(as_published = "as published (corrigendum)", exclude = "excluded",
                        derived_borrowed_sd = "derived post, borrowed SD", change_smd = "change-score SMD",
                        not_applicable = "not applicable (SMC metric)", not_in_set = "not in study set")
wright_tp_labels <- c(as_published = "as published (corrigendum)", endpoint_itt = "endpoint, ITT",
                      week8_completer = "week 8, completers", not_applicable = "not applicable",
                      not_in_set = "not in study set")
inclusion_labels <- c(all_14 = "all 14", no_choi = "without Choi (PST)", no_moodgym = "without MoodGYM (Sethi x2)",
                      no_small_arms = "without arms n < 20", no_kalapatapu = "without Kalapatapu (Mohr subsample)")
instrument_labels <- c(primary = "primary", clinician_preferred = "clinician-rated preferred",
                       self_preferred = "self-report preferred", all_instruments = "all instruments (3-level / RVE)")

dash_long <- curve_df |>
  transmute(spec_rank, data_source,
            `Data source` = data_source_label,
            `Effect size metric` = unname(metric_labels[as.character(es_metric)]),
            `Instrument` = unname(instrument_labels[as.character(instrument)]),
            `Inclusion` = unname(inclusion_labels[as.character(inclusion)]),
            `Estimator` = unname(method_labels[as.character(ma_method)]),
            `Wright 2005 post-test` = unname(wright_post_labels[as.character(wright_post)]),
            `Wright 2005 time point` = unname(wright_tp_labels[as.character(wright_timepoint)])) |>
  pivot_longer(-c(spec_rank, data_source), names_to = "factor", values_to = "level")

factor_order <- c("Data source", "Effect size metric", "Instrument", "Inclusion", "Estimator",
                  "Wright 2005 post-test", "Wright 2005 time point")
dash_caption <- paste(
  "Data: Sanger et al. (2021) corrigendum Table 1 and the reconciled extraction from the primary studies (data/extraction_reconciled.csv).",
  "SMC = difference in standardized mean change with an assumed pre-post correlation r. Wright 2005 reports change scores only.",
  "Specifications that are identical because Wright 2005 is not in the study set are counted once.",
  "The original paper read -1.73 as eCBT more effective; the addendum re-read the same number as less change for eCBT; the corrigendum's -0.92 has the same sign and is placed with the addendum,",
         "although its text claims the original conclusions are unchanged. None is comparable to the multiverse: all divided mean differences by standard errors.",
  sep = "\n")

# one panel per factor, each with its own discrete y scale (levels ordered by
# median estimate, highest at the top); stacked with patchwork
dash_panel <- function(fct, last = FALSE) {
  d <- dash_long |> filter(factor == fct)
  lvl <- d |> left_join(curve_df |> select(spec_rank, estimate), by = "spec_rank") |>
    group_by(level) |> summarise(m = median(estimate), .groups = "drop") |> arrange(m) |> pull(level)
  d <- d |> mutate(level = factor(unname(level), levels = unname(lvl)))
  # the "Data source" block doubles as the colour legend: its row labels take the source colour
  label_cols <- if (fct == "Data source") {
    unname(owid_palette[d |> distinct(level, data_source) |> arrange(level) |> pull(data_source)])
  } else "#333333"
  ggplot(d, aes(x = spec_rank, y = level, colour = data_source)) +
    geom_point(shape = "|", size = 1.6, alpha = 0.6) +
    scale_colour_manual(values = owid_palette) +
    scale_x_continuous(limits = c(0, nrow(curve_df) + 1), expand = expansion(mult = 0.01)) +
    scale_y_discrete(labels = \(x) str_wrap(x, 34)) +
    labs(x = if (last) "Specifications, sorted by estimate" else NULL, y = NULL,
         title = fct, caption = if (last) dash_caption else NULL) +
    theme_owid(base_size = 9) +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(colour = label_cols, face = if (fct == "Data source") "bold" else "plain"),
          plot.title = element_text(size = 9, face = "bold", colour = "#333333", margin = margin(b = 1)),
          plot.margin = margin(2, 6, 2, 6))
}
dash_panels <- imap(factor_order, \(f, i) dash_panel(f, last = i == length(factor_order)))
dash_heights <- map_int(factor_order, \(f) n_distinct(dash_long$level[dash_long$factor == f])) + 1.2
p_dash <- wrap_plots(dash_panels, ncol = 1, heights = dash_heights)

fig_spec_curve <- wrap_plots(p_curve, p_dash, ncol = 1, heights = c(3.2, 3.8))
ggsave(here("output", "figures", "03_specification_curve.png"), fig_spec_curve, width = 11, height = 14, dpi = 300, bg = "white")
ggsave(here("output", "figures", "03_specification_curve.pdf"), fig_spec_curve, width = 11, height = 14)
saveRDS(list(p_curve = p_curve, p_dash = p_dash, dash_heights = dash_heights, owid_palette = owid_palette,
             curve_df = curve_df, published = published, dash_long = dash_long,
             theme_owid = theme_owid, published_colour = published_colour),
        here("output", "figures", "03_specification_curve_parts.rds"))

# 6b. raincloud of estimates by metric and data source
metric_order <- rev(unname(metric_labels))
rain_df <- results |>
  mutate(metric_label = metric_labels[as.character(es_metric)],
         y_num = as.numeric(factor(metric_label, levels = metric_order)),
         y_pos = y_num + case_when(data_source == "corrigendum" ~ 0.16, data_source == "kriston" ~ 0, TRUE ~ -0.16))
rain_medians <- rain_df |> group_by(metric_label, y_num, y_pos, data_source) |>
  summarise(median = median(estimate), n = n(), .groups = "drop")
pub_row <- length(metric_order) + 1.6
pub_df <- published |>
  mutate(y_pos = pub_row + c(0.36, 0.12, -0.12, -0.36),
         label_left = ci_ub > 2,
         label_x = if_else(label_left, ci_lb - 0.05, ci_ub + 0.05),
         label_hjust = if_else(label_left, 1, 0))
x_rng <- range(c(rain_df$estimate, 0, pub_df$ci_lb, pub_df$ci_ub))

p_rain <- ggplot(rain_df, aes(y = y_pos, x = estimate, colour = data_source, fill = data_source)) +
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.4) +
  stat_halfeye(aes(group = interaction(y_num, data_source)), adjust = 0.8, width = 0.55, .width = 0,
               point_colour = NA, alpha = 0.35, orientation = "horizontal") +
  geom_point(aes(group = interaction(y_num, data_source)), size = 0.7, alpha = 0.35,
             position = position_jitter(height = 0.05, width = 0)) +
  geom_text(data = rain_medians, aes(x = median, y = y_pos, label = sprintf("%.2f", median)),
            vjust = 1.8, size = 3, fontface = "bold", show.legend = FALSE) +
  annotate("text", x = x_rng[1], y = length(metric_order) + 0.75, label = source_labels["reconciled"],
           hjust = 0, size = 3.3, fontface = "bold", colour = owid_palette["reconciled"]) +
  annotate("text", x = x_rng[1], y = length(metric_order) + 0.55, label = source_labels["corrigendum"],
           hjust = 0, size = 3.3, fontface = "bold", colour = owid_palette["corrigendum"]) +
  annotate("text", x = x_rng[1], y = length(metric_order) + 0.35, label = source_labels["kriston"],
           hjust = 0, size = 3.3, fontface = "bold", colour = owid_palette["kriston"]) +
  geom_segment(data = pub_df, aes(x = ci_lb, xend = ci_ub, y = y_pos, yend = y_pos),
               colour = published_colour, linewidth = 0.5, inherit.aes = FALSE) +
  geom_point(data = pub_df, aes(x = estimate, y = y_pos), colour = published_colour, size = 2.2, inherit.aes = FALSE) +
  geom_text(data = pub_df, aes(x = label_x, y = y_pos, label = text, hjust = label_hjust), colour = published_colour,
            size = 2.8, inherit.aes = FALSE) +
  scale_colour_manual(values = owid_palette) + scale_fill_manual(values = owid_palette) +
  scale_x_continuous(limits = c(-3.4, 5.0), breaks = seq(-3, 3, 0.5)) +
  scale_y_continuous(breaks = c(seq_along(metric_order), pub_row),
                     labels = c(metric_order, "Published pooled estimates,\nas each paper interpreted them\n(SE-as-SD artefact)"),
                     expand = expansion(add = c(0.4, 0.6))) +
  labs(
    title = "Where the estimates fall, by effect size metric",
    subtitle = "Each point is one specification; labels give the median. Positive favours face-to-face CBT.\nTop rows: the published pooled estimates with 95% CI, each placed as its own paper interpreted it.",
    x = "Pooled effect (Hedges' g)", y = NULL,
    caption = paste("Distributions run across estimators, instrument rules, inclusion sets and Wright 2005 handling within each metric and data source.",
                    "Adjusted differences (Mohr 2012, Luxton 2016) exist only for the reconciled extraction.",
                    "Luo 2020 printed -1.73 and read it as eCBT more effective; the 2020 addendum re-read the same number as less change for eCBT (favours face-to-face).",
                    "The 2021 corrigendum's -0.92 has the same sign and is placed with the addendum, although its text claims the original conclusions are unchanged.",
                    "All published estimates used the standard error of the change score as the SD; see script 01.", sep = "\n")
  ) +
  theme_owid() + theme(panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(colour = "#DDDDDD", linewidth = 0.4))

ggsave(here("output", "figures", "03_estimate_raincloud.png"), p_rain, width = 14, height = 8, dpi = 300, bg = "white")
ggsave(here("output", "figures", "03_estimate_raincloud.pdf"), p_rain, width = 14, height = 8)

# ---------------------------------------------------------------------------
# 7. three-sentence range summary
# ---------------------------------------------------------------------------

n_pos <- sum(results$ci_lb > 0); n_neg <- sum(results$ci_ub < 0)
no_moodgym <- results |> filter(inclusion == "no_moodgym")
summary_text <- sprintf(paste(
  "Across %d distinct specifications (positive favours face-to-face CBT), the pooled Hedges' g ranges from %.2f to %.2f with a median of %.2f (interquartile range %.2f to %.2f); the 95%% CI includes zero in %d of %d specifications, %s, and no specification favours eCBT.",
  "The primary specification (reconciled data, post-test SMD, primary instruments, REML, all 14 studies) gives %.2f [%.2f, %.2f], and the largest single driver is inclusion: without the two MoodGYM trials the median drops to %.2f (range %.2f to %.2f), whereas data source, estimator, instrument rule and the handling of Wright 2005 shift medians by at most %.2f.",
  "No specification supports the original conclusion that eCBT is more effective, and none comes within 0.5 of the corrigendum's -0.92 (0.92 on this axis); the data are compatible with small differences in both directions, and the slight advantage for face-to-face CBT rests on the two small MoodGYM trials."),
  nrow(results), overall$min, overall$max, overall$median, overall$q25, overall$q75,
  sum(!results$ci_excludes_zero), nrow(results),
  if (n_pos == 1) "the one exception favours face-to-face" else sprintf("the %d exceptions all favour face-to-face", n_pos),
  primary_spec$estimate, primary_spec$ci_lb, primary_spec$ci_ub,
  median(no_moodgym$estimate), min(no_moodgym$estimate), max(no_moodgym$estimate),
  by_factor |> filter(factor != "inclusion") |> group_by(factor) |> summarise(spread = diff(range(median))) |> pull(spread) |> max()
)
if (n_neg > 0) summary_text <- paste(summary_text, sprintf("(Note: %d specifications have a CI entirely below zero; check before publishing this sentence.)", n_neg))

writeLines(c("# Multiverse range summary", "", paste("Sign convention:", sign_convention), "", summary_text),
           here("output", "03_multiverse_summary.md"))
cat("\n", summary_text, "\n")
cat("\n03 done. Outputs: output/tables/03_*.csv, output/figures/03_*.png|pdf, output/03_multiverse_summary.md\n")
