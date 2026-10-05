# mm_pre_post_extension.R
# Extension of metaMultiverse (cyplessen/metaMultiverse, v0.2.3) for
# pre/post (change-score) designs.
#
# metaMultiverse starts from precomputed yi / vi and treats study selection as
# "which" factors and estimator / dependency handling as "how" factors. When
# the raw data are arm-level pre/post means and SDs, the choice of effect-size
# metric (post-test SMD, standardized mean change with an assumed pre-post r,
# adjusted differences) is itself a "how" decision. This file adds it as one:
#
#   * es_metric        how the effect size is computed from the arm data
#   * wright_post      how a study with change scores but no post SD enters
#                      the post-test metric (Wright 2005 here; general rule:
#                      any study with post_sd_imputed = TRUE)
#   * wright_timepoint which reported change column is used for such a study
#
# The which-factor machinery, dependency handling (aggregate / modeled) and
# the estimator registry of metaMultiverse are used unchanged. Two functions
# are the public surface:
#
#   compute_pre_post_es(arm_data, es_metric, wright_post, wright_timepoint,
#                       wright_alt)  -> arm_data with yi, vi (rows may drop)
#   run_pre_post_multiverse(arm_data, which_factors, es_grid, ma_methods,
#                           dependencies, spec_filter, wright_alt)
#                       -> list(results, specifications, n_attempted, warnings)
#
# Extra estimators registered here (metafor): dl, dl_hksj, reml_hksj, pm_hksj.
# The package's own reml, pm, three-level and rve are reused.
#
# Sign convention of everything computed here: positive favours the second
# arm (c). In the Luo reanalysis e = eCBT, c = face-to-face, so positive
# favours face-to-face CBT.

suppressPackageStartupMessages({
  library(tidyverse)
  library(metafor)
  library(metaMultiverse)
})

new_ur <- getFromNamespace("new_universe_result", "metaMultiverse")
register_mm <- getFromNamespace("register_ma_method", "metaMultiverse")

# ---------------------------------------------------------------------------
# 1. extra estimators
# ---------------------------------------------------------------------------

mm_fit_metafor <- function(method, test, label) {
  force(method); force(test); force(label)
  function(data) {
    mod <- tryCatch(
      metafor::rma(yi = data$yi, vi = data$vi, method = method, test = test,
                   control = list(stepadj = 0.5, maxiter = 2000)),
      error = function(e) NULL
    )
    if (is.null(mod)) return(new_ur(NA_real_, NA_real_, NA_real_, NA_real_))
    out <- new_ur(mod$b, mod$ci.lb, mod$ci.ub, mod$pval)
    attr(out, "method") <- label
    out
  }
}

register_pre_post_estimators <- function() {
  deps <- c("select_max", "select_min", "aggregate")
  register_mm("dl",        mm_fit_metafor("DL",   "z",    "DL"),          dependencies = deps)
  register_mm("dl_hksj",   mm_fit_metafor("DL",   "knha", "DL + HKSJ"),   dependencies = deps)
  register_mm("reml_hksj", mm_fit_metafor("REML", "knha", "REML + HKSJ"), dependencies = deps)
  register_mm("pm_hksj",   mm_fit_metafor("PM",   "knha", "PM + HKSJ"),   dependencies = deps)
  invisible(metaMultiverse::list_ma_methods())
}

# ---------------------------------------------------------------------------
# 2. effect sizes from arm-level pre/post data
#
# arm_data columns (one row per study x instrument x data source):
#   study, n_e, n_c, n_e_post, n_c_post,
#   pre_e_m, pre_e_sd, pre_c_m, pre_c_sd, post_e_m, post_e_sd, post_c_m, post_c_sd,
#   change_e_m, change_e_sd, change_c_m, change_c_sd,
#   adj_diff, adj_diff_lb, adj_diff_ub, adj_ci_level,
#   post_sd_imputed (logical), instrument
# wright_alt: study, instrument, timepoint, change_e_m, change_e_sd,
#   change_c_m, change_c_sd  (alternative change columns)
# ---------------------------------------------------------------------------

parse_smc_r <- function(es_metric) {
  as.numeric(str_match(es_metric, "^smc_r([0-9.]+)$")[, 2])
}

parse_chg_r <- function(es_metric) {
  as.numeric(str_match(es_metric, "^chg_smd_r([0-9.]+)$")[, 2])
}

es_change_smd_imputed <- function(dat, r) {
  # SMD of change scores with SD_change imputed from pre and post SDs under a
  # pre-post correlation r (the effect size used by Kriston et al. 2022, r = 0.3).
  # Reported change SDs are used where they exist (Wright 2005).
  # f2f change minus eCBT change; positive = f2f improved more
  d <- dat |> mutate(
    chg_e = if_else(!is.na(change_e_m), change_e_m, pre_e_m - post_e_m),
    chg_c = if_else(!is.na(change_c_m), change_c_m, pre_c_m - post_c_m),
    sd_e = if_else(!is.na(change_e_sd), change_e_sd, sqrt(pre_e_sd^2 + post_e_sd^2 - 2 * r * pre_e_sd * post_e_sd)),
    sd_c = if_else(!is.na(change_c_sd), change_c_sd, sqrt(pre_c_sd^2 + post_c_sd^2 - 2 * r * pre_c_sd * post_c_sd))
  )
  es <- escalc("SMD", m1i = chg_c, sd1i = sd_c, n1i = n_c, m2i = chg_e, sd2i = sd_e, n2i = n_e, data = d)
  dat |> mutate(yi = as.numeric(es$yi), vi = as.numeric(es$vi))
}

apply_wright_timepoint <- function(dat, wright_timepoint, wright_alt) {
  if (is.null(wright_alt) || wright_timepoint %in% c("endpoint_itt", "as_published")) return(dat)
  alt <- wright_alt |> filter(timepoint == wright_timepoint) |>
    select(study, instrument, alt_e_m = change_e_m, alt_e_sd = change_e_sd,
           alt_c_m = change_c_m, alt_c_sd = change_c_sd)
  dat |>
    left_join(alt, by = c("study", "instrument")) |>
    mutate(
      swap = !is.na(alt_e_m),
      change_e_m = if_else(swap, alt_e_m, change_e_m),
      change_e_sd = if_else(swap, alt_e_sd, change_e_sd),
      change_c_m = if_else(swap, alt_c_m, change_c_m),
      change_c_sd = if_else(swap, alt_c_sd, change_c_sd),
      post_e_m = if_else(swap, pre_e_m - change_e_m, post_e_m),
      post_c_m = if_else(swap, pre_c_m - change_c_m, post_c_m),
      post_e_sd = if_else(swap & post_sd_imputed, change_e_sd, post_e_sd),
      post_c_sd = if_else(swap & post_sd_imputed, change_c_sd, post_c_sd)
    ) |>
    select(-starts_with("alt_"), -swap)
}

es_post_smd <- function(dat) {
  d <- dat |> mutate(n1 = coalesce(n_e_post, n_e), n2 = coalesce(n_c_post, n_c))
  es <- escalc("SMD", m1i = post_e_m, sd1i = post_e_sd, n1i = n1,
               m2i = post_c_m, sd2i = post_c_sd, n2i = n2, data = d)
  d |> mutate(yi = as.numeric(es$yi), vi = as.numeric(es$vi))
}

es_change_smd <- function(dat) {
  # standardized mean difference of change scores with REPORTED change SDs
  # (f2f change minus eCBT change; positive = f2f improved more)
  es <- escalc("SMD", m1i = change_c_m, sd1i = change_c_sd, n1i = n_c,
               m2i = change_e_m, sd2i = change_e_sd, n2i = n_e, data = dat)
  dat |> mutate(yi = as.numeric(es$yi), vi = as.numeric(es$vi))
}

es_smc <- function(dat, r) {
  # difference of standardized mean changes, (post - pre) / sd_pre per arm,
  # Becker (1988) variance; eCBT minus f2f so positive = less improvement in eCBT
  e <- escalc("SMCR", m1i = post_e_m, m2i = pre_e_m, sd1i = pre_e_sd, ni = n_e,
              ri = rep(r, nrow(dat)), data = dat)
  c <- escalc("SMCR", m1i = post_c_m, m2i = pre_c_m, sd1i = pre_c_sd, ni = n_c,
              ri = rep(r, nrow(dat)), data = dat)
  dat |> mutate(yi = as.numeric(e$yi) - as.numeric(c$yi),
                vi = as.numeric(e$vi) + as.numeric(c$vi))
}

es_adjusted <- function(dat) {
  # adjusted between-group difference at post (eCBT minus f2f, raw units)
  # standardized by the pooled post SD; SE from the reported CI
  z <- qnorm(1 - (1 - dat$adj_ci_level) / 2)
  sd_pool <- sqrt(((dat$n_e - 1) * dat$post_e_sd^2 + (dat$n_c - 1) * dat$post_c_sd^2) /
                    (dat$n_e + dat$n_c - 2))
  se_raw <- (dat$adj_diff_ub - dat$adj_diff_lb) / (2 * z)
  dat |> mutate(yi = adj_diff / sd_pool, vi = (se_raw / sd_pool)^2)
}

compute_pre_post_es <- function(arm_data, es_metric, wright_post = "not_applicable",
                                wright_timepoint = "endpoint_itt", wright_alt = NULL) {
  dat <- apply_wright_timepoint(arm_data, wright_timepoint, wright_alt)
  imputed <- dat$post_sd_imputed %in% TRUE

  if (es_metric == "post_smd" || es_metric == "adjusted_hybrid") {
    if (wright_post == "exclude") {
      dat <- dat[!imputed, ]
      out <- es_post_smd(dat)
    } else if (wright_post == "change_smd") {
      out <- bind_rows(
        es_post_smd(dat[!imputed, ]),
        es_change_smd(dat[imputed, ])
      )
    } else {
      # "derived_borrowed_sd" (reconciled) or "as_published" (corrigendum)
      out <- es_post_smd(dat)
    }
    if (es_metric == "adjusted_hybrid") {
      has_adj <- !is.na(out$adj_diff)
      if (any(has_adj)) {
        adj <- es_adjusted(out[has_adj, ])
        out <- bind_rows(out[!has_adj, ], adj)
      }
    }
  } else if (!is.na(parse_smc_r(es_metric))) {
    out <- es_smc(dat, parse_smc_r(es_metric))
  } else if (!is.na(parse_chg_r(es_metric))) {
    out <- es_change_smd_imputed(dat, parse_chg_r(es_metric))
  } else {
    stop("unknown es_metric: ", es_metric)
  }

  out |>
    filter(!is.na(yi), !is.na(vi), vi > 0) |>
    mutate(es_metric = es_metric, wright_post = wright_post,
           wright_timepoint = wright_timepoint) |>
    arrange(study, instrument)
}

# ---------------------------------------------------------------------------
# 3. run the multiverse with the effect-size how factors
#
# which_factors: a list passed to metaMultiverse::define_factors(data, ...),
#          or a function(dat) returning such a list (needed when custom groups
#          refer to levels that may be absent in some effect-size variants,
#          because define_factors validates group levels against the data)
# es_grid: data frame with columns es_metric, wright_post, wright_timepoint
#          (plus any columns used by spec_filter, e.g. data_source)
# spec_filter: function(specs, es_row, factor_info) -> specs, applied to the
#          package's specification grid before running (drops incompatible
#          combos); factor_info maps factor labels to wf_* column names
# ---------------------------------------------------------------------------

run_pre_post_multiverse <- function(arm_data, which_factors, es_grid, ma_methods,
                                    dependencies = c("aggregate", "modeled"),
                                    spec_filter = function(specs, es_row, factor_info) specs,
                                    wright_alt = NULL,
                                    k_smallest_ma = 3, verbose = TRUE) {
  options(metaMultiverse.k_smallest_ma = k_smallest_ma)
  results <- vector("list", nrow(es_grid))
  specs_all <- vector("list", nrow(es_grid))
  warnings_all <- character(0)

  for (i in seq_len(nrow(es_grid))) {
    g <- es_grid[i, ]
    dat <- compute_pre_post_es(arm_data, g$es_metric, g$wright_post, g$wright_timepoint, wright_alt) |>
      mutate(es_id = row_number()) |>
      as.data.frame()

    wf <- if (is.function(which_factors)) which_factors(dat) else which_factors
    setup <- suppressMessages(suppressWarnings(
      do.call(metaMultiverse::define_factors,
              c(list(metaMultiverse::check_data_multiverse(dat)), wf))
    ))
    factor_info <- setup$factors
    spec_out <- metaMultiverse::create_multiverse_specifications(
      setup, ma_methods = ma_methods, dependencies = dependencies
    )
    spec_out$specifications <- spec_filter(spec_out$specifications, g, factor_info) |>
      mutate(row_id = row_number())
    if (nrow(spec_out$specifications) == 0) next

    res <- suppressMessages(suppressWarnings(
      metaMultiverse::run_multiverse_analysis(spec_out, verbose = FALSE, progress = FALSE)
    ))
    if (verbose) cat(sprintf("  [%2d/%2d] %-16s %-20s %-16s specs %4d -> %4d\n",
                             i, nrow(es_grid), g$es_metric, g$wright_post, g$wright_timepoint,
                             nrow(spec_out$specifications), nrow(res$results)))

    # number of distinct studies per result (k in the package is n effect sizes)
    sets <- strsplit(res$results$set, ",")
    k_studies <- vapply(sets, function(ids) n_distinct(dat$study[dat$es_id %in% as.integer(ids)]), integer(1))
    studies_in_set <- vapply(sets, function(ids) paste(sort(unique(dat$study[dat$es_id %in% as.integer(ids)])), collapse = "; "), character(1))

    wf_names <- setNames(factor_info$label, factor_info$wf_internal)
    results[[i]] <- res$results |>
      as_tibble() |>
      # expand.grid in metaMultiverse yields factor columns; keep everything character
      mutate(across(where(is.factor), as.character)) |>
      rename(any_of(setNames(names(wf_names), wf_names))) |>
      mutate(es_metric = g$es_metric, wright_post = g$wright_post,
             wright_timepoint = g$wright_timepoint, k_studies = k_studies,
             studies_in_set = studies_in_set)
    specs_all[[i]] <- spec_out$specifications |>
      as_tibble() |>
      mutate(across(where(is.factor), as.character)) |>
      rename(any_of(setNames(names(wf_names), wf_names))) |>
      mutate(es_metric = g$es_metric, wright_post = g$wright_post,
             wright_timepoint = g$wright_timepoint)
    warnings_all <- c(warnings_all, res$multiverse_warnings)
  }

  list(
    results = list_rbind(results),
    specifications = list_rbind(specs_all),
    n_attempted = sum(map_int(specs_all, \(s) if (is.null(s)) 0L else nrow(s))),
    warnings = warnings_all
  )
}
