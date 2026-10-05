# 08_report_figure.R
# Reanalysis of Luo et al. (2020), eCBT vs face-to-face CBT for depression.
#
# The single display element of the JRR report: input and output in one figure.
#   Panel A (input): the corrigendum's own Table 1 per study, with the quantity
#     the authors entered as "SD" (the standard error of the mean change, SEM),
#     the SD of the change score, and three effect sizes: as published (mean
#     change / SEM), change-score SMD and post-test SMD. Source: script 06.
#   Panel B (output): the specification curve and dashboard from script 03.
# Everything is drawn by ggplot2 and composed with patchwork; the LaTeX file
# only includes output/figures/08_report_figure.pdf.
#
# Sign convention: positive favours face-to-face CBT in all effect-size columns
# (the published values are sign-reversed from the printed forest plot).

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(patchwork)
})

tab <- read_csv(here("output", "tables", "06_corrected_table1.csv"), show_col_types = FALSE)
pooled <- read_csv(here("output", "tables", "06_corrected_table1_pooled.csv"), show_col_types = FALSE)
parts <- readRDS(here("output", "figures", "03_specification_curve_parts.rds"))
theme_owid <- parts$theme_owid
owid_palette <- parts$owid_palette
# objects referenced lazily inside the saved ggplot layers
curve_df <- parts$curve_df; published <- parts$published; dash_long <- parts$dash_long
published_colour <- parts$published_colour
grey_text <- "#4D4D4D"

f1 <- function(x) sprintf("%.1f", x)
f2 <- function(x) sprintf("%.2f", x)

# ---------------------------------------------------------------------------
# Panel A: table as a ggplot
# ---------------------------------------------------------------------------

dl_row <- pooled |> filter(str_detect(quantity, "DerSimonian"))
i2_row <- pooled |> filter(str_detect(quantity, "I\\^2|I2"))
stopifnot(nrow(dl_row) == 1, nrow(i2_row) == 1)

rows <- tab |>
  arrange(study) |>
  transmute(
    study,
    n = sprintf("%d / %d", n_e, n_c),
    e_change = f1(change_e), e_sem = f1(sem_change_e), e_sd = f1(sd_change_e),
    c_change = f1(change_c), c_sem = f1(sem_change_c), c_sd = f1(sd_change_c),
    g_pub = sprintf("%s (%s)", f2(published_smd_sign_reversed), f2(published_se)),
    g_chg = sprintf("%s (%s)", f2(smd_change_r0.6), f2(smd_change_se)),
    g_post = sprintf("%s (%s)", f2(smd_post), f2(smd_post_se))
  )
pooled_rows <- tibble(
  study = c("Pooled (DL) [95% CI]", "I\u00b2"),
  n = c("", ""),
  e_change = "", e_sem = "", e_sd = "", c_change = "", c_sem = "", c_sd = "",
  g_pub = c(dl_row$published, i2_row$published),
  g_chg = c(dl_row$smd_change, i2_row$smd_change),
  g_post = c(dl_row$smd_post, i2_row$smd_post)
)
body <- bind_rows(rows, pooled_rows) |> mutate(row = rev(seq_len(n())), is_pooled = study %in% pooled_rows$study)

cols <- tribble(
  ~col,      ~x,    ~hjust, ~header,                 ~group,
  "study",   0.00,  0,      "Study",                 "",
  "n",       1.55,  0.5,    "n (e / f)",             "",
  "e_change",2.35,  1,      "\u0394",                "eCBT",
  "e_sem",   2.85,  1,      "SEM",                   "eCBT",
  "e_sd",    3.40,  1,      "SD\u0394",              "eCBT",
  "c_change",4.20,  1,      "\u0394",                "Face-to-face CBT",
  "c_sem",   4.70,  1,      "SEM",                   "Face-to-face CBT",
  "c_sd",    5.25,  1,      "SD\u0394",              "Face-to-face CBT",
  "g_pub",   6.45,  1,      "Published",             "Hedges' g (SE), positive favours face-to-face",
  "g_chg",   7.70,  1,      "Change SMD",            "Hedges' g (SE), positive favours face-to-face",
  "g_post",  8.95,  1,      "Post SMD",              "Hedges' g (SE), positive favours face-to-face"
)
cells <- body |>
  pivot_longer(-c(row, is_pooled), names_to = "col", values_to = "label") |>
  left_join(cols, by = "col") |>
  mutate(
    colour = case_when(col == "g_pub" ~ published_colour,
                       col %in% c("e_sem", "c_sem") ~ published_colour,
                       TRUE ~ "#222222"),
    face = if_else(is_pooled | col == "study", "bold", "plain")
  )
n_rows <- nrow(body)
header_y <- n_rows + 1
group_y <- n_rows + 2
groups <- cols |> filter(group != "") |> group_by(group) |> summarise(x = mean(x), xmin = min(x) - 0.45, xmax = max(x), .groups = "drop")

p_table <- ggplot() +
  geom_hline(yintercept = c(n_rows + 0.5, 2.5, 0.5), colour = "#BBBBBB", linewidth = 0.4) +
  geom_segment(data = groups, aes(x = xmin, xend = xmax, y = group_y - 0.45, yend = group_y - 0.45), colour = "#888888", linewidth = 0.3) +
  geom_text(data = groups, aes(x = (xmin + xmax) / 2, y = group_y, label = group), size = 4.2, fontface = "bold", colour = grey_text) +
  geom_text(data = cols, aes(x = x, y = header_y, label = header, hjust = hjust), size = 4.0, fontface = "bold", colour = grey_text) +
  geom_text(data = cells, aes(x = x, y = row, label = label, hjust = hjust, colour = colour, fontface = face), size = 3.9) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.05, 9.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.3, group_y + 0.6), expand = c(0, 0)) +
  labs(
    title = "A. Input: the corrigendum's Table 1, the quantity entered as \"SD\", and the effect sizes it implies",
    subtitle = paste0("\u0394 = mean change (pre minus post); SEM = sqrt(SD\u00b2pre/n + SD\u00b2post/n), the value entered as each arm's SD (purple);\n",
                      "SD\u0394 = SD of the change score, r = 0.6. Published = \u0394 / SEM as in the corrected forest plot, sign reversed. Wright 2005 as in the corrigendum.")
  ) +
  theme_owid(10) +
  # theme_owid() sets panel.grid.major.y explicitly, and an explicit child element
  # overrides a blanked parent, so the major y gridlines must be blanked by name
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 11.5, colour = grey_text, lineheight = 1.05),
        plot.margin = margin(4, 8, 2, 8))

# ---------------------------------------------------------------------------
# Panel B: specification curve (from script 03) with a compact dashboard
# ---------------------------------------------------------------------------
# For print the dashboard is condensed: implied levels ("not in study set",
# "not applicable") are dropped, Hartung-Knapp becomes its own yes/no row, and
# the estimator row lists DL, REML, PM, three-level and RVE. The full
# dashboard remains in output/figures/03_specification_curve.*.

p_curve <- parts$p_curve +
  labs(title = "B. Output: every reasonable analysis of the same 14 trials",
       subtitle = parts$p_curve$labels$subtitle) +
  theme(plot.title = element_text(size = 17), plot.subtitle = element_text(size = 11.5),
        axis.text = element_text(size = 11), axis.title = element_text(size = 12))

dash_long <- parts$dash_long
compact <- bind_rows(
  dash_long |> filter(factor %in% c("Data source", "Effect size metric", "Instrument", "Inclusion")),
  dash_long |> filter(factor == "Estimator") |>
    mutate(level = str_remove(level, " \\+ HKSJ")),
  dash_long |> filter(factor == "Estimator") |>
    mutate(factor = "Hartung-Knapp adjustment", level = if_else(str_detect(level, "HKSJ"), "yes", "no")),
  dash_long |> filter(factor == "Wright 2005 post-test", !str_detect(level, "^not ")),
  dash_long |> filter(factor == "Wright 2005 time point", !str_detect(level, "^not "))
)
factor_order <- c("Data source", "Effect size metric", "Instrument", "Inclusion", "Estimator",
                  "Hartung-Knapp adjustment", "Wright 2005 post-test", "Wright 2005 time point")
dash_panel <- function(fct, last = FALSE) {
  d <- compact |> filter(factor == fct)
  lvl <- d |> left_join(curve_df |> select(spec_rank, estimate), by = "spec_rank") |>
    group_by(level) |> summarise(m = median(estimate), .groups = "drop") |> arrange(m) |> pull(level)
  d <- d |> mutate(level = factor(unname(level), levels = unname(lvl)))
  # the "Data source" block doubles as the colour legend: its row labels take the source colour
  label_cols <- if (fct == "Data source") {
    unname(owid_palette[d |> distinct(level, data_source) |> arrange(level) |> pull(data_source)])
  } else "#333333"
  ggplot(d, aes(x = spec_rank, y = level, colour = data_source)) +
    geom_point(shape = "|", size = 2.2, alpha = 0.6) +
    scale_colour_manual(values = owid_palette) +
    scale_x_continuous(limits = c(0, nrow(curve_df) + 1), expand = expansion(mult = 0.01)) +
    scale_y_discrete(labels = \(x) str_wrap(x, 36)) +
    labs(x = if (last) "Specifications, sorted by estimate" else NULL, y = NULL, title = fct) +
    theme_owid(base_size = 11) +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10.5, colour = label_cols, face = if (fct == "Data source") "bold" else "plain"),
          axis.title.x = element_text(size = 11),
          plot.title = element_text(size = 11.5, face = "bold", colour = "#333333", margin = margin(b = 0)),
          plot.margin = margin(1, 6, 1, 6))
}
dash_panels <- imap(factor_order, \(f, i) dash_panel(f, last = i == length(factor_order)))
dash_heights <- map_int(factor_order, \(f) n_distinct(compact$level[compact$factor == f])) + 1.3
p_dash <- wrap_plots(dash_panels, ncol = 1, heights = dash_heights)

# ---------------------------------------------------------------------------
# Compose. Sized for print: the report includes the figure at \linewidth
# (about 150 mm), so this 11-inch canvas is scaled by about 0.54; type is set
# at 10.5 to 12 pt here so it prints at 6 to 6.5 pt. cairo_pdf embeds the
# Delta glyph, which the default pdf device's Helvetica lacks.
# ---------------------------------------------------------------------------
fig <- wrap_plots(free(p_table), p_curve, p_dash, ncol = 1, heights = c(4.7, 3.5, 7.8))
ggsave(here("output", "figures", "08_report_figure.pdf"), fig, width = 11, height = 16, device = cairo_pdf)
ggsave(here("output", "figures", "08_report_figure.png"), fig, width = 11, height = 16, dpi = 200, bg = "white")
# copy for the LaTeX include (report/ is the build directory)
file.copy(here("output", "figures", "08_report_figure.pdf"), here("report", "fig_main.pdf"), overwrite = TRUE)
cat("08 done: output/figures/08_report_figure.pdf|png, report/fig_main.pdf\n")
