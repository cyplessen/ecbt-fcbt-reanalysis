# run_all.R
# Rebuilds every table, figure and report number from the raw inputs, in order.
# Requirements: see README.md. Set LUO_RLIB if metaMultiverse lives in a
# separate library. Takes a few minutes; 03 is the slow step.
scripts <- c(
  "00_extraction_from_screenshots.R",  # screenshots -> data/extraction_reconciled.csv
  "01_reproduce_original.R",           # SE-as-SD check; reproduces -1.73 / -0.79 / -0.92
  "02_correct_effects.R",              # post-test g, SMC, change-score SMD, baseline imbalance
  "99_check_port_against_python.R",    # metafor port vs the 2020 Python reference values
  "03_multiverse.R",                   # specification curve (metaMultiverse + pre/post extension)
  "04_per_study_comparison.R",         # per study x source sanity table
  "06_corrected_table.R",              # corrigendum Table 1 with SEM, SD of change, corrected SMDs
  "07_kriston_prior_work.R",           # Kriston et al. 2022: transcription, reproduction, source comparison
  "05_report_numbers.R",               # every number in the report -> report/numbers.tex
  "08_report_figure.R"                 # the report's display element -> report/fig_main.pdf
)
for (s in scripts) {
  cat("\n==>", s, "\n")
  source(file.path("R", s), local = new.env(), echo = FALSE)
}
cat("\nAll scripts ran. Build the report with: cd report && tinytex::latexmk('robustness_report.tex')\n")
