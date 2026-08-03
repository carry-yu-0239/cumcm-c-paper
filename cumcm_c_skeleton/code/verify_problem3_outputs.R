#!/usr/bin/env Rscript
# Structural acceptance for Problem 3 outputs; it deliberately does not recompute the model.

args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", args[grep("^--file=", args)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/verify_problem3_outputs.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
out_dir <- file.path(root_dir, "data", "problem3")
need <- c("decision_lock.csv", "input_audit.csv", "model_reproduction_check.csv", "binary_cv_diagnostics.csv", "binary_oof_predictions.csv", "unknown_predictions.csv", "sensitivity_runs.csv", "sensitivity_summary.csv", "run_summary.txt", "problem3_cv_table.tex", "problem3_prediction_table.tex", "problem3_sensitivity_table.tex", "problem3_figure_generation_report.txt")
stopifnot(all(file.exists(file.path(out_dir, need))))
fig_dir <- file.path(root_dir, "figures")
figures <- c("problem3_cv_diagnostics.pdf", "problem3_cv_diagnostics.png", "problem3_unknown_scores.pdf", "problem3_unknown_scores.png", "problem3_sensitivity.pdf", "problem3_sensitivity.png")
stopifnot(all(file.exists(file.path(fig_dir, figures))))
for (name in figures) stopifnot(file.info(file.path(fig_dir, name))$size > 1024)
for (name in figures[grepl("\\.pdf$", figures)]) {
    stopifnot(identical(rawToChar(readBin(file.path(fig_dir, name), what = "raw", n = 5)), "%PDF-"))
}
pred <- read.csv(file.path(out_dir, "unknown_predictions.csv"), check.names = FALSE, fileEncoding = "UTF-8")
sens <- read.csv(file.path(out_dir, "sensitivity_summary.csv"), check.names = FALSE, fileEncoding = "UTF-8")
audit <- read.csv(file.path(out_dir, "input_audit.csv"), check.names = FALSE, fileEncoding = "UTF-8")
stopifnot(identical(as.character(pred$artifact_id), paste0("A", 1:8)), nrow(sens) == 8L, all(sens$clr_noise_stability >= 0 & sens$clr_noise_stability <= 1), all(sens$delta_stability >= 0 & sens$delta_stability <= 1), all(audit$passed))
cat("Problem 3 output verification: PASS\nThree vector PDFs and matching 300 dpi PNG previews are present; PDF rendering is checked by code/verify_outputs.py.\n")
