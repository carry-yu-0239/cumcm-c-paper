#!/usr/bin/env Rscript
# Structural and numeric-range validation for stable Problem 4 outputs.
# It does not reread inputs or recompute the model.

options(stringsAsFactors = FALSE)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/verify_problem4_outputs.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
out_dir <- file.path(root_dir, "data", "problem4")

assert_that <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
}

required <- c(
    "decision_lock.csv", "input_audit.csv", "component_diagnostics.csv",
    "pearson_high_potassium.csv", "pearson_lead_barium.csv", "spearman_high_potassium.csv", "spearman_lead_barium.csv",
    "difference_signed.csv", "difference_absolute.csv", "correlation_pairs.csv", "global_wilcoxon.csv",
    "method_robustness_summary.csv", "leave_one_artifact_out_runs.csv", "sensitivity_summary.csv", "run_summary.txt",
    "problem4_figure_generation_report.txt",
    "problem4_high_correlations_table.tex", "problem4_difference_table.tex", "problem4_global_test_table.tex",
    "problem4_robustness_table.tex", "pearson_high_potassium_matrix.tex", "pearson_lead_barium_matrix.tex"
)
assert_that(all(file.exists(file.path(out_dir, required))), "A required Problem 4 output is missing.")

read_matrix <- function(file, diagonal) {
    x <- read.csv(file.path(out_dir, file), check.names = FALSE, fileEncoding = "UTF-8")
    assert_that(nrow(x) == 14L && ncol(x) == 15L, paste("Expected 14 by 14 matrix in", file))
    value <- as.matrix(x[, -1, drop = FALSE])
    storage.mode(value) <- "double"
    assert_that(!any(!is.finite(value)), paste("Non-finite matrix entry in", file))
    assert_that(max(abs(value - t(value))) < 1e-12, paste("Matrix is not symmetric in", file))
    assert_that(all(value >= -2 - 1e-12 & value <= 2 + 1e-12), paste("Matrix entry out of range in", file))
    assert_that(max(abs(diag(value) - diagonal)) < 1e-12, paste("Unexpected diagonal in", file))
    value
}

pearson_k <- read_matrix("pearson_high_potassium.csv", 1)
pearson_pb <- read_matrix("pearson_lead_barium.csv", 1)
spearman_k <- read_matrix("spearman_high_potassium.csv", 1)
spearman_pb <- read_matrix("spearman_lead_barium.csv", 1)
signed <- read_matrix("difference_signed.csv", 0)
absolute <- read_matrix("difference_absolute.csv", 0)
assert_that(max(abs(absolute - abs(signed))) < 1e-12, "Difference matrices are inconsistent.")
assert_that(max(abs(signed - (pearson_k - pearson_pb))) < 1e-12, "Signed difference matrix is inconsistent with Pearson outputs.")

diagnostic <- read.csv(file.path(out_dir, "component_diagnostics.csv"), check.names = FALSE, fileEncoding = "UTF-8")
assert_that(nrow(diagnostic) == 28L, "Component diagnostics must contain 28 rows.")
assert_that(all(diagnostic$detection_rate >= 0 & diagnostic$detection_rate <= 1), "Detection rate is out of range.")
assert_that(all(diagnostic$detected_n <= diagnostic$n_points), "Detected count exceeds point count.")
assert_that(all(is.finite(diagnostic$clr_sd) & diagnostic$clr_sd > 0), "CLR standard deviation is invalid.")

pairs <- read.csv(file.path(out_dir, "correlation_pairs.csv"), check.names = FALSE, fileEncoding = "UTF-8")
assert_that(nrow(pairs) == 91L, "Correlation pair table must contain 91 rows.")
assert_that(all(pairs$component_j < pairs$component_k), "Pair order is not upper triangular.")
assert_that(!anyDuplicated(paste(pairs$component_j, pairs$component_k)), "Duplicate component pair detected.")
assert_that(all(abs(pairs$absolute_difference - abs(pairs$signed_difference)) < 1e-12), "Pairwise difference is inconsistent.")
assert_that(all(pairs$signed_difference >= -2 & pairs$signed_difference <= 2), "Signed pairwise difference is out of range.")
assert_that(all(pairs$absolute_difference >= 0 & pairs$absolute_difference <= 2), "Absolute pairwise difference is out of range.")
assert_that(identical(pairs$difference_rank, seq_len(91L)), "Difference ranking is not stable and complete.")

global <- read.csv(file.path(out_dir, "global_wilcoxon.csv"), check.names = FALSE, fileEncoding = "UTF-8")
assert_that(nrow(global) == 1L && global$n_component_pairs[1] == 91L && isTRUE(global$paired[1]),
            "Wilcoxon result is not the required paired 91-pair test.")
assert_that(global$p_value[1] >= 0 && global$p_value[1] <= 1, "Wilcoxon p-value is out of range.")

runs <- read.csv(file.path(out_dir, "leave_one_artifact_out_runs.csv"), check.names = FALSE, fileEncoding = "UTF-8")
sensitivity <- read.csv(file.path(out_dir, "sensitivity_summary.csv"), check.names = FALSE, fileEncoding = "UTF-8")
assert_that(!any(!is.finite(as.matrix(runs[, c("baseline_r", "loao_r", "change", "absolute_change")]))),
            "LOAO runs contain non-finite correlations.")
assert_that(nrow(sensitivity) == 182L, "LOAO summary must contain 91 pairs for each type.")
assert_that(all(sensitivity$sign_stability >= 0 & sensitivity$sign_stability <= 1), "LOAO sign stability is out of range.")
assert_that(all(table(sensitivity$glass_type) == 91L), "Each type must have 91 sensitivity summaries.")

robustness <- read.csv(file.path(out_dir, "method_robustness_summary.csv"), check.names = FALSE, fileEncoding = "UTF-8")
assert_that(nrow(robustness) == 2L, "Robustness summary must have two glass types.")
assert_that(all(robustness$pearson_spearman_sign_agreement >= 0 & robustness$pearson_spearman_sign_agreement <= 1),
            "Pearson--Spearman sign agreement is out of range.")

report <- c(
    "Problem 4 output verification: PASS",
    "Validated stable CSV and LaTeX-table structure only; no raw-input reread and no model recomputation occurred.",
    "Matrices: four 14x14 correlation matrices plus signed and absolute difference matrices.",
    "Pairs: 91 unique upper-triangular pairs; Wilcoxon: one paired whole-structure comparison.",
    sprintf("LOAO summaries=%d; high-potassium artifact removals=%d; lead-barium artifact removals=%d.",
            nrow(sensitivity), robustness$artifact_removals[robustness$glass_type == "高钾"],
            robustness$artifact_removals[robustness$glass_type == "铅钡"]),
    "Problem 4 figure files are required here; PDF rendering, PNG size, and 300 dpi checks are delegated to code/verify_outputs.py."
)
writeLines(report, file.path(out_dir, "problem4_verification_report.txt"), useBytes = TRUE)
cat(paste(report, collapse = "\n"), "\n")
