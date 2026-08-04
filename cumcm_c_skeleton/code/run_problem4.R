#!/usr/bin/env Rscript
# Problem 4 authoritative calculation entry point.
# It consumes immutable preprocessing snapshots and writes stable CSV outputs.
# It does not redraw preprocessing, fit a classifier, or generate figures.

options(stringsAsFactors = FALSE)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/run_problem4.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
snapshot_dir <- file.path(root_dir, "data", "verification_snapshots")
out_dir <- file.path(root_dir, "data", "problem4")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

component_id <- paste0("component_", seq_len(14L))
clr_id <- paste0("CLR_", seq_len(14L))
chemical_name <- c("SiO2", "Na2O", "K2O", "CaO", "MgO", "Al2O3", "Fe2O3",
                   "CuO", "PbO", "BaO", "P2O5", "SrO", "SnO2", "SO2")

assert_that <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
}

write_csv <- function(x, file) {
    write.csv(x, file.path(out_dir, file), row.names = FALSE, fileEncoding = "UTF-8")
}

canonical_artifact_id <- function(x) sprintf("%02d", as.integer(as.character(x)))

classify_strength <- function(r) {
    ifelse(abs(r) >= 0.8, "high", ifelse(abs(r) >= 0.5, "medium", "weak"))
}

classify_direction <- function(r) {
    ifelse(r > 0, "positive", ifelse(r < 0, "negative", "zero"))
}

matrix_to_pairs <- function(mat) {
    index <- which(upper.tri(mat), arr.ind = TRUE)
    out <- data.frame(component_j = index[, "row"], component_k = index[, "col"])
    out[order(out$component_j, out$component_k), , drop = FALSE]
}

compute_correlation_matrix <- function(z, method) {
    result <- stats::cor(z, method = method, use = "everything")
    assert_that(identical(dim(result), c(14L, 14L)), "Correlation matrix is not 14 by 14.")
    assert_that(!any(!is.finite(result)), "Correlation matrix contains non-finite values.")
    assert_that(max(abs(result - t(result))) < 1e-12, "Correlation matrix is not symmetric.")
    assert_that(max(abs(diag(result) - 1)) < 1e-12, "Correlation matrix diagonal is not one.")
    assert_that(all(result >= -1 - 1e-12 & result <= 1 + 1e-12), "Correlation coefficient is outside [-1, 1].")
    result
}

matrix_to_csv <- function(mat) data.frame(component_id = component_id, mat, check.names = FALSE)

read_inputs <- function() {
    files <- file.path(snapshot_dir, c("sheet1_raw_snapshot.csv", "sheet2_raw_snapshot.csv", "matlab_key_results.csv"))
    assert_that(all(file.exists(files)), "Required preprocessing snapshots are missing; do not silently rerun preprocessing.")
    artifact <- read.csv(files[1], check.names = FALSE, fileEncoding = "UTF-8")
    raw <- read.csv(files[2], check.names = FALSE, fileEncoding = "UTF-8")
    clr <- read.csv(files[3], check.names = FALSE, fileEncoding = "UTF-8")
    assert_that(nrow(artifact) == 58L, "Expected 58 artifact records.")
    assert_that(nrow(raw) == 69L, "Expected 69 raw sampling-point records.")
    assert_that(nrow(clr) == 67L, "Expected 67 CLR sampling points.")
    assert_that(identical(names(raw)[5:18], component_id), "Raw component order does not match the original attachment.")
    assert_that(identical(names(clr)[6:19], clr_id), "CLR field order does not match the original attachment.")

    artifact$artifact_id <- canonical_artifact_id(artifact$artifact_id)
    raw$artifact_id <- canonical_artifact_id(raw$artifact_id)
    clr$artifact_id <- canonical_artifact_id(clr$artifact_id)
    assert_that(!anyDuplicated(artifact$artifact_id), "Artifact snapshot has duplicate artifact IDs.")
    assert_that(!anyDuplicated(raw$sample_name), "Raw snapshot has duplicate sample names.")
    assert_that(!anyDuplicated(clr$sample_name), "CLR snapshot has duplicate sample names.")
    assert_that(sum(raw$valid == 1L) == 67L, "Raw validity flag does not retain 67 points.")
    assert_that(!any(clr$artifact_id %in% c("15", "17")), "Invalid artifacts 15 or 17 entered the CLR input.")
    assert_that(all(clr$sample_name %in% raw$sample_name[raw$valid == 1L]), "CLR input does not exactly use valid raw points.")
    assert_that(all(raw$sample_name[raw$valid == 1L] %in% clr$sample_name), "A valid raw point is missing from CLR input.")
    assert_that(all(clr$artifact_id == raw$artifact_id[match(clr$sample_name, raw$sample_name)]), "Artifact IDs disagree between raw and CLR snapshots.")
    assert_that(!any(clr$robust_outlier != 0L), "Problem 4 must not silently exclude robust outliers.")

    artifact_index <- match(clr$artifact_id, artifact$artifact_id)
    assert_that(!anyNA(artifact_index), "Every CLR point must map to one artifact record.")
    model <- clr
    model$glass_type <- artifact$glass_type[artifact_index]
    raw_valid <- raw[match(model$sample_name, raw$sample_name), , drop = FALSE]
    assert_that(!anyNA(model$glass_type), "Glass type is missing after linkage.")
    counts <- as.integer(table(model$glass_type)[c("高钾", "铅钡")])
    assert_that(identical(counts, c(18L, 49L)), "Expected 18 high-potassium and 49 lead-barium points.")
    z <- as.matrix(model[, clr_id, drop = FALSE])
    storage.mode(z) <- "double"
    assert_that(!any(!is.finite(z)), "CLR input contains NA, NaN, or Inf.")
    assert_that(max(abs(rowSums(z))) < 1e-10, "CLR rows do not satisfy the zero-sum constraint.")
    list(artifact = artifact, raw = raw, raw_valid = raw_valid, model = model, z = z)
}

compute_component_diagnostics <- function(raw_valid, model) {
    result <- do.call(rbind, lapply(c("高钾", "铅钡"), function(type) {
        keep <- model$glass_type == type
        raw_group <- raw_valid[keep, component_id, drop = FALSE]
        clr_group <- model[keep, clr_id, drop = FALSE]
        data.frame(
            glass_type = type,
            component_id = seq_len(14L),
            chemical_name = chemical_name,
            n_points = sum(keep),
            detected_n = colSums(raw_group > 0),
            detection_rate = colMeans(raw_group > 0),
            clr_sd = vapply(clr_group, stats::sd, numeric(1)),
            stringsAsFactors = FALSE
        )
    }))
    result$interpretation_note <- ifelse(result$detection_rate < 0.5,
                                         "低检出率：保留计算，但相关解释需谨慎",
                                         "保留并纳入全部相关计算")
    assert_that(nrow(result) == 28L, "Component diagnostic must have 28 rows.")
    assert_that(all(result$detection_rate >= 0 & result$detection_rate <= 1), "Detection rate is outside [0, 1].")
    assert_that(all(result$detected_n <= result$n_points), "Detected count exceeds point count.")
    assert_that(all(is.finite(result$clr_sd) & result$clr_sd > 0), "A CLR coordinate has non-positive standard deviation.")
    result
}

run_leave_one_artifact_out <- function(model, baseline) {
    results <- list()
    position <- 1L
    for (type in c("高钾", "铅钡")) {
        group <- model[model$glass_type == type, , drop = FALSE]
        pairs <- matrix_to_pairs(baseline[[type]])
        for (artifact_id in sort(unique(group$artifact_id))) {
            retained <- group[group$artifact_id != artifact_id, , drop = FALSE]
            assert_that(nrow(retained) >= 3L, "LOAO removal leaves too few points for correlation.")
            loao <- compute_correlation_matrix(as.matrix(retained[, clr_id, drop = FALSE]), "pearson")
            base_r <- baseline[[type]][cbind(pairs$component_j, pairs$component_k)]
            loao_r <- loao[cbind(pairs$component_j, pairs$component_k)]
            results[[position]] <- data.frame(
                glass_type = type, removed_artifact = artifact_id,
                component_j = pairs$component_j, component_k = pairs$component_k,
                baseline_r = base_r, loao_r = loao_r, change = loao_r - base_r,
                absolute_change = abs(loao_r - base_r), sign_same = sign(loao_r) == sign(base_r)
            )
            position <- position + 1L
        }
    }
    result <- do.call(rbind, results)
    assert_that(!any(!is.finite(as.matrix(result[, c("baseline_r", "loao_r", "change", "absolute_change")]))),
                "LOAO output contains non-finite values.")
    result
}

summarize_loao <- function(runs) {
    grouped <- split(runs, interaction(runs$glass_type, runs$component_j, runs$component_k, drop = TRUE))
    result <- do.call(rbind, lapply(grouped, function(x) data.frame(
        glass_type = x$glass_type[1], component_j = x$component_j[1], component_k = x$component_k[1],
        baseline_r = x$baseline_r[1], max_absolute_change = max(x$absolute_change),
        median_absolute_change = stats::median(x$absolute_change), minimum_r = min(x$loao_r),
        maximum_r = max(x$loao_r), sign_stability = mean(x$sign_same)
    )))
    result <- result[order(result$glass_type, result$component_j, result$component_k), , drop = FALSE]
    rownames(result) <- NULL
    assert_that(nrow(result) == 182L, "LOAO summary must contain 91 pairs for each type.")
    assert_that(all(result$sign_stability >= 0 & result$sign_stability <= 1), "LOAO sign stability is outside [0, 1].")
    result
}

input <- read_inputs()
model <- input$model
diagnostics <- compute_component_diagnostics(input$raw_valid, model)
z_by_type <- lapply(c("高钾", "铅钡"), function(type) as.matrix(model[model$glass_type == type, clr_id, drop = FALSE]))
names(z_by_type) <- c("高钾", "铅钡")
pearson <- lapply(z_by_type, compute_correlation_matrix, method = "pearson")
spearman <- lapply(z_by_type, compute_correlation_matrix, method = "spearman")
for (type in names(pearson)) {
    colnames(pearson[[type]]) <- rownames(pearson[[type]]) <- chemical_name
    colnames(spearman[[type]]) <- rownames(spearman[[type]]) <- chemical_name
}

pairs <- matrix_to_pairs(pearson[["高钾"]])
pairs$chemical_j <- chemical_name[pairs$component_j]
pairs$chemical_k <- chemical_name[pairs$component_k]
pairs$pearson_high_potassium <- pearson[["高钾"]][cbind(pairs$component_j, pairs$component_k)]
pairs$pearson_lead_barium <- pearson[["铅钡"]][cbind(pairs$component_j, pairs$component_k)]
pairs$spearman_high_potassium <- spearman[["高钾"]][cbind(pairs$component_j, pairs$component_k)]
pairs$spearman_lead_barium <- spearman[["铅钡"]][cbind(pairs$component_j, pairs$component_k)]
pairs$signed_difference <- pairs$pearson_high_potassium - pairs$pearson_lead_barium
pairs$absolute_difference <- abs(pairs$signed_difference)
pairs$high_potassium_strength <- classify_strength(pairs$pearson_high_potassium)
pairs$lead_barium_strength <- classify_strength(pairs$pearson_lead_barium)
pairs$high_potassium_direction <- classify_direction(pairs$pearson_high_potassium)
pairs$lead_barium_direction <- classify_direction(pairs$pearson_lead_barium)
pairs$pearson_spearman_gap_high_potassium <- abs(pairs$pearson_high_potassium - pairs$spearman_high_potassium)
pairs$pearson_spearman_gap_lead_barium <- abs(pairs$pearson_lead_barium - pairs$spearman_lead_barium)
pairs <- pairs[order(-pairs$absolute_difference, pairs$component_j, pairs$component_k), , drop = FALSE]
pairs$difference_rank <- seq_len(nrow(pairs))
rownames(pairs) <- NULL
assert_that(nrow(pairs) == 91L, "Expected exactly 91 unordered component pairs.")
assert_that(all(pairs$absolute_difference == abs(pairs$signed_difference)), "Absolute difference does not equal the absolute signed difference.")
assert_that(all(pairs$signed_difference >= -2 & pairs$signed_difference <= 2), "Signed difference is outside [-2, 2].")
assert_that(all(pairs$absolute_difference >= 0 & pairs$absolute_difference <= 2), "Absolute difference is outside [0, 2].")

signed_matrix <- pearson[["高钾"]] - pearson[["铅钡"]]
absolute_matrix <- abs(signed_matrix)
wilcoxon <- stats::wilcox.test(pairs$pearson_high_potassium, pairs$pearson_lead_barium,
                               paired = TRUE, exact = FALSE, conf.int = TRUE)
global_wilcoxon <- data.frame(
    test_name = "paired_wilcoxon_signed_rank", n_component_pairs = nrow(pairs), paired = TRUE,
    statistic = unname(wilcoxon$statistic), p_value = wilcoxon$p.value,
    estimated_location_shift = if (is.null(wilcoxon$estimate)) NA_real_ else unname(wilcoxon$estimate),
    confidence_low = wilcoxon$conf.int[1], confidence_high = wilcoxon$conf.int[2],
    alternative = wilcoxon$alternative,
    interpretation_scope = "探索性整体比较；91个相关系数并非独立观测，不对单个成分对作显著性结论"
)

robustness <- data.frame(
    glass_type = c("高钾", "铅钡"), n_component_pairs = 91L,
    pearson_spearman_sign_agreement = c(
        mean(sign(pairs$pearson_high_potassium) == sign(pairs$spearman_high_potassium)),
        mean(sign(pairs$pearson_lead_barium) == sign(pairs$spearman_lead_barium))
    ),
    mean_absolute_pearson_spearman_gap = c(mean(pairs$pearson_spearman_gap_high_potassium),
                                           mean(pairs$pearson_spearman_gap_lead_barium)),
    median_absolute_pearson_spearman_gap = c(stats::median(pairs$pearson_spearman_gap_high_potassium),
                                             stats::median(pairs$pearson_spearman_gap_lead_barium))
)

loao_runs <- run_leave_one_artifact_out(model, pearson)
sensitivity <- summarize_loao(loao_runs)
robustness$artifact_removals <- vapply(robustness$glass_type, function(type) {
    length(unique(loao_runs$removed_artifact[loao_runs$glass_type == type]))
}, integer(1))
robustness$median_loao_sign_stability <- vapply(robustness$glass_type, function(type) {
    stats::median(sensitivity$sign_stability[sensitivity$glass_type == type])
}, numeric(1))
robustness$minimum_loao_sign_stability <- vapply(robustness$glass_type, function(type) {
    min(sensitivity$sign_stability[sensitivity$glass_type == type])
}, numeric(1))

write_csv(data.frame(
    item = c("analysis_unit", "primary_input", "primary_transform", "primary_method", "robustness_method",
             "difference_output", "global_test", "pairwise_p_values", "sensitivity", "component_order"),
    value = c("sampling_point", "matlab_key_results.csv", "CLR", "Pearson", "Spearman", "signed_and_absolute",
              "paired_wilcoxon_signed_rank", "not_produced", "leave_one_artifact_out", "original_attachment_order")
), "decision_lock.csv")
write_csv(data.frame(
    item = c("artifact_records", "raw_sampling_points", "valid_sampling_points", "valid_artifacts",
             "clr_fields", "raw_component_fields", "high_potassium_points", "lead_barium_points",
             "retained_robust_outliers", "invalid_artifacts_in_clr", "max_abs_clr_row_sum"),
    value = c(nrow(input$artifact), nrow(input$raw), nrow(model), length(unique(model$artifact_id)),
              length(clr_id), length(component_id), sum(model$glass_type == "高钾"), sum(model$glass_type == "铅钡"),
              sum(model$robust_outlier), sum(model$artifact_id %in% c("15", "17")), max(abs(rowSums(input$z))))
), "input_audit.csv")
write_csv(diagnostics, "component_diagnostics.csv")
write_csv(matrix_to_csv(pearson[["高钾"]]), "pearson_high_potassium.csv")
write_csv(matrix_to_csv(pearson[["铅钡"]]), "pearson_lead_barium.csv")
write_csv(matrix_to_csv(spearman[["高钾"]]), "spearman_high_potassium.csv")
write_csv(matrix_to_csv(spearman[["铅钡"]]), "spearman_lead_barium.csv")
write_csv(matrix_to_csv(signed_matrix), "difference_signed.csv")
write_csv(matrix_to_csv(absolute_matrix), "difference_absolute.csv")
write_csv(pairs, "correlation_pairs.csv")
write_csv(global_wilcoxon, "global_wilcoxon.csv")
write_csv(robustness, "method_robustness_summary.csv")
write_csv(loao_runs, "leave_one_artifact_out_runs.csv")
write_csv(sensitivity, "sensitivity_summary.csv")

summary_lines <- c(
    "Problem 4 calculation: PASS",
    "Authoritative calculation: R script run_problem4.R.",
    "Inputs: three immutable preprocessing snapshots; original workbook and preprocessing outputs were not modified.",
    sprintf("Valid sampling points=%d; high-potassium=%d; lead-barium=%d; valid artifacts=%d.",
            nrow(model), sum(model$glass_type == "高钾"), sum(model$glass_type == "铅钡"), length(unique(model$artifact_id))),
    sprintf("Pearson and Spearman matrices: 14 by 14 for each type; component pairs=%d.", nrow(pairs)),
    sprintf("Paired Wilcoxon signed-rank: statistic=%.3f; p=%.6g; exploratory whole-structure comparison only.",
            global_wilcoxon$statistic, global_wilcoxon$p_value),
    sprintf("LOAO: high-potassium artifacts=%d; lead-barium artifacts=%d; all removals retained finite correlations.",
            robustness$artifact_removals[robustness$glass_type == "高钾"], robustness$artifact_removals[robustness$glass_type == "铅钡"]),
    "No figures were generated by this numerical entry point; code/make_problem4_figures.R is the separate figure-only layer."
)
writeLines(summary_lines, file.path(out_dir, "run_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
