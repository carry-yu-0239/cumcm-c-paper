#!/usr/bin/env Rscript
# Problem 3 authoritative numerical calculation entry point.
# It reuses the fixed Problem 2 inputs, emits stable CSV results, and creates no figures.

options(stringsAsFactors = FALSE)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/run_problem3.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
data_dir <- file.path(root_dir, "data")
snapshot_dir <- file.path(data_dir, "verification_snapshots")
p2_dir <- file.path(data_dir, "problem2")
out_dir <- file.path(data_dir, "problem3")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

component_id <- paste0("component_", 1:14)
clr_name <- paste0("CLR_", 1:14)
compact_features <- c("CLR_9", "CLR_3", "CLR_10", "CLR_12")
class_label <- c("铅钡玻璃", "高钾玻璃")

assert_that <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
write_csv <- function(x, file) write.csv(x, file.path(out_dir, file), row.names = FALSE, fileEncoding = "UTF-8")

standardize_fit <- function(x) {
    scale_value <- apply(x, 2, sd)
    scale_value[!is.finite(scale_value) | scale_value < 1e-12] <- 1
    list(center = colMeans(x), scale = scale_value)
}
standardize_apply <- function(x, fit) sweep(sweep(as.matrix(x), 2, fit$center, "-"), 2, fit$scale, "/")
one_hot <- function(y) cbind(class_1 = as.numeric(y == 1L), class_2 = as.numeric(y == 2L))

fit_pls2 <- function(x, y, ncomp) {
    x <- as.matrix(x); y <- as.matrix(y)
    xfit <- standardize_fit(x)
    xh <- standardize_apply(x, xfit)
    ymean <- colMeans(y); yh <- sweep(y, 2, ymean, "-")
    p <- ncol(xh); q <- ncol(yh)
    w <- matrix(0, p, ncomp); p_load <- matrix(0, p, ncomp); q_load <- matrix(0, q, ncomp)
    scores <- matrix(0, nrow(xh), ncomp); ss_y <- numeric(ncomp)
    for (a in seq_len(ncomp)) {
        sv <- svd(crossprod(xh, yh), nu = 1, nv = 0)
        wa <- sv$u[, 1]; ta <- drop(xh %*% wa); denom <- sum(ta ^ 2)
        assert_that(denom > 1e-12, "PLS component has zero score variance.")
        pa <- drop(crossprod(xh, ta) / denom); qa <- drop(crossprod(yh, ta) / denom)
        w[, a] <- wa; p_load[, a] <- pa; q_load[, a] <- qa; scores[, a] <- ta
        ss_y[a] <- denom * sum(qa ^ 2)
        xh <- xh - tcrossprod(ta, pa); yh <- yh - tcrossprod(ta, qa)
    }
    coefficients <- w %*% solve(crossprod(p_load, w), t(q_load))
    list(xfit = xfit, ymean = ymean, coefficients = coefficients, weights = w,
         loadings_x = p_load, loadings_y = q_load, scores = scores, ss_y = ss_y)
}
predict_pls2 <- function(fit, new_x) sweep(standardize_apply(new_x, fit$xfit) %*% fit$coefficients, 2, fit$ymean, "+")

classification_metrics <- function(actual, predicted) {
    recall <- vapply(1:2, function(g) sum(actual == g & predicted == g) / sum(actual == g), numeric(1))
    precision <- vapply(1:2, function(g) {
        den <- sum(predicted == g); if (den == 0) 0 else sum(actual == g & predicted == g) / den
    }, numeric(1))
    f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    c(accuracy = mean(actual == predicted), balanced_accuracy = mean(recall), macro_f1 = mean(f1))
}
rmsep <- function(actual, score) {
    y <- one_hot(actual)
    c(lead_barium = sqrt(mean((y[, 1] - score[, 1]) ^ 2)),
      high_potassium = sqrt(mean((y[, 2] - score[, 2]) ^ 2)),
      overall = sqrt(mean((y - score) ^ 2)))
}

cross_validate <- function(x, y, folds, ncomp) {
    score <- matrix(NA_real_, nrow(x), 2)
    for (fold in sort(unique(folds))) {
        test <- folds == fold
        fit <- fit_pls2(x[!test, , drop = FALSE], one_hot(y[!test]), ncomp)
        score[test, ] <- predict_pls2(fit, x[test, , drop = FALSE])
    }
    assert_that(!anyNA(score), "Every training record must receive one out-of-fold score.")
    list(score = score, class = max.col(score, ties.method = "first"))
}

to_clr <- function(raw, delta) {
    raw <- as.matrix(raw)
    totals <- rowSums(raw)
    assert_that(all(totals >= 85 & totals <= 105), "Sensitivity input contains an invalid composition sum.")
    closed <- 100 * raw / totals
    zero_count <- rowSums(closed == 0)
    positive_scale <- (100 - zero_count * delta) / 100
    replaced <- closed
    for (i in seq_len(nrow(replaced))) {
        replaced[i, closed[i, ] == 0] <- delta
        replaced[i, closed[i, ] > 0] <- closed[i, closed[i, ] > 0] * positive_scale[i]
    }
    assert_that(all(replaced > 0), "Near-zero replacement failed to produce positive components.")
    assert_that(max(abs(rowSums(replaced) - 100)) < 1e-10, "Replacement did not preserve closure.")
    z <- log(replaced / exp(rowMeans(log(replaced))))
    assert_that(max(abs(rowSums(z))) < 1e-10, "CLR rows must sum to zero.")
    z
}

required <- c(file.path(p2_dir, "plot_model_matrix.csv"), file.path(p2_dir, "grouped_cv_folds.csv"),
              file.path(p2_dir, "tree_rule.csv"), file.path(p2_dir, "plsda_binary_grouped_cv.csv"),
              file.path(out_dir, "unknown_clr_input.csv"), file.path(snapshot_dir, "sheet2_raw_snapshot.csv"),
              file.path(snapshot_dir, "sheet3_raw_snapshot.csv"))
assert_that(all(file.exists(required)), "Run export_problem3_inputs.m first and retain all fixed Problem 2 inputs.")

model <- read.csv(file.path(p2_dir, "plot_model_matrix.csv"), check.names = FALSE, fileEncoding = "UTF-8")
fold_table <- read.csv(file.path(p2_dir, "grouped_cv_folds.csv"), check.names = FALSE, fileEncoding = "UTF-8")
unknown <- read.csv(file.path(out_dir, "unknown_clr_input.csv"), check.names = FALSE, fileEncoding = "UTF-8")
raw_train <- read.csv(file.path(snapshot_dir, "sheet2_raw_snapshot.csv"), check.names = FALSE, fileEncoding = "UTF-8")
raw_unknown <- read.csv(file.path(snapshot_dir, "sheet3_raw_snapshot.csv"), check.names = FALSE, fileEncoding = "UTF-8")
tree <- read.csv(file.path(p2_dir, "tree_rule.csv"), check.names = FALSE, fileEncoding = "UTF-8")

assert_that(nrow(model) == 67L && length(unique(model$artifact_id)) == 56L, "Training input audit failed.")
assert_that(identical(sort(unique(model$class_code)), c(1L, 2L)), "Class codes must be 1 and 2.")
assert_that(all(model$glass_type[model$class_code == 1L] == "铅钡") && all(model$glass_type[model$class_code == 2L] == "高钾"), "Class labels mismatch.")
assert_that(all(clr_name %in% names(model)) && all(clr_name %in% names(unknown)), "Unexpected CLR schema.")
assert_that(nrow(unknown) == 8L && identical(as.character(unknown$artifact_id), paste0("A", 1:8)), "Unknown IDs must be A1--A8.")
assert_that(!any(!is.finite(as.matrix(model[, clr_name]))) && !any(!is.finite(as.matrix(unknown[, clr_name]))), "CLR data contain a non-finite value.")
assert_that(max(abs(rowSums(model[, clr_name]))) < 1e-10 && max(abs(rowSums(unknown[, clr_name]))) < 1e-10, "CLR row-sum audit failed.")
assert_that(all(tree$root_component == "CLR_9") && abs(tree$root_threshold[1] - 1.74966148981006) < 1e-12 && tree$node_count[1] == 3L, "Fixed tree rule audit failed.")

key_model <- paste(model$sample_name, model$artifact_id, sep = "|")
key_fold <- paste(fold_table$sample_name, fold_table$artifact_id, sep = "|")
folds <- fold_table$fold[match(key_model, key_fold)]
assert_that(!anyNA(folds) && identical(sort(unique(folds)), 1:5), "Fixed folds do not cover training data.")
artifact_fold_count <- aggregate(fold_table$fold, list(fold_table$artifact_id), function(x) length(unique(x)))
assert_that(all(artifact_fold_count$x == 1L), "An artifact crosses fixed CV folds.")

x_main <- as.matrix(model[, clr_name]); y <- as.integer(model$class_code)
main_cv <- cross_validate(x_main, y, folds, 1L)
main_metrics <- classification_metrics(y, main_cv$class)
reference <- read.csv(file.path(p2_dir, "plsda_binary_grouped_cv.csv"), check.names = FALSE)
reference_main <- reference[reference$ncomp == 1L, ]
reproduction <- data.frame(
    metric = c("accuracy", "balanced_accuracy", "macro_f1"),
    reference_value = c(reference_main$accuracy, reference_main$balanced_accuracy, reference_main$macro_f1),
    recomputed_value = unname(main_metrics[c("accuracy", "balanced_accuracy", "macro_f1")]),
    tolerance = 1e-12, stringsAsFactors = FALSE
)
reproduction$absolute_difference <- abs(reproduction$reference_value - reproduction$recomputed_value)
reproduction$passed <- reproduction$absolute_difference <= reproduction$tolerance
assert_that(all(reproduction$passed), "Problem 2 main-model reproduction failed; do not continue.")

cv_rows <- list()
oof_main <- NULL
for (ncomp in 1:5) {
    cv <- cross_validate(x_main, y, folds, ncomp)
    met <- classification_metrics(y, cv$class); err <- rmsep(y, cv$score)
    cv_rows[[length(cv_rows) + 1L]] <- data.frame(model = "main_14clr", ncomp = ncomp, t(met),
                                                   rmsep_lead_barium = err[1], rmsep_high_potassium = err[2], rmsep_overall = err[3])
    if (ncomp == 1L) oof_main <- cv
}
compact_cv_rows <- list()
x_compact <- as.matrix(model[, compact_features])
for (ncomp in 1:4) {
    cv <- cross_validate(x_compact, y, folds, ncomp)
    met <- classification_metrics(y, cv$class); err <- rmsep(y, cv$score)
    compact_cv_rows[[length(compact_cv_rows) + 1L]] <- data.frame(model = "compact_4clr", ncomp = ncomp, t(met),
                                                                   rmsep_lead_barium = err[1], rmsep_high_potassium = err[2], rmsep_overall = err[3])
}
cv_diagnostics <- do.call(rbind, c(cv_rows, compact_cv_rows))
write_csv(cv_diagnostics, "binary_cv_diagnostics.csv")
compact_choice <- cv_diagnostics[cv_diagnostics$model == "compact_4clr", ]
compact_choice <- compact_choice[order(-compact_choice$balanced_accuracy, compact_choice$ncomp), ][1, ]
compact_ncomp <- compact_choice$ncomp

main_fit <- fit_pls2(x_main, one_hot(y), 1L)
compact_fit <- fit_pls2(x_compact, one_hot(y), compact_ncomp)
main_score <- predict_pls2(main_fit, as.matrix(unknown[, clr_name]))
compact_score <- predict_pls2(compact_fit, as.matrix(unknown[, compact_features]))
main_class <- max.col(main_score, ties.method = "first")
compact_class <- max.col(compact_score, ties.method = "first")
tree_class <- ifelse(unknown$CLR_9 < tree$root_threshold[1], 2L, 1L)
weather <- raw_unknown$weather[match(as.character(unknown$artifact_id), as.character(raw_unknown$artifact_id))]
assert_that(!anyNA(weather), "Unknown weather display fields are incomplete.")

prediction <- data.frame(
    artifact_id = unknown$artifact_id, weather = weather,
    main_score_lead_barium = main_score[, 1], main_score_high_potassium = main_score[, 2],
    main_score_margin = abs(main_score[, 1] - main_score[, 2]), main_class_code = main_class,
    main_class_label = class_label[main_class], compact_selected_ncomp = compact_ncomp,
    compact_score_lead_barium = compact_score[, 1], compact_score_high_potassium = compact_score[, 2],
    compact_score_margin = abs(compact_score[, 1] - compact_score[, 2]), compact_class_code = compact_class,
    compact_class_label = class_label[compact_class], tree_clr_pbo = unknown$CLR_9,
    tree_threshold = tree$root_threshold[1], tree_distance = abs(unknown$CLR_9 - tree$root_threshold[1]),
    tree_class_code = tree_class, tree_class_label = class_label[tree_class], stringsAsFactors = FALSE
)
agreement_count <- (main_class == compact_class) + (main_class == tree_class) + (compact_class == tree_class)
prediction$agreement_count <- agreement_count
prediction$all_models_agree <- agreement_count == 3L
prediction$attention_reason <- ifelse(prediction$all_models_agree, "三种模型一致", "模型结论不完全一致，保留主模型并重点讨论")
write_csv(prediction, "unknown_predictions.csv")

noise_rows <- list(); set.seed(20260804L)
for (i in seq_len(nrow(unknown))) for (b in 1:100) {
    noise <- rnorm(14, 0, 0.03); noise <- noise - mean(noise)
    z <- as.numeric(unknown[i, clr_name]) + noise
    assert_that(abs(sum(z)) < 1e-10, "CLR perturbation broke the zero-sum constraint.")
    main_b <- predict_pls2(main_fit, matrix(z, nrow = 1, dimnames = list(NULL, clr_name)))
    compact_b <- predict_pls2(compact_fit, matrix(z[match(compact_features, clr_name)], nrow = 1, dimnames = list(NULL, compact_features)))
    tree_b <- ifelse(z[9] < tree$root_threshold[1], 2L, 1L)
    noise_rows[[length(noise_rows) + 1L]] <- data.frame(artifact_id = unknown$artifact_id[i], sensitivity_type = "clr_noise", scenario = "sd_0.03", replicate = b,
        main_class_code = max.col(main_b), compact_class_code = max.col(compact_b), tree_class_code = tree_b,
        main_score_margin = abs(main_b[1, 1] - main_b[1, 2]), stringsAsFactors = FALSE)
}
noise_runs <- do.call(rbind, noise_rows)

raw_train <- raw_train[raw_train$valid == 1L, ]
raw_train <- raw_train[match(model$sample_name, raw_train$sample_name), ]
raw_unknown <- raw_unknown[match(unknown$artifact_id, raw_unknown$artifact_id), ]
assert_that(!anyNA(raw_train$sample_name) && !anyNA(raw_unknown$artifact_id), "Sensitivity raw-input alignment failed.")
raw_train_x <- as.matrix(raw_train[, component_id]); raw_unknown_x <- as.matrix(raw_unknown[, component_id])
delta_rows <- list(); delta_errors <- numeric(3)
for (idx in seq_along(c(0.01, 0.02, 0.04))) {
    delta <- c(0.01, 0.02, 0.04)[idx]
    train_z <- to_clr(raw_train_x, delta); unknown_z <- to_clr(raw_unknown_x, delta)
    colnames(train_z) <- clr_name; colnames(unknown_z) <- clr_name
    if (abs(delta - 0.02) < 1e-12) {
        delta_errors[idx] <- max(abs(train_z - x_main), abs(unknown_z - as.matrix(unknown[, clr_name])))
        assert_that(delta_errors[idx] <= 1e-10, "The 0.02% CLR sensitivity reconstruction does not reproduce the baseline.")
    }
    fit_delta <- fit_pls2(train_z, one_hot(y), 1L)
    score_delta <- predict_pls2(fit_delta, unknown_z)
    class_delta <- max.col(score_delta, ties.method = "first")
    for (i in seq_len(nrow(unknown))) delta_rows[[length(delta_rows) + 1L]] <- data.frame(
        artifact_id = unknown$artifact_id[i], sensitivity_type = "near_zero", scenario = sprintf("delta_%.2f", delta), replicate = 1L,
        main_class_code = class_delta[i], compact_class_code = NA_integer_, tree_class_code = NA_integer_,
        main_score_margin = abs(score_delta[i, 1] - score_delta[i, 2]), stringsAsFactors = FALSE)
}
delta_runs <- do.call(rbind, delta_rows)
sensitivity_runs <- rbind(noise_runs, delta_runs)
write_csv(sensitivity_runs, "sensitivity_runs.csv")

sensitivity_summary <- do.call(rbind, lapply(seq_len(nrow(prediction)), function(i) {
    id <- prediction$artifact_id[i]
    nr <- noise_runs[noise_runs$artifact_id == id, ]
    dr <- delta_runs[delta_runs$artifact_id == id, ]
    data.frame(artifact_id = id, baseline_class = prediction$main_class_label[i],
        clr_noise_stability = mean(nr$main_class_code == prediction$main_class_code[i]),
        delta_stability = mean(dr$main_class_code == prediction$main_class_code[i]),
        minimum_main_margin = min(c(prediction$main_score_margin[i], nr$main_score_margin, dr$main_score_margin)),
        models_all_agree = prediction$all_models_agree[i], perturbation_flip_count = sum(nr$main_class_code != prediction$main_class_code[i]),
        attention_reason = prediction$attention_reason[i], stringsAsFactors = FALSE)
}))
write_csv(sensitivity_summary, "sensitivity_summary.csv")

oof_output <- data.frame(sample_name = model$sample_name, artifact_id = model$artifact_id, fold = folds, class_code = y,
    actual_label = class_label[y], score_lead_barium = oof_main$score[, 1], score_high_potassium = oof_main$score[, 2],
    predicted_class_code = oof_main$class, predicted_label = class_label[oof_main$class], correct = y == oof_main$class)
write_csv(oof_output, "binary_oof_predictions.csv")
parameter_output <- rbind(
    data.frame(model = "main_14clr", selected_ncomp = 1L, response_class = c("lead_barium", "high_potassium"),
               intercept = main_fit$ymean, stringsAsFactors = FALSE),
    data.frame(model = "compact_4clr", selected_ncomp = compact_ncomp, response_class = c("lead_barium", "high_potassium"),
               intercept = compact_fit$ymean, stringsAsFactors = FALSE)
)
write_csv(parameter_output, "binary_model_parameters.csv")
write_csv(reproduction, "model_reproduction_check.csv")
write_csv(data.frame(
    decision_key = c("main_model", "main_ncomp", "compact_model", "tree_role", "subclass_prediction", "rmsep_role"),
    decision_value = c("14-dimensional CLR PLS-DA", "1", "PbO,K2O,BaO,SrO CLR PLS-DA contrast", "explanatory only", "not performed", "diagnostic only"),
    source = c("Problem 3 locked plan", "Problem 2 grouped CV", "Problem 3 locked plan", "Problem 2 tree rule", "Problem statement scope", "Problem 3 locked plan"),
    status = "locked", stringsAsFactors = FALSE), "decision_lock.csv")
audit <- data.frame(
    check_item = c("training_rows", "unique_training_artifacts", "unknown_rows", "unknown_ids", "training_clr_row_sum", "unknown_clr_row_sum", "fixed_folds", "delta_0.02_reproduction", "problem2_main_reproduction"),
    expected = c("67", "56", "8", "A1--A8", "<1e-10", "<1e-10", "5 folds; no artifact split", "<=1e-10", "all metric differences <=1e-12"),
    actual = c(nrow(model), length(unique(model$artifact_id)), nrow(unknown), paste(unknown$artifact_id, collapse = ","),
               format(max(abs(rowSums(model[, clr_name]))), digits = 4), format(max(abs(rowSums(unknown[, clr_name]))), digits = 4),
               paste(sort(unique(folds)), collapse = ","), format(max(delta_errors), digits = 4), "passed"),
    passed = TRUE, details = c("fixed Problem 2 matrix", "fixed Problem 2 matrix", "exported U3_CLR", "ordered export audit", "all rows", "all rows", "fixed grouped folds", "raw snapshots independently reconstructed", "matches stored grouped CV"),
    stringsAsFactors = FALSE)
write_csv(audit, "input_audit.csv")

summary_lines <- c(
    "Problem 3 calculation: PASS",
    "Authoritative calculation: R script run_problem3.R.",
    "Unknown input: the existing U3_CLR/U3_ZCLR-derived CSV files passed the required export audit.",
    sprintf("Main 14-CLR PLS-DA reproduction: accuracy=%.4f; balanced accuracy=%.4f; macro-F1=%.4f.", main_metrics[1], main_metrics[2], main_metrics[3]),
    sprintf("Compact four-CLR contrast selected %d component(s), with grouped-CV balanced accuracy %.4f.", compact_ncomp, compact_choice$balanced_accuracy),
    sprintf("Unknown predictions: lead-barium=%d; high-potassium=%d; all three models agree for %d of 8 samples.", sum(main_class == 1L), sum(main_class == 2L), sum(prediction$all_models_agree)),
    sprintf("Sensitivity: 100 fixed-seed CLR perturbations per sample; 0.02%% raw-snapshot CLR reconstruction maximum error=%.3g.", max(delta_errors)),
    "No figures were generated in this run; figure production is intentionally deferred."
)
writeLines(summary_lines, file.path(out_dir, "run_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
