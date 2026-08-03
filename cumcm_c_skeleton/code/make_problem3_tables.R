#!/usr/bin/env Rscript
# Renders LaTeX tables only from stable Problem 3 CSV outputs; it does not fit a model.

args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", args[grep("^--file=", args)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/make_problem3_tables.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
out_dir <- file.path(root_dir, "data", "problem3")
stopifnot(file.exists(file.path(out_dir, "binary_cv_diagnostics.csv")), file.exists(file.path(out_dir, "unknown_predictions.csv")), file.exists(file.path(out_dir, "sensitivity_summary.csv")))

cv <- read.csv(file.path(out_dir, "binary_cv_diagnostics.csv"), check.names = FALSE)
pred <- read.csv(file.path(out_dir, "unknown_predictions.csv"), check.names = FALSE, fileEncoding = "UTF-8")
sens <- read.csv(file.path(out_dir, "sensitivity_summary.csv"), check.names = FALSE, fileEncoding = "UTF-8")
main_cv <- cv[cv$model == "main_14clr" & cv$ncomp == 1L, ]
compact_cv <- cv[cv$model == "compact_4clr" & cv$ncomp == pred$compact_selected_ncomp[1], ][1, ]

writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{两种PLS--DA的按文物分组验证与RMSEP诊断}", "  \\label{tab:p3-cv}", "  \\small", "  \\begin{tabular}{lccccc}", "    \\toprule", "    模型 & 潜变量数 & 准确率 & 平衡准确率 & 宏平均F1 & RMSEP \\\\", "    \\midrule",
    sprintf("    14维CLR--PLS--DA（主模型） & %d & %.4f & %.4f & %.4f & %.4f \\\\", main_cv$ncomp, main_cv$accuracy, main_cv$balanced_accuracy, main_cv$macro_f1, main_cv$rmsep_overall),
    sprintf("    四变量CLR--PLS--DA（对照） & %d & %.4f & %.4f & %.4f & %.4f \\\\", compact_cv$ncomp, compact_cv$accuracy, compact_cv$balanced_accuracy, compact_cv$macro_f1, compact_cv$rmsep_overall),
    "    \\bottomrule", "  \\end{tabular}", "\\end{table}", ""), file.path(out_dir, "problem3_cv_table.tex"), useBytes = TRUE)

pred_rows <- vapply(seq_len(nrow(pred)), function(i) sprintf("    %s & %s & %s & %.3f & %s & %s & %s & %.2f\\%% \\\\", pred$artifact_id[i], pred$weather[i], pred$main_class_label[i], pred$main_score_margin[i], pred$tree_class_label[i], pred$compact_class_label[i], ifelse(pred$all_models_agree[i], "3/3一致", "不完全一致"), 100 * sens$clr_noise_stability[match(pred$artifact_id[i], sens$artifact_id)]), character(1))
writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{未知玻璃样品的类型鉴别结果}", "  \\label{tab:p3-prediction}", "  \\small", "  \\begin{tabularx}{\\textwidth}{C{0.9cm} C{1.3cm} C{1.5cm} C{1.1cm} C{1.5cm} C{1.5cm} C{1.4cm} Y}", "    \\toprule", "    样品 & 风化状态 & 主模型类型 & 得分间隔 & 决策树类型 & 四变量类型 & 模型一致性 & CLR扰动稳定率 \\\\", "    \\midrule", pred_rows, "    \\bottomrule", "  \\end{tabularx}", "\\end{table}", ""), file.path(out_dir, "problem3_prediction_table.tex"), useBytes = TRUE)

sens_rows <- vapply(seq_len(nrow(sens)), function(i) sprintf("    %s & %s & %.2f\\%% & %.2f\\%% & %.3f & %d \\\\", sens$artifact_id[i], sens$baseline_class[i], 100 * sens$clr_noise_stability[i], 100 * sens$delta_stability[i], sens$minimum_main_margin[i], sens$perturbation_flip_count[i]), character(1))
writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{未知样品分类敏感性汇总}", "  \\label{tab:p3-sensitivity}", "  \\small", "  \\begin{tabular}{cccccc}", "    \\toprule", "    样品 & 基准类型 & CLR扰动稳定率 & 近零参数稳定率 & 最小得分间隔 & CLR扰动翻转次数 \\\\", "    \\midrule", sens_rows, "    \\bottomrule", "  \\end{tabular}", "\\end{table}", ""), file.path(out_dir, "problem3_sensitivity_table.tex"), useBytes = TRUE)
