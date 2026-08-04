#!/usr/bin/env Rscript
# Renders LaTeX tables only from stable Problem 4 CSV outputs; it does not compute correlations.

options(stringsAsFactors = FALSE)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/make_problem4_tables.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
out_dir <- file.path(root_dir, "data", "problem4")
required <- c("correlation_pairs.csv", "global_wilcoxon.csv", "method_robustness_summary.csv",
              "pearson_high_potassium.csv", "pearson_lead_barium.csv")
stopifnot(all(file.exists(file.path(out_dir, required))))

pairs <- read.csv(file.path(out_dir, "correlation_pairs.csv"), check.names = FALSE, fileEncoding = "UTF-8")
global <- read.csv(file.path(out_dir, "global_wilcoxon.csv"), check.names = FALSE, fileEncoding = "UTF-8")
robust <- read.csv(file.path(out_dir, "method_robustness_summary.csv"), check.names = FALSE, fileEncoding = "UTF-8")

latex_name <- function(x) {
    map <- c(SiO2 = "SiO$_2$", Na2O = "Na$_2$O", K2O = "K$_2$O", CaO = "CaO", MgO = "MgO",
             Al2O3 = "Al$_2$O$_3$", Fe2O3 = "Fe$_2$O$_3$", CuO = "CuO", PbO = "PbO", BaO = "BaO",
             P2O5 = "P$_2$O$_5$", SrO = "SrO", SnO2 = "SnO$_2$", SO2 = "SO$_2$")
    unname(map[x])
}

strength_label <- function(x) unname(c(high = "强", medium = "中等", weak = "弱")[x])
direction_label <- function(x) unname(c(positive = "正向", negative = "负向", zero = "零")[x])

select_pairs <- function(value, strength) {
    chosen <- pairs[strength == "high", , drop = FALSE]
    if (!nrow(chosen)) chosen <- pairs
    head(chosen[order(-abs(chosen[[value]]), chosen$component_j, chosen$component_k), , drop = FALSE], 6L)
}

high_k <- select_pairs("pearson_high_potassium", pairs$high_potassium_strength)
high_pb <- select_pairs("pearson_lead_barium", pairs$lead_barium_strength)
strong <- rbind(
    data.frame(glass_type = "高钾", chemical_j = high_k$chemical_j, chemical_k = high_k$chemical_k,
               pearson_r = high_k$pearson_high_potassium, strength = high_k$high_potassium_strength,
               direction = high_k$high_potassium_direction),
    data.frame(glass_type = "铅钡", chemical_j = high_pb$chemical_j, chemical_k = high_pb$chemical_k,
               pearson_r = high_pb$pearson_lead_barium, strength = high_pb$lead_barium_strength,
               direction = high_pb$lead_barium_direction)
)
strong_rows <- vapply(seq_len(nrow(strong)), function(i) sprintf(
    "    %s & %s--%s & %.3f & %s & %s \\\\",
    strong$glass_type[i], latex_name(strong$chemical_j[i]), latex_name(strong$chemical_k[i]),
    strong$pearson_r[i], strength_label(strong$strength[i]), direction_label(strong$direction[i])
), character(1))
writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{两类玻璃中绝对Pearson相关系数最大的成分对}",
    "  \\label{tab:p4-high-correlations}", "  \\small", "  \\begin{tabular}{ccccc}", "    \\toprule",
    "    玻璃类型 & 成分对 & Pearson相关系数 & 强度 & 方向 \\\\", "    \\midrule", strong_rows,
    "    \\bottomrule", "  \\end{tabular}", "\\end{table}"
), file.path(out_dir, "problem4_high_correlations_table.tex"), useBytes = TRUE)

top_diff <- head(pairs[order(-pairs$absolute_difference, pairs$component_j, pairs$component_k), ], 10L)
diff_rows <- vapply(seq_len(nrow(top_diff)), function(i) sprintf(
    "    %d & %s--%s & %.3f & %.3f & %.3f & %.3f \\\\",
    i, latex_name(top_diff$chemical_j[i]), latex_name(top_diff$chemical_k[i]),
    top_diff$pearson_high_potassium[i], top_diff$pearson_lead_barium[i],
    top_diff$signed_difference[i], top_diff$absolute_difference[i]
), character(1))
writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{两类玻璃Pearson相关差异最大的10组成分对}",
    "  \\label{tab:p4-differences}", "  \\small", "  \\begin{tabular}{crrrrr}", "    \\toprule",
    "    排名 & 成分对 & 高钾 & 铅钡 & 有方向差异 & 绝对差异 \\\\", "    \\midrule", diff_rows,
    "    \\bottomrule", "  \\end{tabular}", "\\end{table}"
), file.path(out_dir, "problem4_difference_table.tex"), useBytes = TRUE)

writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{两类玻璃相关结构的配对Wilcoxon符号秩整体比较}",
    "  \\label{tab:p4-global-test}", "  \\small", "  \\begin{tabular}{rrrrr}", "    \\toprule",
    "    成分对数 & 统计量 & $p$值 & 位置差估计 & 置信区间 \\\\", "    \\midrule",
    sprintf("    %d & %.3f & %.4g & %.4f & [%.4f, %.4f] \\\\",
            global$n_component_pairs[1], global$statistic[1], global$p_value[1],
            global$estimated_location_shift[1], global$confidence_low[1], global$confidence_high[1]),
    "    \\bottomrule", "  \\end{tabular}", "\\end{table}"
), file.path(out_dir, "problem4_global_test_table.tex"), useBytes = TRUE)

robust_rows <- vapply(seq_len(nrow(robust)), function(i) sprintf(
    "    %s & %.1f\\%% & %.3f & %d & %.1f\\%% & %.1f\\%% \\\\",
    robust$glass_type[i], 100 * robust$pearson_spearman_sign_agreement[i],
    robust$mean_absolute_pearson_spearman_gap[i], robust$artifact_removals[i],
    100 * robust$median_loao_sign_stability[i], 100 * robust$minimum_loao_sign_stability[i]
), character(1))
writeLines(c(
    "\\begin{table}[H]", "  \\centering", "  \\caption{Pearson--Spearman对照与按文物删除稳健性}",
    "  \\label{tab:p4-robustness}", "  \\small", "  \\begin{tabular}{lrrrrr}", "    \\toprule",
    "    类型 & 符号一致率 & 平均绝对差 & 删除文物数 & LOAO中位符号稳定率 & LOAO最小符号稳定率 \\\\",
    "    \\midrule", robust_rows, "    \\bottomrule", "  \\end{tabular}", "\\end{table}"
), file.path(out_dir, "problem4_robustness_table.tex"), useBytes = TRUE)

write_matrix_table <- function(file, caption, label) {
    mat <- read.csv(file.path(out_dir, file), check.names = FALSE, fileEncoding = "UTF-8")
    headers <- vapply(names(mat)[-1], latex_name, character(1))
    matrix_rows <- vapply(seq_len(nrow(mat)), function(i) {
        paste0("    ", latex_name(mat$component_id[i]), " & ",
               paste(sprintf("%.2f", as.numeric(mat[i, -1])), collapse = " & "), " \\\\")
    }, character(1))
    output_file <- sub("\\.csv$", "_matrix.tex", file)
    writeLines(c(
        "\\begin{table}[H]", "  \\centering", paste0("  \\caption{", caption, "}"), paste0("  \\label{", label, "}"),
        "  \\scriptsize", "  \\resizebox{\\textwidth}{!}{%", paste0("  \\begin{tabular}{l", paste(rep("r", 14L), collapse = ""), "}"),
        "    \\toprule", paste0("    成分 & ", paste(headers, collapse = " & "), " \\\\"), "    \\midrule", matrix_rows,
        "    \\bottomrule", "  \\end{tabular}%", "  }", "\\end{table}"
    ), file.path(out_dir, output_file), useBytes = TRUE)
}

write_matrix_table("pearson_high_potassium.csv", "高钾玻璃14维CLR Pearson相关矩阵", "tab:p4-matrix-k")
write_matrix_table("pearson_lead_barium.csv", "铅钡玻璃14维CLR Pearson相关矩阵", "tab:p4-matrix-pb")
