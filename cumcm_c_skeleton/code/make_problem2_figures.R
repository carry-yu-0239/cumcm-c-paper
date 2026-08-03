#!/usr/bin/env Rscript
# Figure-only layer for Question 2.  It reads stable CSV products from
# run_problem2.R and never rereads the workbook or re-estimates a model.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- sub("^--file=", "", script_arg[1])
if (is.na(script_path) || !file.exists(script_path)) script_path <- "cumcm_c_skeleton/code/make_problem2_figures.R"
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data", "problem2")
fig_dir <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

required <- c(
    "tree_rule.csv", "tree_grouped_cv.csv", "plsda_binary_grouped_cv.csv",
    "plsda_binary_display_scores.csv", "binary_model_metrics.csv",
    "kmeans_selection_metrics.csv", "kmeans_sensitivity_summary.csv",
    "high_potassium_cluster_pca_display.csv", "lead_barium_cluster_pca_display.csv",
    "high_potassium_subclass_plsda_scores.csv", "lead_barium_subclass_plsda_scores.csv"
)
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing)) stop("Missing Question 2 plot inputs: ", paste(missing, collapse = ", "))

read_data <- function(name) read.csv(file.path(data_dir, name), check.names = FALSE, fileEncoding = "UTF-8")
tree_rule <- read_data("tree_rule.csv")
tree_cv <- read_data("tree_grouped_cv.csv")
pls_cv <- read_data("plsda_binary_grouped_cv.csv")
binary_scores <- read_data("plsda_binary_display_scores.csv")
metrics <- read_data("binary_model_metrics.csv")
k_selection <- read_data("kmeans_selection_metrics.csv")
sensitivity <- read_data("kmeans_sensitivity_summary.csv")
high_pca <- read_data("high_potassium_cluster_pca_display.csv")
lead_pca <- read_data("lead_barium_cluster_pca_display.csv")
high_pls <- read_data("high_potassium_subclass_plsda_scores.csv")
lead_pls <- read_data("lead_barium_subclass_plsda_scores.csv")

stopifnot(nrow(binary_scores) == 67L, nrow(high_pca) == 18L, nrow(lead_pca) == 49L)

font_family <- Sys.getenv("CUMCM_PLOT_FONT", unset = "Microsoft YaHei")
ink <- "#25343C"
muted <- "#5F7F98"
accent <- "#C66B3D"
green <- "#5B8C74"
grid_col <- "#D9E0E4"
cluster_col <- c("#1D3E53", "#5F7F98", "#8D9CA3", "#C66B3D", "#5B8C74")
cluster_pch <- c(16, 17, 15, 18, 8)

open_device <- function(path, kind, width = 7.25, height = 4.45) {
    if (kind == "pdf") {
        grDevices::cairo_pdf(path, width = width, height = height, family = font_family, onefile = FALSE)
    } else {
        grDevices::png(path, width = round(width * 300), height = round(height * 300), res = 300, type = "cairo")
    }
}
with_device <- function(stem, draw, width = 7.25, height = 4.45) {
    for (kind in c("pdf", "png")) {
        open_device(file.path(fig_dir, paste0(stem, ".", kind)), kind, width, height)
        draw()
        grDevices::dev.off()
    }
}
metric_value <- function(name) metrics$value[match(name, metrics$metric)]
class_label <- function(x) ifelse(x == 1, "铅钡玻璃", "高钾玻璃")
cluster_label <- function(class_name, id) {
    prefix <- ifelse(class_name == "high_potassium", "H", "L")
    paste0(prefix, id)
}
setup_plot <- function(mar = c(4.0, 4.2, 2.2, 0.5)) {
    par(mar = mar, mgp = c(2.0, 0.55, 0), family = font_family, las = 1)
}

draw_tree <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    setup_plot(c(1.4, 0.7, 0.8, 0.7))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", bty = "n")
    rect(.27, .62, .73, .88, col = "#E8EEF2", border = muted, lwd = 1.4)
    text(.5, .80, "根节点：PbO 的 CLR 值", cex = .98, col = ink)
    text(.5, .70, sprintf("阈值：%.4f", tree_rule$root_threshold[1]), cex = .9, col = ink)
    segments(.5, .62, .23, .38, col = "#7E8E96", lwd = 1.45)
    segments(.5, .62, .77, .38, col = "#7E8E96", lwd = 1.45)
    text(.31, .53, "< 1.7497", cex = .83, col = muted)
    text(.70, .53, "≥ 1.7497", cex = .83, col = muted)
    rect(.05, .13, .41, .39, col = "#F2F5F6", border = muted, lwd = 1.25)
    rect(.59, .13, .95, .39, col = "#F8EDE8", border = accent, lwd = 1.25)
    text(.23, .29, "高钾玻璃", cex = 1.04, col = ink)
    text(.23, .20, "18 个有效采样点", cex = .82, col = ink)
    text(.77, .29, "铅钡玻璃", cex = 1.04, col = ink)
    text(.77, .20, "49 个有效采样点", cex = .82, col = ink)
    text(.5, .04, sprintf("信息增益树（3 个节点）；按文物分组5折平衡准确率 = %.3f",
                           metric_value("tree_grouped_cv_best_balanced_accuracy")), cex = .82, col = ink)
}

draw_binary_plsda <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    setup_plot(c(4.0, 4.3, 2.4, .4))
    groups <- sort(unique(binary_scores$class_code))
    cols <- c("#C66B3D", "#5F7F98")
    pchs <- c(16, 17)
    xr <- range(binary_scores$component_1); yr <- range(binary_scores$component_2)
    padx <- .10 * diff(xr); pady <- .12 * diff(yr)
    plot(binary_scores$component_1, binary_scores$component_2, type = "n",
         xlim = xr + c(-padx, padx), ylim = yr + c(-pady, pady), bty = "n",
         xlab = "PLS 成分 1（展示）", ylab = "PLS 成分 2（展示）",
         main = "二分类 PLS--DA 得分图", cex.main = .93)
    abline(h = 0, v = 0, lty = 3, col = grid_col)
    for (i in seq_along(groups)) {
        d <- binary_scores[binary_scores$class_code == groups[i], ]
        if (nrow(d) > 2) {
            hull <- chull(d$component_1, d$component_2)
            polygon(d$component_1[hull], d$component_2[hull], col = adjustcolor(cols[i], alpha.f = .10), border = NA)
        }
        points(d$component_1, d$component_2, pch = pchs[i], col = cols[i], cex = .92)
    }
    legend("topright", class_label(groups), pch = pchs, col = cols, bty = "n", cex = .76)
    mtext("二维图只用于展示；最终分类器选用 1 个潜变量", side = 3, line = .18, cex = .62, col = ink)

    setup_plot(c(4.0, 4.5, 2.4, .4))
    ylim <- c(0.75, 1.01)
    plot(pls_cv$ncomp, pls_cv$balanced_accuracy, type = "b", pch = 16, col = muted, lwd = 1.8,
         ylim = ylim, xaxt = "n", xlab = "潜变量数", ylab = "按文物分组5折指标",
         main = "PLS--DA 验证曲线", bty = "n", cex.main = .93)
    axis(1, at = pls_cv$ncomp)
    lines(pls_cv$ncomp, pls_cv$macro_f1, type = "b", pch = 17, col = accent, lwd = 1.6)
    abline(v = 1, lty = 2, col = green, lwd = 1.1)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
    legend("bottomright", c("平衡准确率", "宏平均 F1", "最终选择：1 成分"),
           pch = c(16, 17, NA), lty = c(1, 1, 2), col = c(muted, accent, green), bty = "n", cex = .72)
}

draw_k_selection_panel <- function(d, value, ylab, main) {
    d <- d[order(d$k), ]
    setup_plot(c(3.8, 4.4, 2.2, .4))
    vals <- d[[value]]
    plot(d$k, vals, type = "b", pch = 16, lwd = 1.7, col = muted, xaxt = "n", bty = "n",
         xlab = "聚类数 K", ylab = ylab, main = main, cex.main = .86)
    axis(1, at = d$k)
    k0 <- unique(d$selected_k)
    points(k0, vals[match(k0, d$k)], pch = 21, bg = accent, col = accent, cex = 1.55, lwd = 1.1)
    text(k0, vals[match(k0, d$k)], labels = paste0(" 选K=", k0), pos = 4, cex = .69, col = ink)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
}
draw_k_selection <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(2, 2), family = font_family)
    high <- k_selection[k_selection$class_name == "high_potassium", ]
    lead <- k_selection[k_selection$class_name == "lead_barium", ]
    draw_k_selection_panel(high, "withinss", "组内平方和", "高钾：肘部诊断")
    draw_k_selection_panel(high, "average_silhouette", "平均轮廓系数", "高钾：分离度诊断")
    draw_k_selection_panel(lead, "withinss", "组内平方和", "铅钡：肘部诊断")
    draw_k_selection_panel(lead, "average_silhouette", "平均轮廓系数", "铅钡：分离度诊断")
}

draw_cluster_projection_panel <- function(d, title) {
    ids <- sort(unique(d$cluster_id))
    setup_plot(c(4.0, 4.3, 2.4, .4))
    xr <- range(d$pc1); yr <- range(d$pc2)
    padx <- .10 * diff(xr); pady <- .12 * diff(yr)
    plot(d$pc1, d$pc2, type = "n", xlim = xr + c(-padx, padx), ylim = yr + c(-pady, pady), bty = "n",
         xlab = sprintf("PC1（解释 %.1f%%）", 100 * d$pc1_explained[1]),
         ylab = sprintf("PC2（解释 %.1f%%）", 100 * d$pc2_explained[1]), main = title, cex.main = .90)
    abline(h = 0, v = 0, lty = 3, col = grid_col)
    for (id in ids) {
        q <- d[d$cluster_id == id, ]
        points(q$pc1, q$pc2, pch = cluster_pch[id], col = cluster_col[id], cex = 1.03)
    }
    legend("topright", cluster_label(d$class_name[1], ids), pch = cluster_pch[ids], col = cluster_col[ids], bty = "n", cex = .70)
    mtext("PCA 仅用于显示已固定的 K-means 划分", side = 3, line = .18, cex = .62, col = ink)
}
draw_kmeans_clusters <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    draw_cluster_projection_panel(high_pca, "高钾玻璃：K-means 亚类")
    draw_cluster_projection_panel(lead_pca, "铅钡玻璃：K-means 亚类")
}

draw_subclass_pls_panel <- function(d, title) {
    ids <- sort(unique(d$cluster_id))
    setup_plot(c(3.6, 4.0, 2.15, .25))
    par(cex.axis = .72, cex.lab = .72)
    xr <- range(d$component_1); yr <- range(d$component_2)
    padx <- .10 * diff(xr); pady <- .12 * diff(yr)
    plot(d$component_1, d$component_2, type = "n", xlim = xr + c(-padx, padx), ylim = yr + c(-pady, pady), bty = "n",
         xlab = "PLS 成分 1", ylab = "PLS 成分 2", main = title, cex.main = .78)
    abline(h = 0, v = 0, lty = 3, col = grid_col)
    for (id in ids) {
        q <- d[d$cluster_id == id, ]
        points(q$component_1, q$component_2, pch = cluster_pch[id], col = cluster_col[id], cex = 1.03)
    }
    legend("topright", cluster_label(d$class_name[1], ids), pch = cluster_pch[ids], col = cluster_col[ids], bty = "n", cex = .60)
    mtext("描述同一数据上的 K-means 簇，不构成独立泛化验证", side = 3, line = .05, cex = .48, col = ink)
}
draw_subclass_plsda <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    draw_subclass_pls_panel(high_pls, "高钾亚类的 PLS--DA 得分")
    draw_subclass_pls_panel(lead_pls, "铅钡亚类的 PLS--DA 得分")
}

draw_sensitivity_panel <- function(class_name, title) {
    d <- sensitivity[sensitivity$class_name == class_name, ]
    order_key <- c("initialization", "top4_features", "top6_features", "zclr_noise_sd_0.03")
    d <- d[match(order_key, d$scenario, nomatch = 0L), ]
    labels <- c("单起点初始化", "前四变量", "前六变量", "CLR坐标加噪声")
    y <- rev(seq_len(nrow(d)))
    setup_plot(c(3.7, 6.0, 2.4, .8))
    par(cex.axis = .68, cex.lab = .75)
    plot(d$mean_adjusted_rand_index, y, xlim = c(0, 1.03), ylim = c(.5, nrow(d) + .5), yaxt = "n", bty = "n",
         pch = 16, col = muted, xlab = "相对基准划分的 ARI", ylab = "", main = title, cex.main = .82)
    segments(d$min_adjusted_rand_index, y, d$mean_adjusted_rand_index, y, col = "#9AA9B1", lwd = 2.2)
    points(d$mean_adjusted_rand_index, y, pch = 16, col = muted, cex = 1.05)
    axis(2, at = y, labels = labels, las = 1, cex.axis = .64)
    abline(v = 1, lty = 2, col = accent)
    text(d$mean_adjusted_rand_index, y, sprintf("均值 %.3f", d$mean_adjusted_rand_index), pos = 2, offset = .28, cex = .56, col = ink)
}
draw_sensitivity <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    draw_sensitivity_panel("high_potassium", "高钾：亚类敏感性")
    draw_sensitivity_panel("lead_barium", "铅钡：亚类敏感性")
}

with_device("problem2_decision_tree", draw_tree, 7.25, 3.55)
with_device("problem2_binary_plsda", draw_binary_plsda, 7.25, 3.85)
with_device("problem2_kmeans_selection", draw_k_selection, 7.25, 6.35)
with_device("problem2_kmeans_clusters", draw_kmeans_clusters, 7.25, 3.85)
with_device("problem2_subclass_plsda", draw_subclass_plsda, 7.25, 4.15)
with_device("problem2_kmeans_sensitivity", draw_sensitivity, 7.25, 3.90)

report <- c(
    "Question 2 figure generation: PASS",
    "Inputs: stable CSV products in data/problem2/ generated by code/run_problem2.R.",
    "Outputs: six cairo vector PDFs and matching 300 dpi PNG previews.",
    "Scope: the plotting script does not read the workbook or fit a classification, clustering, or projection model.",
    "Interpretation guardrail: two-dimensional PLS--DA and PCA panels are displays; binary validation remains grouped by artifact, whereas subclass score plots are descriptive of K-means labels."
)
writeLines(report, file.path(data_dir, "problem2_figure_generation_report.txt"), useBytes = TRUE)
cat(paste(report, collapse = "\n"), "\n")
