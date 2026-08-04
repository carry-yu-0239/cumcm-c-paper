#!/usr/bin/env Rscript
# Figure-only layer for Question 4. It reads stable CSV products from
# run_problem4.R and never rereads preprocessing snapshots or re-estimates a model.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- sub("^--file=", "", script_arg[1])
if (is.na(script_path) || !file.exists(script_path)) script_path <- "cumcm_c_skeleton/code/make_problem4_figures.R"
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data", "problem4")
fig_dir <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

required <- c(
    "pearson_high_potassium.csv", "pearson_lead_barium.csv",
    "difference_absolute.csv", "correlation_pairs.csv",
    "method_robustness_summary.csv", "sensitivity_summary.csv"
)
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing)) stop("Missing Question 4 plot inputs: ", paste(missing, collapse = ", "))

read_data <- function(name) read.csv(file.path(data_dir, name), check.names = FALSE, fileEncoding = "UTF-8")
read_matrix <- function(name, diagonal) {
    x <- read_data(name)
    stopifnot(nrow(x) == 14L, ncol(x) == 15L, identical(x$component_id, paste0("component_", 1:14)))
    result <- as.matrix(x[, -1, drop = FALSE])
    storage.mode(result) <- "double"
    stopifnot(!any(!is.finite(result)), max(abs(result - t(result))) < 1e-12,
              max(abs(diag(result) - diagonal)) < 1e-12)
    result
}

pearson_high <- read_matrix("pearson_high_potassium.csv", 1)
pearson_lead <- read_matrix("pearson_lead_barium.csv", 1)
difference_abs <- read_matrix("difference_absolute.csv", 0)
pairs <- read_data("correlation_pairs.csv")
robustness <- read_data("method_robustness_summary.csv")
sensitivity <- read_data("sensitivity_summary.csv")

stopifnot(
    nrow(pairs) == 91L,
    nrow(robustness) == 2L,
    nrow(sensitivity) == 182L,
    all(pairs$absolute_difference >= 0 & pairs$absolute_difference <= 2),
    all(sensitivity$sign_stability >= 0 & sensitivity$sign_stability <= 1)
)

components <- c("SiO2", "Na2O", "K2O", "CaO", "MgO", "Al2O3", "Fe2O3",
                "CuO", "PbO", "BaO", "P2O5", "SrO", "SnO2", "SO2")
font_family <- Sys.getenv("CUMCM_PLOT_FONT", unset = "Microsoft YaHei")
ink <- "#25343C"
grid_col <- "#FFFFFF"
blue <- "#2A628F"
orange <- "#C66B3D"
green <- "#4F886E"
neutral <- "#8A99A1"

open_device <- function(path, kind, width = 8.7, height = 7.9) {
    if (kind == "pdf") {
        grDevices::cairo_pdf(path, width = width, height = height, family = font_family, onefile = FALSE)
    } else {
        grDevices::png(path, width = round(width * 300), height = round(height * 300),
                       res = 300, type = "cairo")
    }
}

with_device <- function(stem, draw, width, height) {
    for (kind in c("pdf", "png")) {
        open_device(file.path(fig_dir, paste0(stem, ".", kind)), kind, width, height)
        draw()
        grDevices::dev.off()
    }
}

setup_plot <- function(mar = c(5.2, 5.4, 2.8, 3.1)) {
    par(mar = mar, mgp = c(2.0, 0.55, 0), family = font_family, las = 1)
}

map_colours <- function(value, limits, palette, n = 200L) {
    scaled <- pmax(limits[1], pmin(limits[2], value))
    index <- floor((scaled - limits[1]) / diff(limits) * (n - 1L)) + 1L
    grDevices::colorRampPalette(palette)(n)[index]
}

draw_colourbar <- function(limits, palette, ticks, label) {
    xleft <- 14.85
    xright <- 15.22
    y <- seq(1, 14, length.out = 201L)
    value <- seq(limits[2], limits[1], length.out = 200L)
    cols <- map_colours(value, limits, palette)
    rect(xleft, y[-length(y)], xright, y[-1], col = cols, border = NA)
    text(xright + 0.18, c(1, 7.5, 14), labels = sprintf("%.1f", c(limits[2], mean(limits), limits[1])),
         adj = 0, cex = .58, col = ink)
    text(xleft + .18, .48, labels = label, cex = .56, col = ink)
}

draw_heatmap <- function(mat, main, limits, palette, legend_label) {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    setup_plot()
    plot(NA, xlim = c(.5, 16.0), ylim = c(14.65, .35), xaxs = "i", yaxs = "i", axes = FALSE,
         xlab = "", ylab = "", bty = "n")
    axis(1, at = 1:14, labels = components, las = 2, tick = FALSE, cex.axis = .67, line = -.2)
    axis(2, at = 1:14, labels = components, las = 2, tick = FALSE, cex.axis = .67, line = -.2)
    for (row in 1:14) {
        for (column in 1:14) {
            value <- mat[row, column]
            white_text <- if (limits[1] < 0) abs(value) > 0.55 else value > 0.95
            rect(column - .5, row - .5, column + .5, row + .5,
                 col = map_colours(value, limits, palette), border = grid_col, lwd = .45)
            text(column, row, sprintf("%.2f", value), cex = .47,
                 col = ifelse(white_text, "white", ink))
        }
    }
    draw_colourbar(limits, palette, NULL, legend_label)
    title(main = main, cex.main = 1.03, col.main = ink, line = 1.1)
    mtext("固定原附件成分顺序；对角线保留但不参与成分对排序", side = 3, line = -.15, cex = .63, col = neutral)
    box(col = "#95A2A8")
}

draw_high <- function() {
    draw_heatmap(
        pearson_high, "高钾玻璃：14维 CLR Pearson 相关矩阵", c(-1, 1),
        c("#1D4E6D", "#5D96AE", "#DDEAF0", "#F7F6F3", "#EFCDB5", "#C66B3D", "#7A3026"),
        "Pearson r"
    )
}

draw_lead <- function() {
    draw_heatmap(
        pearson_lead, "铅钡玻璃：14维 CLR Pearson 相关矩阵", c(-1, 1),
        c("#1D4E6D", "#5D96AE", "#DDEAF0", "#F7F6F3", "#EFCDB5", "#C66B3D", "#7A3026"),
        "Pearson r"
    )
}

draw_difference <- function() {
    draw_heatmap(
        difference_abs, "两类玻璃 Pearson 相关的绝对差异", c(0, 2),
        c("#FCFCFB", "#F4E8DA", "#E9C7AF", "#D98E62", "#A94432"),
        "|Δr|"
    )
}

draw_robustness <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)

    setup_plot(c(4.5, 4.7, 2.7, .5))
    plot(pairs$pearson_high_potassium, pairs$spearman_high_potassium,
         xlim = c(-1, 1), ylim = c(-1, 1), pch = 16, col = grDevices::adjustcolor(blue, .72),
         xlab = "Pearson 相关系数", ylab = "Spearman 相关系数",
         main = "秩相关稳健性对照", bty = "n", cex = .70, cex.main = .95)
    points(pairs$pearson_lead_barium, pairs$spearman_lead_barium,
           pch = 17, col = grDevices::adjustcolor(orange, .72), cex = .70)
    abline(0, 1, lty = 2, col = neutral, lwd = 1.2)
    abline(h = 0, v = 0, lty = 3, col = "#D9E0E4")
    legend("topleft",
           c(sprintf("高钾：符号一致 %.1f%%", 100 * robustness$pearson_spearman_sign_agreement[robustness$glass_type == "高钾"]),
             sprintf("铅钡：符号一致 %.1f%%", 100 * robustness$pearson_spearman_sign_agreement[robustness$glass_type == "铅钡"])),
           pch = c(16, 17), col = c(blue, orange), bty = "n", cex = .70)
    mtext("每点为一组无序成分对；虚线为完全一致", side = 3, line = .25, cex = .60, col = ink)

    setup_plot(c(4.5, 4.7, 2.7, .5))
    high <- sort(sensitivity$sign_stability[sensitivity$glass_type == "高钾"])
    lead <- sort(sensitivity$sign_stability[sensitivity$glass_type == "铅钡"])
    x_high <- seq_along(high) / length(high)
    x_lead <- seq_along(lead) / length(lead)
    plot(x_high, high, ylim = c(0, 1.04), xlim = c(0, 1), pch = 16,
         col = grDevices::adjustcolor(blue, .72), xlab = "类型内91组成分对的累积比例",
         ylab = "LOAO 符号稳定率", main = "按文物删除敏感性", bty = "n",
         cex = .70, cex.main = .95)
    points(x_lead, lead, pch = 17, col = grDevices::adjustcolor(orange, .72), cex = .70)
    abline(h = 1, lty = 2, col = green, lwd = 1.15)
    legend("bottomleft",
           c(sprintf("高钾：最小 %.1f%%", 100 * min(high)),
             sprintf("铅钡：最小 %.1f%%", 100 * min(lead))),
           pch = c(16, 17), col = c(blue, orange), bty = "n", cex = .70)
    mtext("每次完整删除一件文物的全部采样点", side = 3, line = .25, cex = .60, col = ink)
}

with_device("problem4_high_potassium_pearson", draw_high, 8.70, 7.95)
with_device("problem4_lead_barium_pearson", draw_lead, 8.70, 7.95)
with_device("problem4_difference_heatmap", draw_difference, 8.70, 7.95)
with_device("problem4_robustness", draw_robustness, 8.70, 4.35)

report <- c(
    "Problem 4 figure generation: PASS",
    "Inputs: stable CSV products in data/problem4/ generated by code/run_problem4.R.",
    "Outputs: four Chinese cairo vector PDFs and matching 300 dpi PNG previews.",
    "Scope: this plotting script does not reread preprocessing snapshots or re-estimate a correlation matrix.",
    "Colour scales: Pearson heatmaps are fixed to [-1, 1]; the absolute-difference heatmap is fixed to [0, 2].",
    "Interpretation guardrail: low-detection components remain displayed but require cautious interpretation; all associations are non-causal."
)
writeLines(report, file.path(data_dir, "problem4_figure_generation_report.txt"), useBytes = TRUE)
cat(paste(report, collapse = "\n"), "\n")
