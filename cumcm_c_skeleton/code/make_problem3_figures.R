#!/usr/bin/env Rscript
# Figure-only layer for Question 3. It reads stable CSV products from
# run_problem3.R and never rereads the workbook or re-estimates a model.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- sub("^--file=", "", script_arg[1])
if (is.na(script_path) || !file.exists(script_path)) script_path <- "cumcm_c_skeleton/code/make_problem3_figures.R"
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data", "problem3")
fig_dir <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

required <- c("binary_cv_diagnostics.csv", "unknown_predictions.csv", "sensitivity_summary.csv")
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing)) stop("Missing Question 3 plot inputs: ", paste(missing, collapse = ", "))

read_data <- function(name) read.csv(file.path(data_dir, name), check.names = FALSE, fileEncoding = "UTF-8")
cv <- read_data("binary_cv_diagnostics.csv")
pred <- read_data("unknown_predictions.csv")
sens <- read_data("sensitivity_summary.csv")

stopifnot(
    identical(as.character(pred$artifact_id), paste0("A", 1:8)),
    identical(as.character(sens$artifact_id), paste0("A", 1:8)),
    all(pred$all_models_agree),
    all(sens$clr_noise_stability >= 0 & sens$clr_noise_stability <= 1),
    all(sens$delta_stability >= 0 & sens$delta_stability <= 1)
)

font_family <- Sys.getenv("CUMCM_PLOT_FONT", unset = "Microsoft YaHei")
ink <- "#25343C"
muted <- "#5F7F98"
accent <- "#C66B3D"
green <- "#5B8C74"
grid_col <- "#D9E0E4"
light_blue <- "#E8EEF2"
light_orange <- "#F8EDE8"

open_device <- function(path, kind, width = 7.25, height = 4.45) {
    if (kind == "pdf") {
        grDevices::cairo_pdf(path, width = width, height = height, family = font_family, onefile = FALSE)
    } else {
        grDevices::png(path, width = round(width * 300), height = round(height * 300),
                       res = 300, type = "cairo")
    }
}

with_device <- function(stem, draw, width = 7.25, height = 4.45) {
    for (kind in c("pdf", "png")) {
        open_device(file.path(fig_dir, paste0(stem, ".", kind)), kind, width, height)
        draw()
        grDevices::dev.off()
    }
}

setup_plot <- function(mar = c(4.0, 4.3, 2.5, 0.5)) {
    par(mar = mar, mgp = c(2.05, 0.6, 0), family = font_family, las = 1)
}

model_label <- function(model) {
    ifelse(model == "main_14clr", "主模型：14 个 CLR", "对照：4 个 CLR")
}

class_colour <- function(label) {
    ifelse(label == "高钾玻璃", muted, accent)
}

draw_cv_diagnostics <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    models <- c("main_14clr", "compact_4clr")
    cols <- c(muted, accent)
    pchs <- c(16, 17)

    setup_plot(c(4.1, 4.2, 2.6, 0.4))
    plot(0, 0, type = "n", xlim = c(.8, 5.2), ylim = c(.88, 1.01), xaxt = "n", bty = "n",
         xlab = "潜变量数", ylab = "按文物分组 5 折平衡准确率",
         main = "分类性能与锁定设定", cex.main = .93)
    axis(1, at = 1:5)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
    for (i in seq_along(models)) {
        d <- cv[cv$model == models[i], ]
        lines(d$ncomp, d$balanced_accuracy, type = "b", pch = pchs[i], col = cols[i], lwd = 1.8)
        d1 <- d[d$ncomp == 1, ]
        points(d1$ncomp, d1$balanced_accuracy, pch = 21, bg = "white", col = cols[i], cex = 1.65, lwd = 1.1)
    }
    abline(v = 1, lty = 2, col = green, lwd = 1.1)
    text(1.12, .892, "两模型均选 1 个潜变量", adj = c(0, 0), cex = .67, col = green)
    legend("bottomright", model_label(models), pch = pchs, lty = 1, col = cols, bty = "n", cex = .72)

    setup_plot(c(4.1, 4.2, 2.6, 0.4))
    plot(0, 0, type = "n", xlim = c(.8, 5.2), ylim = c(.165, .217), xaxt = "n", bty = "n",
         xlab = "潜变量数", ylab = "折外 RMSEP",
         main = "误差诊断（不替代锁定规则）", cex.main = .93)
    axis(1, at = 1:5)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
    for (i in seq_along(models)) {
        d <- cv[cv$model == models[i], ]
        lines(d$ncomp, d$rmsep_overall, type = "b", pch = pchs[i], col = cols[i], lwd = 1.8)
    }
    abline(v = 1, lty = 2, col = green, lwd = 1.1)
    mtext("RMSEP 仅作诊断；主模型仍沿用问题二验证最优的 1 个潜变量", side = 3,
          line = .22, cex = .60, col = ink)
    legend("topright", model_label(models), pch = pchs, lty = 1, col = cols, bty = "n", cex = .72)
}

draw_unknown_scores <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    ids <- as.character(pred$artifact_id)
    y <- rev(seq_along(ids))
    cols <- class_colour(pred$main_class_label)

    setup_plot(c(4.1, 4.7, 2.6, .35))
    plot(0, 0, type = "n", xlim = c(-.08, 1.08), ylim = c(.4, 8.6), yaxt = "n", bty = "n",
         xlab = "14-CLR PLS--DA 判别得分", ylab = "未知样品",
         main = "主模型的双类判别得分", cex.main = .91)
    axis(2, at = y, labels = ids, las = 1)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
    abline(v = .5, lty = 2, col = "#86959D")
    for (i in seq_along(ids)) {
        segments(pred$main_score_high_potassium[i], y[i], pred$main_score_lead_barium[i], y[i],
                 col = cols[i], lwd = 2.6)
        points(pred$main_score_high_potassium[i], y[i], pch = 21, bg = "white", col = muted, cex = 1.05)
        points(pred$main_score_lead_barium[i], y[i], pch = 21, bg = "white", col = accent, cex = 1.05)
    }
    text(.02, 8.38, "高钾得分", adj = c(0, .5), cex = .66, col = muted)
    text(.98, 8.38, "铅钡得分", adj = c(1, .5), cex = .66, col = accent)
    text(.50, .58, "蓝：高钾结论   棕：铅钡结论", cex = .61, col = ink)
    mtext("得分不作概率解释；线段长度为两类得分间隔", side = 3, line = .20, cex = .61, col = ink)

    setup_plot(c(4.1, 2.9, 2.6, .55))
    plot(0, 0, type = "n", xlim = c(.45, 8.55), ylim = c(.35, 3.65), xaxt = "n", yaxt = "n", bty = "n",
         xlab = "未知样品", ylab = "", main = "三种模型的结论一致性", cex.main = .91)
    axis(1, at = 1:8, labels = ids, gap.axis = 0, cex.axis = .78)
    axis(2, at = 3:1, labels = c("主模型", "4-CLR 对照", "PbO 决策树"), las = 1, cex.axis = .77)
    for (i in seq_along(ids)) {
        labels <- c(pred$main_class_label[i], pred$compact_class_label[i], pred$tree_class_label[i])
        fills <- class_colour(labels)
        for (j in seq_along(labels)) {
            rect(i - .38, 3 - j + 1 - .30, i + .38, 3 - j + 1 + .30,
                 col = fills[j], border = "white")
            text(i, 3 - j + 1, ifelse(labels[j] == "高钾玻璃", "高钾", "铅钡"), cex = .57, col = "white")
        }
    }
    text(4.5, .52, "A1--A8 均为 3/3 一致", cex = .68, col = green)
    legend("topright", c("高钾玻璃", "铅钡玻璃"), fill = c(muted, accent), border = NA, bty = "n", cex = .66)
}

draw_sensitivity <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    ids <- as.character(sens$artifact_id)
    cols <- class_colour(sens$baseline_class)

    setup_plot(c(4.1, 4.3, 2.6, .4))
    ymax <- max(sens$minimum_main_margin) * 1.20
    bp <- barplot(sens$minimum_main_margin, names.arg = ids, col = cols, border = NA, ylim = c(0, ymax),
                  ylab = "扰动后最小主模型得分间隔", xlab = "未知样品",
                  main = "局部扰动下的最小判别间隔", cex.names = .84)
    grid(nx = NA, ny = NULL, col = grid_col, lty = 3)
    text(bp, sens$minimum_main_margin, sprintf("%.3f", sens$minimum_main_margin), pos = 3, cex = .64, col = ink)
    text(bp[which.min(sens$minimum_main_margin)], sens$minimum_main_margin[which.min(sens$minimum_main_margin)],
         "A5", pos = 1, offset = 1.45, cex = .64, col = accent)
    text(4.5, .08, "蓝：高钾结论   棕：铅钡结论", cex = .61, col = ink)
    mtext("100 次 CLR 局部扰动后；A5 最接近边界，仍未翻转", side = 3, line = .20, cex = .60, col = ink)

    setup_plot(c(4.1, 3.2, 2.6, .5))
    plot(0, 0, type = "n", xlim = c(.45, 8.55), ylim = c(.35, 2.65), xaxt = "n", yaxt = "n", bty = "n",
         xlab = "未知样品", ylab = "", main = "分类稳定性", cex.main = .91)
    axis(1, at = 1:8, labels = ids, gap.axis = 0, cex.axis = .78)
    axis(2, at = 2:1, labels = c("CLR 局部扰动", "近零参数变化"), las = 1, cex.axis = .77)
    stability <- rbind(sens$clr_noise_stability, sens$delta_stability)
    for (i in seq_along(ids)) {
        for (j in 1:2) {
            fill <- grDevices::colorRampPalette(c("#EEF2F3", green))(101)[round(100 * stability[j, i]) + 1]
            rect(i - .38, 3 - j - .30, i + .38, 3 - j + .30, col = fill, border = "white")
            text(i, 3 - j, sprintf("%.0f%%", 100 * stability[j, i]), cex = .62,
                 col = ifelse(stability[j, i] >= .85, "white", ink))
        }
    }
    text(4.5, .52, "两类敏感性检验均保持 100% 的原判别结论", cex = .67, col = green)
    mtext("近零参数变化：δ=0.01%、0.02%、0.04%", side = 3, line = .20, cex = .60, col = ink)
}

with_device("problem3_cv_diagnostics", draw_cv_diagnostics, 7.25, 3.85)
with_device("problem3_unknown_scores", draw_unknown_scores, 7.25, 3.90)
with_device("problem3_sensitivity", draw_sensitivity, 7.25, 3.90)

report <- c(
    "Question 3 figure generation: PASS",
    "Inputs: stable CSV products in data/problem3/ generated by code/run_problem3.R.",
    "Outputs: three Chinese cairo vector PDFs and matching 300 dpi PNG previews.",
    "Scope: the plotting script does not read the workbook or fit/re-estimate a model.",
    "Interpretation guardrail: PLS--DA outputs are discriminant scores rather than calibrated probabilities; RMSEP is diagnostic and does not replace the locked one-component selection."
)
writeLines(report, file.path(data_dir, "problem3_figure_generation_report.txt"), useBytes = TRUE)
cat(paste(report, collapse = "\n"), "\n")
