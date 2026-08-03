#!/usr/bin/env Rscript
# Figure-only layer for Question 1.  It reads stable CSV products from
# run_problem1.R and never rereads the workbook or re-estimates a model.

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep('^--file=', args, value = TRUE)
script_path <- normalizePath(sub('^--file=', '', script_arg[1]))
root <- normalizePath(file.path(dirname(script_path), '..'))
data_dir <- file.path(root, 'data', 'problem1')
fig_dir <- file.path(root, 'figures')
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

required <- c('artifact_attributes_for_plot.csv', 'clr_baseline_centers.csv',
    'sensitivity_log_changes.csv', 'paired_leave_one_artifact_out_clr.csv',
    'paired_leave_one_artifact_out_clr_components.csv')
missing <- required[!file.exists(file.path(data_dir, required))]
if (length(missing)) stop('Missing Question 1 plot inputs: ', paste(missing, collapse = ', '))

artifact <- read.csv(file.path(data_dir, 'artifact_attributes_for_plot.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
centers <- read.csv(file.path(data_dir, 'clr_baseline_centers.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
sensitivity <- read.csv(file.path(data_dir, 'sensitivity_log_changes.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
pair_dist <- read.csv(file.path(data_dir, 'paired_leave_one_artifact_out_clr.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
pair_comp <- read.csv(file.path(data_dir, 'paired_leave_one_artifact_out_clr_components.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
stopifnot(nrow(artifact) == 58L, nrow(centers) == 28L, nrow(pair_comp) == 28L)

component_labels <- c('二氧化硅（SiO2）', '氧化钠（Na2O）', '氧化钾（K2O）', '氧化钙（CaO）',
    '氧化镁（MgO）', '氧化铝（Al2O3）', '氧化铁（Fe2O3）', '氧化铜（CuO）',
    '氧化铅（PbO）', '氧化钡（BaO）', '五氧化二磷（P2O5）', '氧化锶（SrO）',
    '氧化锡（SnO2）', '二氧化硫（SO2）')
component_label <- function(x) component_labels[match(x, paste0('component_', 1:14))]
font_family <- Sys.getenv('CUMCM_PLOT_FONT', unset = 'Microsoft YaHei')
ink <- '#25343C'; muted <- '#5F7F98'; accent <- '#C66B3D'; pale <- '#E8EEF2'; grid_col <- '#D9E0E4'; green <- '#5B8C74'

open_device <- function(path, kind, width = 7.25, height = 4.45) {
    if (kind == 'pdf') {
        grDevices::cairo_pdf(path, width = width, height = height, family = font_family, onefile = FALSE)
    } else {
        grDevices::png(path, width = round(width * 300), height = round(height * 300), res = 300, type = 'cairo')
    }
}
with_device <- function(stem, draw, width = 7.25, height = 4.45) {
    for (kind in c('pdf', 'png')) {
        open_device(file.path(fig_dir, paste0(stem, '.', kind)), kind, width, height)
        draw()
        grDevices::dev.off()
    }
}
wilson <- function(k, n, z = 1.96) {
    p <- k / n; d <- 1 + z^2 / n
    c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
        (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d)
}
rate_summary <- function(group, data = artifact) {
    split_rows <- split(data, data[[group]])
    out <- lapply(names(split_rows), function(name) {
        d <- split_rows[[name]]; ci <- wilson(sum(d$weathered == 1), nrow(d))
        data.frame(level = name, n = nrow(d), rate = mean(d$weathered == 1), low = ci[1], high = ci[2])
    })
    do.call(rbind, out)
}
draw_bar_rates <- function(tab, title) {
    tab <- tab[order(tab$rate), ]
    par(mar = c(4.0, 4.0, 2.1, .5), mgp = c(2.1, .55, 0), family = font_family, las = 1)
    bp <- barplot(100 * tab$rate, names.arg = tab$level, ylim = c(0, 100), col = muted, border = NA,
        ylab = '风化率（%）', main = title, cex.names = .82, cex.main = .92)
    arrows(bp, 100 * tab$low, bp, 100 * tab$high, angle = 90, code = 3, length = .045, lwd = 1.1, col = ink)
    text(bp, pmin(100, 100 * tab$high + 7), sprintf('%.0f%%\nn=%d', 100 * tab$rate, tab$n), cex = .68, col = ink)
    abline(h = seq(0, 100, 20), col = grid_col, lwd = .7)
    box(bty = 'l', col = '#8A969C')
}
draw_color_rates <- function(tab) {
    tab <- tab[order(tab$rate), ]; y <- seq_len(nrow(tab))
    par(mar = c(4.0, 6.0, 2.1, .5), mgp = c(2.1, .55, 0), family = font_family, las = 1)
    plot(100 * tab$rate, y, xlim = c(0, 100), ylim = c(.5, nrow(tab) + .5), yaxt = 'n', pch = 16, col = accent,
        xlab = '风化率（%）', ylab = '', main = '颜色（热卡填补主情景）', cex.main = .92, bty = 'n')
    segments(100 * tab$low, y, 100 * tab$high, y, col = ink, lwd = 1.15)
    axis(2, at = y, labels = sprintf('%s（n=%d）', tab$level, tab$n), las = 1, cex.axis = .76)
    abline(v = seq(0, 100, 20), col = grid_col, lwd = .7)
    text(pmin(97, 100 * tab$high + 5), y, sprintf('%.0f%%', 100 * tab$rate), pos = 4, cex = .67, col = ink)
}
draw_association <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 3), oma = c(0, 0, .3, 0), family = font_family)
    draw_bar_rates(rate_summary('glass_type'), '玻璃类型')
    draw_bar_rates(rate_summary('decoration'), '纹饰')
    draw_color_rates(rate_summary('color_hotdeck'))
}

draw_centers <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    for (g in c('高钾', '铅钡')) {
        d <- centers[centers$glass_type == g, ]
        d <- d[order(abs(d$log_change), decreasing = TRUE)[1:8], ]
        d <- d[order(pmax(d$unweathered_center, d$weathered_center)), ]
        y <- seq_len(nrow(d))
        par(mar = c(4.1, 7.0, 2.2, .5), mgp = c(2.1, .55, 0), las = 1)
        plot(0, 0, type = 'n', xlim = c(0, 100), ylim = c(.5, nrow(d) + .5), yaxt = 'n', xlab = '稳健中心组成（%）', ylab = '', bty = 'n', main = g, cex.main = .95)
        segments(d$unweathered_center, y, d$weathered_center, y, col = '#94A6B0', lwd = 2)
        points(d$unweathered_center, y, pch = 21, bg = 'white', col = muted, cex = 1.3, lwd = 1.2)
        points(d$weathered_center, y, pch = 21, bg = accent, col = accent, cex = 1.3)
        axis(2, at = y, labels = component_label(d$component), las = 1, cex.axis = .69)
        abline(v = seq(0, 100, 20), col = grid_col, lwd = .7)
        if (g == '高钾') legend('bottomright', c('未风化', '风化'), pch = 21, pt.bg = c('white', accent), col = c(muted, accent), bty = 'n', cex = .73)
    }
}

draw_sensitivity <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    for (g in c('高钾', '铅钡')) {
        d <- sensitivity[sensitivity$glass_type == g, ]
        base <- d[d$scenario == 'CLR baseline; delta=0.02%', c('component', 'median_log_change')]
        names(base)[2] <- 'base'
        span <- aggregate(median_log_change ~ component, d, function(x) c(min = min(x), max = max(x)))
        out <- merge(base, data.frame(component = span$component, low = span$median_log_change[, 1], high = span$median_log_change[, 2]))
        out <- out[order(out$base), ]; y <- seq_len(nrow(out))
        lim <- range(c(out$low, out$high, 0)); pad <- .08 * diff(lim); if (!is.finite(pad) || pad == 0) pad <- 1
        par(mar = c(4.1, 7.0, 2.2, .5), mgp = c(2.1, .55, 0), las = 1)
        plot(out$base, y, xlim = lim + c(-pad, pad), ylim = c(.5, nrow(out) + .5), yaxt = 'n', pch = 16, col = muted,
            xlab = 'log(风化中心 / 未风化中心)', ylab = '', bty = 'n', main = g, cex.main = .95)
        segments(out$low, y, out$high, y, col = '#9AA9B1', lwd = 2.1)
        points(out$base, y, pch = 16, col = muted, cex = .88)
        axis(2, at = y, labels = component_label(out$component), las = 1, cex.axis = .66)
        abline(v = 0, lty = 2, col = accent, lwd = 1)
        if (g == '高钾') mtext('线段：δ=0.01%、0.02%、0.04%及删除严重风化点的结果范围', side = 3, line = .05, cex = .61, col = ink)
    }
}

ternary_xy <- function(a, b, c) {
    s <- a + b + c; a <- a / s; b <- b / s; c <- c / s
    cbind(b + .5 * c, sqrt(3) / 2 * c)
}
draw_ternary_panel <- function(d, title, keys) {
    tri <- rbind(c(0, 0), c(1, 0), c(.5, sqrt(3) / 2), c(0, 0))
    par(mar = c(3.3, 2.0, 2.4, 2.0), family = font_family)
    plot(tri[, 1], tri[, 2], type = 'l', asp = 1, axes = FALSE, xlab = '', ylab = '', bty = 'n', main = title, cex.main = .9, xlim = c(-.13, 1.13), ylim = c(-.12, .98))
    for (q in seq(.2, .8, .2)) {
        segments(.5 * q, sqrt(3) / 2 * q, 1 - .5 * q, sqrt(3) / 2 * q, col = grid_col, lwd = .65)
        segments(q, 0, .5 + .5 * q, sqrt(3) / 2 * (1 - q), col = grid_col, lwd = .65)
        segments(1 - q, 0, .5 * (1 - q), sqrt(3) / 2 * (1 - q), col = grid_col, lwd = .65)
    }
    text(-.06, -.06, component_label(keys[1]), cex = .65, adj = c(0, 1))
    text(1.06, -.06, component_label(keys[2]), cex = .65, adj = c(1, 1))
    text(.5, .93, component_label(keys[3]), cex = .65)
    p0 <- ternary_xy(d$unweathered_center[match(keys, d$component)][1], d$unweathered_center[match(keys, d$component)][2], d$unweathered_center[match(keys, d$component)][3])
    p1 <- ternary_xy(d$weathered_center[match(keys, d$component)][1], d$weathered_center[match(keys, d$component)][2], d$weathered_center[match(keys, d$component)][3])
    arrows(p0[1], p0[2], p1[1], p1[2], length = .09, col = '#8798A0', lwd = 1.4)
    points(p0[1], p0[2], pch = 21, bg = 'white', col = muted, cex = 1.45, lwd = 1.2)
    points(p1[1], p1[2], pch = 21, bg = accent, col = accent, cex = 1.45)
}
draw_ternary <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    draw_ternary_panel(centers[centers$glass_type == '高钾', ], '高钾玻璃关键三成分', c('component_1', 'component_3', 'component_4'))
    draw_ternary_panel(centers[centers$glass_type == '铅钡', ], '铅钡玻璃关键三成分', c('component_1', 'component_9', 'component_11'))
    legend(.72, .93, c('未风化中心', '风化中心'), pch = 21, pt.bg = c('white', accent), col = c(muted, accent), bty = 'n', cex = .7, xpd = NA)
}

draw_pair_cases <- function() {
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), family = font_family)
    for (hold in c('49', '50')) {
        d <- pair_comp[pair_comp$artifact_id == as.integer(hold), ]
        select <- order(pmax(d$current_weathered, d$restored_leaveout, d$observed_unweathered), decreasing = TRUE)[1:8]
        d <- d[select, ]; d <- d[order(d$observed_unweathered), ]; y <- seq_len(nrow(d))
        xmax <- max(c(d$current_weathered, d$restored_leaveout, d$observed_unweathered)) * 1.16
        par(mar = c(4.1, 7.0, 2.2, .5), mgp = c(2.1, .55, 0), las = 1)
        plot(d$current_weathered, y, xlim = c(0, xmax), ylim = c(.5, nrow(d) + .5), yaxt = 'n', pch = 16, col = '#97A6AE',
            xlab = '闭合组成（%）', ylab = '', bty = 'n', main = paste0(hold, '号整件留出'), cex.main = .95)
        segments(d$current_weathered, y, d$observed_unweathered, y, col = '#CBD4D8', lwd = 2)
        points(d$current_weathered, y, pch = 16, col = '#97A6AE', cex = .82)
        points(d$restored_leaveout, y, pch = 17, col = green, cex = .95)
        points(d$observed_unweathered, y, pch = 21, bg = accent, col = accent, cex = 1.05)
        axis(2, at = y, labels = component_label(d$component), las = 1, cex.axis = .68)
        abline(v = pretty(c(0, xmax)), col = grid_col, lwd = .7)
        if (hold == '49') legend('bottomright', c('当前风化点', '留出后CLR校正', '实测未风化点'), pch = c(16, 17, 21), pt.bg = c(NA, NA, accent), col = c('#97A6AE', green, accent), bty = 'n', cex = .67)
    }
}

with_device('problem1_association_rates', draw_association, 7.4, 3.55)
with_device('problem1_centers_dumbbell', draw_centers, 7.4, 4.25)
with_device('problem1_change_sensitivity', draw_sensitivity, 7.4, 4.35)
with_device('problem1_ternary_centers', draw_ternary, 7.4, 3.8)
with_device('problem1_paired_cases', draw_pair_cases, 7.4, 4.25)

report <- c('Question 1 figure generation: PASS',
    'Inputs: data/problem1/*.csv generated by code/run_problem1.R.',
    'Outputs: five cairo vector PDFs and matching 300 dpi PNG previews.',
    'Scope: all plotted composition conclusions use the admitted CLR robust-centre baseline; no unconverged Dirichlet posterior interval is plotted.')
writeLines(report, file.path(data_dir, 'problem1_figure_generation_report.txt'), useBytes = TRUE)
cat(paste(report, collapse = '\n'), '\n')
