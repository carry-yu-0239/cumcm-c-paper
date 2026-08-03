#!/usr/bin/env Rscript
# Figure-only layer. It reads cleaned verification CSVs and never recomputes models.

args <- commandArgs(trailingOnly = FALSE)
script.arg <- grep('^--file=', args, value = TRUE)
script.path <- normalizePath(sub('^--file=', '', script.arg[1]))
root <- normalizePath(file.path(dirname(script.path), '..'))
snap <- file.path(root, 'data', 'verification_snapshots')
fig <- file.path(root, 'figures')
dir.create(fig, showWarnings = FALSE, recursive = TRUE)

points <- read.csv(file.path(snap, 'sheet2_raw_snapshot.csv'), check.names = FALSE, fileEncoding = 'UTF-8')
component_names <- c('二氧化硅 (SiO2)', '氧化钠 (Na2O)', '氧化钾 (K2O)', '氧化钙 (CaO)', '氧化镁 (MgO)', '氧化铝 (Al2O3)', '氧化铁 (Fe2O3)', '氧化铜 (CuO)', '氧化铅 (PbO)', '氧化钡 (BaO)', '五氧化二磷 (P2O5)', '氧化锶 (SrO)', '氧化锡 (SnO2)', '二氧化硫 (SO2)')
component_cols <- paste0('component_', seq_along(component_names))
stopifnot(nrow(points) == 69L, all(component_cols %in% names(points)))

font_family <- Sys.getenv('CUMCM_PLOT_FONT', unset = 'Microsoft YaHei')
open_device <- function(path, type) {
    if (type == 'pdf') grDevices::cairo_pdf(path, width = 7.15, height = 3.45, family = font_family, onefile = FALSE)
    else grDevices::png(path, width = 2145, height = 1035, res = 300, type = 'cairo')
}
draw_diagnostics <- function() {
    sum_order <- order(points$composition_sum)
    p <- points[sum_order, ]
    non_detect <- 100 * colSums(points[points$valid == 1, component_cols] == 0) / sum(points$valid == 1)
    bar_order <- order(non_detect, decreasing = TRUE)
    muted_blue <- '#5F7F98'; accent <- '#C66B3D'; band <- '#E8EEF2'; grid <- '#D7DDE1'
    old <- par(no.readonly = TRUE); on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), mar = c(3.6, 3.9, 1.0, 0.5), mgp = c(2.1, .55, 0), family = font_family, las = 1)
    plot(seq_len(nrow(p)), p$composition_sum, type = 'n', ylim = c(68, 108), xlab = '按成分和升序的采样点', ylab = '14种成分之和（%）', xaxt = 'n', bty = 'n')
    rect(.5, 85, nrow(p) + .5, 105, col = band, border = NA)
    abline(h = c(85, 105), col = '#8A989F', lty = 2, lwd = .8)
    valid_point <- p$valid == 1
    points(which(valid_point), p$composition_sum[valid_point], pch = 16, cex = .62, col = muted_blue)
    points(which(!valid_point), p$composition_sum[!valid_point], pch = 16, cex = .9, col = accent)
    bad <- which(!valid_point)
    text(bad, p$composition_sum[bad] + 1.5, labels = sprintf('%s：%.2f%%', p$sample_name[bad], p$composition_sum[bad]), cex = .68, col = accent, xpd = NA)
    text(nrow(p) - 2, 103.4, '有效区间 85%–105%', cex = .66, col = '#62727B')
    axis(1, at = c(1, 18, 35, 52, 69), labels = c('1', '18', '35', '52', '69'))
    grid(nx = NA, ny = NULL, col = grid, lty = 1)
    bar_col <- rep(muted_blue, length(bar_order)); bar_col[seq_len(min(3, length(bar_col)))] <- accent
    bp <- barplot(non_detect[bar_order], horiz = TRUE, names.arg = component_names[bar_order], col = bar_col, border = NA, xlim = c(0, max(non_detect) * 1.20), xlab = '未检出率（%）', cex.names = .64, las = 1, axes = FALSE)
    axis(1, at = pretty(c(0, max(non_detect))), cex.axis = .72)
    abline(v = pretty(c(0, max(non_detect))), col = grid, lwd = .7)
    text(non_detect[bar_order] + max(non_detect) * .025, bp, labels = sprintf('%.1f%%', non_detect[bar_order]), pos = 4, cex = .64, col = '#44515A')
    box(bty = 'l', col = '#89949A')
}

for (kind in c('pdf', 'png')) {
    out <- file.path(fig, paste0('data_preprocess_diagnostics.', kind))
    open_device(out, kind); draw_diagnostics(); grDevices::dev.off()
}
writeLines(c('Figure generation: PASS', 'data_preprocess_diagnostics.pdf: vector cairo PDF', 'data_preprocess_diagnostics.png: 300 dpi preview', 'Input: data/verification_snapshots/sheet2_raw_snapshot.csv'), file.path(root, 'data', 'figure_generation_report.txt'), useBytes = TRUE)
