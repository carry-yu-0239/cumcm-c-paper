#!/usr/bin/env Rscript
# Independent verification: reads question/附件.xlsx directly with base R only.
# It intentionally does not consume MATLAB CSV snapshots as input data.

args <- commandArgs(trailingOnly = FALSE)
script.arg <- grep('^--file=', args, value = TRUE)
script.path <- normalizePath(sub('^--file=', '', script.arg[1]))
root <- normalizePath(file.path(dirname(script.path), '..'))
question <- file.path(dirname(root), 'question')
book <- list.files(question, pattern = '\\.xlsx$', full.names = TRUE)
stopifnot(length(book) == 1L)

xml_unescape <- function(x) {
    x <- gsub('&amp;', '&', x, fixed = TRUE)
    x <- gsub('&lt;', '<', x, fixed = TRUE)
    x <- gsub('&gt;', '>', x, fixed = TRUE)
    x <- gsub('&quot;', '"', x, fixed = TRUE)
    gsub('&#39;', "'", x, fixed = TRUE)
}
shared_strings <- function(dir) {
    p <- file.path(dir, 'xl', 'sharedStrings.xml')
    if (!file.exists(p)) return(character())
    x <- paste(readLines(p, warn = FALSE, encoding = 'UTF-8'), collapse = '')
    si <- regmatches(x, gregexpr('<si>.*?</si>', x, perl = TRUE))[[1]]
    vapply(si, function(a) {
        txt <- regmatches(a, gregexpr('<t[^>]*>.*?</t>', a, perl = TRUE))[[1]]
        xml_unescape(paste(gsub('<[^>]+>', '', txt, perl = TRUE), collapse = ''))
    }, character(1))
}
column_number <- function(ref) {
    letters <- gsub('[0-9]', '', ref)
    ans <- 0L
    for (ch in strsplit(letters, '', fixed = TRUE)[[1]]) ans <- ans * 26L + match(ch, LETTERS)
    ans
}
read_xlsx_sheet <- function(dir, number, strings) {
    p <- file.path(dir, 'xl', 'worksheets', paste0('sheet', number, '.xml'))
    x <- paste(readLines(p, warn = FALSE, encoding = 'UTF-8'), collapse = '')
    rows <- regmatches(x, gregexpr('<row[^>]*>.*?</row>', x, perl = TRUE))[[1]]
    out <- vector('list', length(rows))
    max_col <- 0L
    for (i in seq_along(rows)) {
        cells <- regmatches(rows[i], gregexpr('<c[^>]*(?:/>|>.*?</c>)', rows[i], perl = TRUE))[[1]]
        one <- list()
        for (cell in cells) {
            ref <- sub('.* r="([A-Z]+[0-9]+)".*', '\\1', cell, perl = TRUE)
            col <- column_number(ref); max_col <- max(max_col, col)
            kind <- if (grepl(' t="s"', cell, fixed = TRUE)) 's' else if (grepl(' t="inlineStr"', cell, fixed = TRUE)) 'inline' else 'n'
            value <- if (kind == 'inline') gsub('<[^>]+>', '', sub('.*<is>(.*?)</is>.*', '\\1', cell, perl = TRUE), perl = TRUE) else sub('.*<v>(.*?)</v>.*', '\\1', cell, perl = TRUE)
            if (!grepl('<v>|<is>', cell, perl = TRUE)) value <- NA_character_
            if (kind == 's' && !is.na(value)) value <- strings[as.integer(value) + 1L]
            one[[as.character(col)]] <- xml_unescape(value)
        }
        out[[i]] <- one
    }
    ans <- matrix(NA_character_, nrow = length(rows), ncol = max_col)
    for (i in seq_along(out)) for (j in names(out[[i]])) ans[i, as.integer(j)] <- out[[i]][[j]]
    ans
}
read_workbook <- function(path) {
    tmp <- tempfile('xlsx-'); dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(path, exdir = tmp)
    ss <- shared_strings(tmp)
    lapply(1:3, function(i) read_xlsx_sheet(tmp, i, ss))
}
closure <- function(x) 100 * x / rowSums(x)
replace_zero <- function(x, delta = 0.02) {
    out <- x
    for (i in seq_len(nrow(x))) {
        z <- x[i, ] == 0; k <- sum(z, na.rm = TRUE)
        if (k > 0) { out[i, z] <- delta; out[i, !z] <- x[i, !z] * (100 - k * delta) / 100 }
    }
    out
}
clr <- function(x) log(x / exp(rowMeans(log(x))))
diagnose <- function(z, glass_type, binary_weather) {
    groups <- paste(glass_type, binary_weather, sep = '|')
    d <- mad_t <- iqr_t <- rep(NA_real_, nrow(z)); outlier <- rep(FALSE, nrow(z))
    for (g in unique(groups)) {
        ix <- groups == g; center <- apply(z[ix, , drop = FALSE], 2, median)
        d[ix] <- sqrt(rowSums((z[ix, , drop = FALSE] - rep(center, each = sum(ix)))^2))
        med <- median(d[ix]); mad_t[ix] <- med + 3.5 * median(abs(d[ix] - med))
        qs <- quantile(d[ix], c(.25, .75), names = FALSE); iqr_t[ix] <- qs[2] + 1.5 * diff(qs)
        outlier[ix] <- d[ix] > mad_t[ix] & d[ix] > iqr_t[ix]
    }
    list(distance = d, mad = mad_t, iqr = iqr_t, outlier = outlier)
}
impute_colors <- function(aid, decoration, glass_type, color, missing, rz) {
    filled <- color; method <- donors <- rep('', length(aid)); share <- rep(NA_real_, length(aid))
    for (i in which(missing)) {
        ix <- which(!missing & glass_type == glass_type[i] & decoration == decoration[i] & complete.cases(rz))
        d <- sqrt(rowSums((rz[ix, , drop = FALSE] - rep(rz[i, ], each = length(ix)))^2)); ord <- order(d)[seq_len(min(5, length(d)))]; ix <- ix[ord]; d <- d[ord]
        w <- 1 / pmax(d, .Machine$double.eps); labels <- color[ix]; score <- tapply(w, labels, sum); winner <- names(score)[which.max(score)]; share[i] <- max(score) / sum(score)
        if (share[i] >= .5) { filled[i] <- winner; method[i] <- 'hot-deck' } else { candidates <- color[!missing & glass_type == glass_type[i]]; filled[i] <- names(sort(table(candidates), decreasing = TRUE))[1]; method[i] <- 'type-mode-fallback' }
        donors[i] <- paste(aid[ix], collapse = ',')
    }
    list(filled = filled, method = method, donors = donors, share = share)
}

sheets <- read_workbook(book)
s1 <- sheets[[1]][-1, , drop = FALSE]; s2 <- sheets[[2]][-1, , drop = FALSE]; s3 <- sheets[[3]][-1, , drop = FALSE]
stopifnot(nrow(s1) == 58L, nrow(s2) == 69L, nrow(s3) == 8L)
aid <- sprintf('%02d', as.integer(s1[, 1])); decoration <- s1[, 2]; glass_type <- s1[, 3]; color <- s1[, 4]; color[is.na(color)] <- ''
artifact_weather <- s1[, 5]; sample_name <- s2[, 1]; artifact_id <- substr(sample_name, 1, 2)
x <- apply(s2[, 2:15, drop = FALSE], 2, as.numeric); x[is.na(x)] <- 0
unknown <- apply(s3[, 3:16, drop = FALSE], 2, as.numeric); unknown[is.na(unknown)] <- 0
sums <- rowSums(x); valid <- sums >= 85 & sums <= 105
is_severe <- grepl('严重风化点', sample_name, fixed = TRUE); is_unweathered <- grepl('未风化点', sample_name, fixed = TRUE); is_ordinary <- !is_severe & !is_unweathered
stopifnot(sum(valid) == 67L, all(artifact_id[!valid] %in% c('15', '17')), sum(is_severe) == 3L, sum(is_unweathered) == 10L)
stopifnot(sum(x[valid, ] == 0) == 317L, abs(min(x[valid, ][x[valid, ] > 0]) - .04) < 1e-12, all(rowSums(unknown) >= 85 & rowSums(unknown) <= 105))

rep <- matrix(NA_real_, nrow(s1), 14)
for (i in seq_along(aid)) {
    ix <- valid & artifact_id == aid[i]
    if (any(ix)) {
        if (any(is_ordinary[ix]) && any(is_severe[ix])) rep[i, ] <- colMeans(x[ix & is_ordinary, , drop = FALSE]) / 3 + 2 * colMeans(x[ix & is_severe, , drop = FALSE]) / 3
        else if (any(is_ordinary[ix])) rep[i, ] <- colMeans(x[ix & is_ordinary, , drop = FALSE])
        else if (any(is_unweathered[ix])) rep[i, ] <- colMeans(x[ix & is_unweathered, , drop = FALSE])
        else rep[i, ] <- colMeans(x[ix, , drop = FALSE])
    }
}
missing <- color == ''; rz <- clr(replace_zero(closure(rep))); imp <- impute_colors(aid, decoration, glass_type, color, missing, rz)
expected_ids <- c('19', '40', '48', '58'); expected_colors <- c('黑', '浅蓝', '浅蓝', '浅蓝')
stopifnot(identical(unname(imp$filled[match(expected_ids, aid)]), expected_colors), imp$method[aid == '40'] == 'type-mode-fallback')

closed <- closure(x[valid, ]); z <- clr(replace_zero(closed)); vsname <- sample_name[valid]; vsid <- artifact_id[valid]
point_tag <- ifelse(grepl('严重风化点', vsname, fixed = TRUE), 'severe-weathered-point', ifelse(grepl('未风化点', vsname, fixed = TRUE), 'unweathered-point', 'ordinary-point'))
binary_weather <- ifelse(point_tag == 'unweathered-point', 'unweathered', ifelse(point_tag == 'severe-weathered-point', 'weathered', ifelse(nchar(artifact_weather[match(vsid, aid)]) == 3L, 'unweathered', 'weathered')))
diag <- diagnose(z, glass_type[match(vsid, aid)], binary_weather); stopifnot(!any(diag$outlier))
delta <- c(.01, .02, .04); sensitivity <- lapply(delta, function(d) { q <- impute_colors(aid, decoration, glass_type, color, missing, clr(replace_zero(closure(rep), d))); list(colors = q$filled[match(expected_ids, aid)], donor_changes = sum(q$donors[missing] != imp$donors[missing]), outliers = sum(diagnose(clr(replace_zero(closed, d)), glass_type[match(vsid, aid)], binary_weather)$outlier)) })
stopifnot(all(vapply(sensitivity, function(x) x$outliers, numeric(1)) == 0L))

snapshot <- file.path(root, 'data', 'verification_snapshots', 'matlab_key_results.csv')
max_diff <- NA_real_
if (file.exists(snapshot)) {
    m <- read.csv(snapshot, check.names = FALSE, fileEncoding = 'UTF-8'); mz <- as.matrix(m[, grep('^CLR_', names(m))]); storage.mode(mz) <- 'double'
    max_diff <- max(abs(z - mz)); stopifnot(max_diff <= 1e-8)
}
sens_color_text <- vapply(seq_along(delta), function(i) paste0(sprintf('%.2f%%', delta[i]), ': ', paste(sensitivity[[i]]$colors, collapse = '/')), character(1))
lines <- c('R direct-XLSX preprocessing verification: PASS', sprintf('Artifacts=%d; raw points=%d; valid=%d; unknown=%d.', nrow(s1), nrow(s2), sum(valid), nrow(s3)), sprintf('Point labels: severe=%d; unweathered=%d; ordinary=%d.', sum(is_severe), sum(is_unweathered), sum(is_ordinary)), sprintf('Invalid sums: 15=%.2f; 17=%.2f; undetected valid cells=%d.', sums[artifact_id == '15'], sums[artifact_id == '17'], sum(x[valid, ] == 0)), sprintf('Colors: %s.', paste(sprintf('%s=%s', expected_ids, imp$filled[match(expected_ids, aid)]), collapse = '; ')), sprintf('Sensitivity colors [19/40/48/58]: %s.', paste(sens_color_text, collapse = '; ')), sprintf('Sensitivity donor-set changes=%s; new robust outliers=%s.', paste(vapply(sensitivity, function(x) x$donor_changes, numeric(1)), collapse = ','), paste(vapply(sensitivity, function(x) x$outliers, numeric(1)), collapse = ',')), if (is.na(max_diff)) 'MATLAB comparison snapshot unavailable.' else sprintf('Max direct-R to MATLAB CLR difference=%.3e.', max_diff))
writeLines(lines, file.path(root, 'data', 'r_preprocess_consistency_report.txt'), useBytes = TRUE)
writeLines(lines)
