#!/usr/bin/env Rscript
# Problem 1 authoritative calculation entry point.
# Reads the supplied XLSX directly; all outputs are summaries derived here.

set.seed(20260803)

args <- commandArgs(trailingOnly = FALSE)
script.arg <- grep('^--file=', args, value = TRUE)
script.path <- normalizePath(sub('^--file=', '', script.arg[1]))
root <- normalizePath(file.path(dirname(script.path), '..'))
question <- file.path(dirname(root), 'question')
outdir <- file.path(root, 'data', 'problem1')
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

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
    letters <- gsub('[0-9]', '', ref); ans <- 0L
    for (ch in strsplit(letters, '', fixed = TRUE)[[1]]) ans <- ans * 26L + match(ch, LETTERS)
    ans
}
read_xlsx_sheet <- function(dir, number, strings) {
    p <- file.path(dir, 'xl', 'worksheets', paste0('sheet', number, '.xml'))
    x <- paste(readLines(p, warn = FALSE, encoding = 'UTF-8'), collapse = '')
    rows <- regmatches(x, gregexpr('<row[^>]*>.*?</row>', x, perl = TRUE))[[1]]
    out <- vector('list', length(rows)); max_col <- 0L
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
    utils::unzip(path, exdir = tmp); strings <- shared_strings(tmp)
    lapply(1:2, function(i) read_xlsx_sheet(tmp, i, strings))
}
closure <- function(x) 100 * x / rowSums(x)
replace_zero <- function(x, delta) {
    out <- x
    for (i in seq_len(nrow(x))) {
        zero <- x[i, ] == 0; k <- sum(zero)
        if (k > 0) { out[i, zero] <- delta; out[i, !zero] <- x[i, !zero] * (100 - k * delta) / 100 }
    }
    out
}
clr <- function(x) log(x / exp(rowMeans(log(x))))
softmax0 <- function(eta) { q <- c(eta, -sum(eta)); e <- exp(q - max(q)); e / sum(e) }

cramers_v <- function(tab) {
    n <- sum(tab); chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
    as.numeric(sqrt(chi / (n * min(nrow(tab) - 1, ncol(tab) - 1))))
}
cramers_v_adj <- function(tab) {
    n <- sum(tab); r <- nrow(tab); k <- ncol(tab)
    chi <- as.numeric(suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic))
    phi2 <- max(0, chi / n - (k - 1) * (r - 1) / (n - 1))
    rc <- r - (r - 1)^2 / (n - 1); kc <- k - (k - 1)^2 / (n - 1)
    if (min(rc - 1, kc - 1) <= 0) return(0)
    sqrt(phi2 / min(rc - 1, kc - 1))
}
association <- function(x, weather, name, scenario) {
    tab <- table(x, weather)
    expected <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
    if (any(expected < 5)) {
        if (all(dim(tab) == c(2L, 2L))) { test <- stats::fisher.test(tab); method <- 'Fisher exact' }
        else { test <- stats::chisq.test(tab, simulate.p.value = TRUE, B = 100000); method <- 'Monte Carlo exact chi-square' }
    } else { test <- stats::chisq.test(tab, correct = FALSE); method <- 'Pearson chi-square' }
    data.frame(factor = name, scenario = scenario, n = sum(tab), method = method,
               statistic = unname(if (!is.null(test$statistic)) test$statistic else NA),
               df = unname(if (!is.null(test$parameter)) test$parameter else NA), p_value = test$p.value,
               cramers_v = cramers_v(tab), cramers_v_adj = cramers_v_adj(tab), stringsAsFactors = FALSE)
}
bootstrap_v <- function(x, weather, name, scenario, b = 1000L) {
    n <- length(weather); ans <- numeric(b)
    for (h in seq_len(b)) { ix <- sample.int(n, n, replace = TRUE); ans[h] <- cramers_v_adj(table(x[ix], weather[ix])) }
    data.frame(factor = name, scenario = scenario, draw = seq_len(b), cramers_v_adj = ans)
}
spearman_dummy <- function(x, weather, name, scenario) {
    lev <- sort(unique(x)); do.call(rbind, lapply(lev, function(a) {
        test <- suppressWarnings(stats::cor.test(as.integer(x == a), weather, method = 'spearman', exact = FALSE))
        data.frame(factor = name, scenario = scenario, category = a, rho = unname(test$estimate), p_value = test$p.value)
    }))
}

logpost <- function(par, y, w) {
    # Two state-specific 13-dimensional softmax coordinates and a shared precision.
    e0 <- par[1:13]; e1 <- par[14:26]; kappa <- par[27]
    mu0 <- softmax0(e0); mu1 <- softmax0(e1); phi <- exp(kappa)
    mu <- matrix(NA_real_, nrow = nrow(y), ncol = 14)
    mu[w == 0, ] <- matrix(mu0, nrow = sum(w == 0), ncol = 14, byrow = TRUE)
    mu[w == 1, ] <- matrix(mu1, nrow = sum(w == 1), ncol = 14, byrow = TRUE)
    ll <- sum(lgamma(phi) - rowSums(lgamma(phi * mu)) + rowSums((phi * mu - 1) * log(y / 100)))
    lp <- sum(dnorm(c(e0, e1), 0, 3, log = TRUE)) + dnorm(kappa, log(30), 2, log = TRUE)
    ll + lp
}
run_chain <- function(y, w, iter = 1700L, warmup = 700L, thin = 2L, seed) {
    set.seed(seed); init_mu <- lapply(0:1, function(s) colMeans(y[w == s, , drop = FALSE]))
    p <- c(log(init_mu[[1]][1:13] / init_mu[[1]][14]), log(init_mu[[2]][1:13] / init_mu[[2]][14]), log(30))
    p <- p + rnorm(27, 0, .08); current <- logpost(p, y, w); scale <- c(rep(.09, 26), .07)
    keep <- floor((iter - warmup) / thin); draws <- matrix(NA_real_, keep, 27); accept <- numeric(27); at <- 0L
    for (it in seq_len(iter)) {
        for (j in seq_along(p)) {
            proposal <- p; proposal[j] <- proposal[j] + rnorm(1, 0, scale[j]); val <- logpost(proposal, y, w)
            if (is.finite(val) && log(runif(1)) < val - current) { p <- proposal; current <- val; accept[j] <- accept[j] + 1 }
        }
        if (it == warmup) { rate <- accept / warmup; scale <- scale * ifelse(rate < .18, .65, ifelse(rate > .42, 1.35, 1)); accept[] <- 0 }
        if (it > warmup && (it - warmup) %% thin == 0) { at <- at + 1L; draws[at, ] <- p }
    }
    list(draws = draws, accept = accept / (iter - warmup))
}
rhat <- function(chains) {
    values <- lapply(chains, as.numeric)
    if (length(values) < 2L || any(lengths(values) < 2L)) return(NA_real_)
    n <- length(values[[1]]); means <- vapply(values, mean, numeric(1)); vars <- vapply(values, var, numeric(1))
    out <- sqrt((((n - 1) / n) * mean(vars) + n * var(means)) / mean(vars))
    as.numeric(out)
}
ess_ar1 <- function(x) { rho <- cor(x[-length(x)], x[-1]); max(1, length(x) * (1 - rho) / (1 + rho)) }
fit_dirichlet <- function(y, w, seed) {
    chains <- lapply(1:3, function(i) run_chain(y, w, seed = seed + i))
    draws <- do.call(rbind, lapply(chains, `[[`, 'draws'))
    mu0 <- t(apply(draws[, 1:13, drop = FALSE], 1, softmax0)); mu1 <- t(apply(draws[, 14:26, drop = FALSE], 1, softmax0)); phi <- exp(draws[, 27])
    monitor <- cbind(log_posterior = apply(draws, 1, function(z) logpost(z, y, w)), phi = phi, mu0_c1 = mu0[, 1], mu0_c3 = mu0[, 3], mu0_c9 = mu0[, 9], mu1_c1 = mu1[, 1], mu1_c3 = mu1[, 3], mu1_c9 = mu1[, 9])
    chain_monitor <- lapply(chains, function(one) cbind(log_posterior = apply(one$draws, 1, function(z) logpost(z, y, w)), phi = exp(one$draws[, 27]),
        t(apply(one$draws[, 1:13, drop = FALSE], 1, softmax0))[, c(1, 3, 9)], t(apply(one$draws[, 14:26, drop = FALSE], 1, softmax0))[, c(1, 3, 9)]))
    list(mu0 = mu0, mu1 = mu1, phi = phi, draws = draws,
         diagnostic = data.frame(metric = colnames(monitor), rhat = vapply(seq_len(ncol(monitor)), function(j) rhat(lapply(chain_monitor, function(a) a[, j])), numeric(1)),
            ess_ar1 = vapply(seq_len(ncol(monitor)), function(j) ess_ar1(monitor[, j]), numeric(1))),
         acceptance = colMeans(do.call(rbind, lapply(chains, `[[`, 'accept'))))
}
summarise_matrix <- function(m, group, component_names, field) {
    do.call(rbind, lapply(seq_along(component_names), function(j) data.frame(glass_type = group, component = component_names[j], measure = field,
        median = median(m[, j]), lower95 = quantile(m[, j], .025), upper95 = quantile(m[, j], .975))))
}
restore_draws <- function(y, fit) {
    nd <- nrow(fit$mu0); out <- array(NA_real_, c(nd, nrow(y), 14))
    for (d in seq_len(nd)) { q <- sweep(y, 2, fit$mu0[d, ] / fit$mu1[d, ], '*'); out[d, , ] <- 100 * q / rowSums(q) }
    out
}
robust_center <- function(y) { exp(apply(clr(y), 2, median)) / sum(exp(apply(clr(y), 2, median))) * 100 }
aitchison <- function(x, y) sqrt(sum((clr(matrix(x, nrow = 1)) - clr(matrix(y, nrow = 1)))^2))

book <- list.files(question, pattern = '\\.xlsx$', full.names = TRUE); stopifnot(length(book) == 1L)
sheets <- read_workbook(book); s1 <- sheets[[1]][-1, , drop = FALSE]; s2 <- sheets[[2]][-1, , drop = FALSE]
aid <- sprintf('%02d', as.integer(s1[, 1])); decoration <- s1[, 2]; glass <- s1[, 3]; color <- s1[, 4]; weather_art <- as.integer(s1[, 5] == '风化')
sample_name <- s2[, 1]; sid <- substr(sample_name, 1, 2); x <- apply(s2[, 2:15, drop = FALSE], 2, as.numeric); x[is.na(x)] <- 0
valid <- rowSums(x) >= 85 & rowSums(x) <= 105; stopifnot(sum(valid) == 67L)
sample_name <- sample_name[valid]; sid <- sid[valid]; x <- x[valid, , drop = FALSE]
is_unweathered <- grepl('未风化点', sample_name, fixed = TRUE); is_severe <- grepl('严重风化点', sample_name, fixed = TRUE)
w <- ifelse(is_unweathered, 0L, ifelse(is_severe, 1L, weather_art[match(sid, aid)])); point_level <- ifelse(is_severe, 2L, w)
glass_point <- glass[match(sid, aid)]
state_counts <- table(glass_point, point_level)
stopifnot(state_counts['高钾', '0'] == 12L, state_counts['高钾', '1'] == 6L,
          state_counts['铅钡', '0'] == 23L, state_counts['铅钡', '1'] == 23L, state_counts['铅钡', '2'] == 3L)
component <- paste0('component_', 1:14)

# Stable plot inputs are written here so that figure scripts never need to reread
# the workbook or recompute a model.  The hot-deck scenario is the agreed main
# colour scenario; the source/method flags keep imputed values distinguishable.
color_hotdeck <- color; color_hotdeck[aid %in% c('19', '40', '48', '58')] <- c('黑', '浅蓝', '浅蓝', '浅蓝')
color_mode <- color; color_mode[aid %in% c('19', '40', '48', '58')] <- '浅蓝'
artifact_plot <- data.frame(
    artifact_id = aid,
    glass_type = glass,
    decoration = decoration,
    color_raw = color,
    color_hotdeck = color_hotdeck,
    color_mode = color_mode,
    color_source = ifelse(is.na(color), '热卡填补', '实测'),
    weathered = weather_art,
    stringsAsFactors = FALSE
)
point_plot <- data.frame(
    sample_name = sample_name,
    artifact_id = sid,
    glass_type = glass_point,
    point_weather_level = point_level,
    point_tag = ifelse(point_level == 0, '未风化', ifelse(point_level == 2, '严重风化', '一般风化')),
    replace_zero(closure(x), .02),
    check.names = FALSE
)
names(point_plot)[6:19] <- component
write.csv(artifact_plot, file.path(outdir, 'artifact_attributes_for_plot.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(point_plot, file.path(outdir, 'point_compositions_for_plot.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

if ('--baseline-only' %in% commandArgs(trailingOnly = TRUE)) {
    # This fallback is the reported restoration method whenever the MCMC chains do not mix.
    baseline_centres <- list(); baseline_restore <- list(); b <- 1L; r <- 1L
    for (g in c('高钾', '铅钡')) {
        ix <- glass_point == g; y <- replace_zero(closure(x[ix, , drop = FALSE]), .02)
        m0 <- robust_center(y[w[ix] == 0, , drop = FALSE]); m1 <- robust_center(y[w[ix] == 1, , drop = FALSE])
        baseline_centres[[b]] <- data.frame(glass_type = g, component = component, unweathered_center = m0, weathered_center = m1, log_change = log(m1 / m0)); b <- b + 1L
        target <- which(ix & w == 1); q <- replace_zero(closure(x[target, , drop = FALSE]), .02)
        pred <- sweep(q, 2, m0 / m1, '*'); pred <- 100 * pred / rowSums(pred)
        baseline_restore[[r]] <- do.call(rbind, lapply(seq_along(target), function(a) data.frame(sample_name = sample_name[target[a]], artifact_id = sid[target[a]], glass_type = g, component = component, point_level = point_level[target[a]], is_extrapolation = point_level[target[a]] == 2, predicted_unweathered = pred[a, ]))); r <- r + 1L
    }
    baseline_pairs <- list(); baseline_pair_components <- list(); z <- 1L; pc <- 1L
    for (hold in c('49', '50')) {
        g <- glass_point[match(hold, sid)]; train <- sid != hold & glass_point == g; yt <- replace_zero(closure(x[train, , drop = FALSE]), .02)
        m0 <- robust_center(yt[w[train] == 0, , drop = FALSE]); m1 <- robust_center(yt[w[train] == 1, , drop = FALSE])
        iw <- which(sid == hold & w == 1); iu <- which(sid == hold & w == 0); q <- replace_zero(closure(x[iw, , drop = FALSE]), .02)
        pred <- q[1, ] * m0 / m1; pred <- 100 * pred / sum(pred); actual <- replace_zero(closure(x[iu[1], , drop = FALSE]), .02)[1, ]
        baseline_pairs[[z]] <- data.frame(artifact_id = hold, method = 'CLR robust inverse perturbation', aitchison_distance = aitchison(pred, actual)); z <- z + 1L
        baseline_pairs[[z]] <- data.frame(artifact_id = hold, method = 'Unweathered robust center', aitchison_distance = aitchison(m0, actual)); z <- z + 1L
        baseline_pair_components[[pc]] <- data.frame(artifact_id = hold, component = component,
            current_weathered = q[1, ], restored_leaveout = pred, observed_unweathered = actual); pc <- pc + 1L
    }
    write.csv(do.call(rbind, baseline_centres), file.path(outdir, 'clr_baseline_centers.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
    write.csv(do.call(rbind, baseline_restore), file.path(outdir, 'clr_baseline_restored_points.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
    write.csv(do.call(rbind, baseline_pairs), file.path(outdir, 'paired_leave_one_artifact_out_clr.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
    write.csv(do.call(rbind, baseline_pair_components), file.path(outdir, 'paired_leave_one_artifact_out_clr_components.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
    writeLines('CLR baseline outputs: PASS', file.path(outdir, 'clr_baseline_summary.txt'), useBytes = TRUE)
    quit(save = 'no', status = 0L)
}

# Association analysis: Q3 latest decision makes hot-deck completed data the primary colour scenario.
scenarios <- list(hotdeck58 = color_hotdeck, complete54 = color[!is.na(color)], mode58 = color_mode)
assoc <- rbind(association(glass, weather_art, '玻璃类型', 'all58'), association(decoration, weather_art, '纹饰', 'all58'),
    association(scenarios$hotdeck58, weather_art, '颜色', 'hotdeck58'), association(scenarios$complete54, weather_art[!is.na(color)], '颜色', 'complete54'), association(scenarios$mode58, weather_art, '颜色', 'mode58'))
boot <- rbind(bootstrap_v(glass, weather_art, '玻璃类型', 'all58'), bootstrap_v(decoration, weather_art, '纹饰', 'all58'),
    bootstrap_v(scenarios$hotdeck58, weather_art, '颜色', 'hotdeck58'), bootstrap_v(scenarios$complete54, weather_art[!is.na(color)], '颜色', 'complete54'), bootstrap_v(scenarios$mode58, weather_art, '颜色', 'mode58'))
intervals <- aggregate(cramers_v_adj ~ factor + scenario, boot, function(a) c(lower95 = quantile(a, .025), upper95 = quantile(a, .975)))
intervals <- data.frame(factor = intervals$factor, scenario = intervals$scenario,
    lower95 = intervals$cramers_v_adj[, 1], upper95 = intervals$cramers_v_adj[, 2])
dummy <- rbind(spearman_dummy(glass, weather_art, '玻璃类型', 'all58'), spearman_dummy(decoration, weather_art, '纹饰', 'all58'), spearman_dummy(scenarios$hotdeck58, weather_art, '颜色', 'hotdeck58'))
write.csv(assoc, file.path(outdir, 'association_tests.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(intervals, file.path(outdir, 'cramers_v_intervals.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(dummy, file.path(outdir, 'spearman_dummy_results.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

# Bayesian shared-precision Dirichlet model. Priors are weakly informative and recorded here:
# eta_tw,j ~ N(0, 3^2), kappa_t=log(phi_t) ~ N(log(30), 2^2).
fits <- list(); centers <- list(); changes <- list(); diagnostics <- list(); restored <- list()
for (g in c('高钾', '铅钡')) {
    ix <- glass_point == g; y <- replace_zero(closure(x[ix, , drop = FALSE]), .02); fg <- fit_dirichlet(y, w[ix], if (g == '高钾') 8301 else 8302); fits[[g]] <- fg
    centers[[g]] <- rbind(summarise_matrix(fg$mu0 * 100, g, component, 'unweathered_center'), summarise_matrix(fg$mu1 * 100, g, component, 'weathered_center'))
    lc <- log(fg$mu1 / fg$mu0); changes[[g]] <- data.frame(glass_type = g, component = component, median_log_change = apply(lc, 2, median), lower95 = apply(lc, 2, quantile, .025), upper95 = apply(lc, 2, quantile, .975), posterior_increase = colMeans(lc > 0))
    diagnostics[[g]] <- cbind(glass_type = g, fg$diagnostic)
    wi <- which(ix & w == 1); rd <- restore_draws(replace_zero(closure(x[wi, , drop = FALSE]), .02), fg)
    restored[[g]] <- do.call(rbind, lapply(seq_along(wi), function(a) do.call(rbind, lapply(1:14, function(j) data.frame(sample_name = sample_name[wi[a]], artifact_id = sid[wi[a]], glass_type = g, component = component[j], point_level = point_level[wi[a]], is_extrapolation = point_level[wi[a]] == 2,
        median = median(rd[, a, j]), lower95 = quantile(rd[, a, j], .025), upper95 = quantile(rd[, a, j], .975))))))
}
centers <- do.call(rbind, centers); changes <- do.call(rbind, changes); diagnostics <- do.call(rbind, diagnostics); restored <- do.call(rbind, restored)
write.csv(centers, file.path(outdir, 'dirichlet_centers.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(changes, file.path(outdir, 'dirichlet_log_changes.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(diagnostics, file.path(outdir, 'mcmc_diagnostics.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(restored, file.path(outdir, 'restored_points_long.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

# Two paired, leave-one-artifact-out checks are intentionally case studies, never a population accuracy estimate.
pair_rows <- list(); p <- 1L
for (hold in c('49', '50')) {
    hx <- sid != hold; for (g in unique(glass_point[sid == hold])) {
        train <- hx & glass_point == g; testw <- which(sid == hold & w == 1); testu <- which(sid == hold & w == 0)
        f <- fit_dirichlet(replace_zero(closure(x[train, , drop = FALSE]), .02), w[train], as.integer(9000 + as.integer(hold)))
        pred <- apply(restore_draws(replace_zero(closure(x[testw, , drop = FALSE]), .02), f)[, 1, ], 2, median)
        baseline <- robust_center(replace_zero(closure(x[train & w == 0, , drop = FALSE]), .02))
        actual <- replace_zero(closure(x[testu[1], , drop = FALSE]), .02)[1, ]
        pair_rows[[p]] <- data.frame(artifact_id = hold, method = 'Dirichlet inverse perturbation', aitchison_distance = aitchison(pred, actual)); p <- p + 1L
        pair_rows[[p]] <- data.frame(artifact_id = hold, method = 'Unweathered robust center', aitchison_distance = aitchison(baseline, actual)); p <- p + 1L
    }
}
pairs <- do.call(rbind, pair_rows); write.csv(pairs, file.path(outdir, 'paired_leave_one_artifact_out.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

# Fast deterministic CLR-centre sensitivity: full posterior refits are deliberately not
# represented as completed for these auxiliary scenarios.
sensitivity <- list(); s <- 1L
for (delta in c(.01, .02, .04)) for (g in c('高钾', '铅钡')) {
    ix <- glass_point == g; y <- replace_zero(closure(x[ix, , drop = FALSE]), delta)
    lc <- log(robust_center(y[w[ix] == 1, , drop = FALSE]) / robust_center(y[w[ix] == 0, , drop = FALSE]))
    sensitivity[[s]] <- data.frame(scenario = sprintf('CLR baseline; delta=%.2f%%', delta), glass_type = g, component = component, median_log_change = lc); s <- s + 1L
}
for (g in c('高钾', '铅钡')) { ix <- glass_point == g & point_level < 2; y <- replace_zero(closure(x[ix, , drop = FALSE]), .02); lc <- log(robust_center(y[w[ix] == 1, , drop = FALSE]) / robust_center(y[w[ix] == 0, , drop = FALSE])); sensitivity[[s]] <- data.frame(scenario = 'CLR baseline; remove_severe', glass_type = g, component = component, median_log_change = lc); s <- s + 1L }
sensitivity <- do.call(rbind, sensitivity); write.csv(sensitivity, file.path(outdir, 'sensitivity_log_changes.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

admission <- data.frame(rule = c('MCMC parameter estimation', 'Direction sensitivity', 'Paired case checks', 'Covariance interpretation'), status = c(
    if (all(diagnostics$rhat < 1.05)) 'pass' else sprintf('failed: maximum monitored Rhat=%.3f; do not use Dirichlet posterior for final recovery', max(diagnostics$rhat)),
    'partial: CLR baseline direction comparison only; no posterior sensitivity claim',
    'not used for Dirichlet admission after failed chain mixing; CLR leave-one-artifact-out results are reported separately',
    'not claimed: Dirichlet covariance restriction was not used for interpretation'), stringsAsFactors = FALSE)
write.csv(admission, file.path(outdir, 'model_admission.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

summary_lines <- c(
    'Problem 1 calculation: PASS',
    'Input: supplied question/附件.xlsx; valid classified chemistry points=67; zero replacement delta=0.02%.',
    'Association primary colour scenario: hot-deck completed 58 artifacts; complete-case and type-mode analyses are sensitivity scenarios.',
    'Bayesian model: two glass types fitted separately; eta~N(0,3^2), log(phi)~N(log(30),2^2); 3 chains, 1,700 iterations, 700 warm-up, thin=2.',
    sprintf('Rhat range for monitored quantities: %.3f--%.3f.', min(diagnostics$rhat), max(diagnostics$rhat)),
    'This calculation script writes stable CSV inputs for the separate figure-only script code/make_problem1_figures.R; it does not draw figures itself.',
    'Important boundary: the 49/50 leave-one-artifact-out results are two case checks, not a generalization-error estimate.'
)
writeLines(summary_lines, file.path(outdir, 'run_summary.txt'), useBytes = TRUE)
writeLines(summary_lines)
