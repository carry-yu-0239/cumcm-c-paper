#!/usr/bin/env Rscript
# Problem 2 authoritative calculation entry point.
# It consumes the immutable preprocessing snapshots produced by preprocess.m.
# No figures are created here; later figure code must consume data/problem2/*.csv.

options(stringsAsFactors = FALSE)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
if (is.na(script_file) || !file.exists(script_file)) script_file <- "cumcm_c_skeleton/code/run_problem2.R"
root_dir <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
data_dir <- file.path(root_dir, "data")
snapshot_dir <- file.path(data_dir, "verification_snapshots")
out_dir <- file.path(data_dir, "problem2")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

set.seed(20260804)
component_id <- paste0("component_", seq_len(14))
chemical_name <- c("SiO2", "Na2O", "K2O", "CaO", "MgO", "Al2O3", "Fe2O3",
                   "CuO", "PbO", "BaO", "P2O5", "SrO", "SnO2", "SO2")
clr_name <- paste0("CLR_", seq_len(14))
stopifnot(length(component_id) == 14L, length(chemical_name) == 14L)

assert_that <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
}

write_csv <- function(x, file) {
    write.csv(x, file.path(out_dir, file), row.names = FALSE, fileEncoding = "UTF-8")
}

inverse_clr <- function(z) {
    z <- as.matrix(z)
    e <- exp(z - apply(z, 1, max))
    100 * e / rowSums(e)
}

standardize_fit <- function(x) {
    center <- colMeans(x)
    scale_value <- apply(x, 2, sd)
    scale_value[!is.finite(scale_value) | scale_value < 1e-12] <- 1
    list(center = center, scale = scale_value)
}

standardize_apply <- function(x, fit) {
    sweep(sweep(as.matrix(x), 2, fit$center, "-"), 2, fit$scale, "/")
}

make_group_folds <- function(groups, y, k = 5L, seed = 20260804L) {
    group_table <- unique(data.frame(group = as.character(groups), class = as.integer(y)))
    group_table <- group_table[order(group_table$class, group_table$group), ]
    set.seed(seed)
    group_table$fold <- NA_integer_
    for (label in sort(unique(group_table$class))) {
        at <- which(group_table$class == label)
        at <- sample(at, length(at))
        group_table$fold[at] <- rep(seq_len(k), length.out = length(at))
    }
    group_table$fold[match(as.character(groups), group_table$group)]
}

classification_metrics <- function(actual, predicted) {
    actual <- as.integer(actual)
    predicted <- as.integer(predicted)
    labels <- sort(unique(c(actual, predicted)))
    recall <- vapply(labels, function(label) {
        denominator <- sum(actual == label)
        if (denominator == 0L) NA_real_ else sum(actual == label & predicted == label) / denominator
    }, numeric(1))
    precision <- vapply(labels, function(label) {
        denominator <- sum(predicted == label)
        if (denominator == 0L) 0 else sum(actual == label & predicted == label) / denominator
    }, numeric(1))
    f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    data.frame(
        accuracy = mean(actual == predicted),
        balanced_accuracy = mean(recall, na.rm = TRUE),
        macro_f1 = mean(f1, na.rm = TRUE),
        stringsAsFactors = FALSE
    )
}

adjusted_rand_index <- function(a, b) {
    tab <- table(a, b)
    n <- sum(tab)
    choose2 <- function(x) x * (x - 1) / 2
    index <- sum(choose2(tab))
    row_index <- sum(choose2(rowSums(tab)))
    col_index <- sum(choose2(colSums(tab)))
    expected <- row_index * col_index / choose2(n)
    denominator <- (row_index + col_index) / 2 - expected
    if (abs(denominator) < 1e-12) return(1)
    (index - expected) / denominator
}

fit_pls2 <- function(x, y, ncomp) {
    x <- as.matrix(x)
    y <- as.matrix(y)
    xfit <- standardize_fit(x)
    xh <- standardize_apply(x, xfit)
    ymean <- colMeans(y)
    yh <- sweep(y, 2, ymean, "-")
    p <- ncol(xh)
    q <- ncol(yh)
    w <- matrix(0, p, ncomp)
    p_load <- matrix(0, p, ncomp)
    q_load <- matrix(0, q, ncomp)
    t_score <- matrix(0, nrow(xh), ncomp)
    ss_y <- numeric(ncomp)
    for (a in seq_len(ncomp)) {
        cross_xy <- crossprod(xh, yh)
        sv <- svd(cross_xy, nu = 1, nv = 0)
        wa <- sv$u[, 1]
        ta <- drop(xh %*% wa)
        denom <- sum(ta^2)
        assert_that(denom > 1e-12, "PLS component has zero score variance.")
        pa <- drop(crossprod(xh, ta) / denom)
        qa <- drop(crossprod(yh, ta) / denom)
        w[, a] <- wa
        p_load[, a] <- pa
        q_load[, a] <- qa
        t_score[, a] <- ta
        ss_y[a] <- denom * sum(qa^2)
        xh <- xh - tcrossprod(ta, pa)
        yh <- yh - tcrossprod(ta, qa)
    }
    b <- w %*% solve(crossprod(p_load, w), t(q_load))
    list(
        xfit = xfit, ymean = ymean, coefficients = b, weights = w,
        loadings_x = p_load, loadings_y = q_load, scores = t_score,
        ss_y = ss_y
    )
}

predict_pls2 <- function(fit, new_x) {
    xh <- standardize_apply(new_x, fit$xfit)
    sweep(xh %*% fit$coefficients, 2, fit$ymean, "+")
}

pls_vip <- function(fit) {
    denominator <- sum(fit$ss_y)
    assert_that(denominator > 1e-12, "PLS response sum of squares is zero.")
    sqrt(nrow(fit$weights) * drop((fit$weights^2) %*% fit$ss_y) / denominator)
}

one_hot <- function(y) {
    labels <- sort(unique(as.integer(y)))
    out <- sapply(labels, function(label) as.numeric(y == label))
    if (is.null(dim(out))) out <- matrix(out, ncol = 1)
    colnames(out) <- paste0("class_", labels)
    out
}

read_snapshot_inputs <- function() {
    required <- c("sheet1_raw_snapshot.csv", "sheet2_raw_snapshot.csv", "matlab_key_results.csv")
    paths <- file.path(snapshot_dir, required)
    assert_that(all(file.exists(paths)), "Required preprocessing snapshots are missing.")
    artifact <- read.csv(paths[1], fileEncoding = "UTF-8", check.names = FALSE)
    sample_clean <- read.csv(paths[2], fileEncoding = "UTF-8", check.names = FALSE)
    clr <- read.csv(paths[3], fileEncoding = "UTF-8", check.names = FALSE)
    assert_that(nrow(artifact) == 58L, "Expected 58 artifact records.")
    assert_that(nrow(sample_clean) == 69L, "Expected 69 raw sampling-point records.")
    assert_that(nrow(clr) == 67L, "Expected 67 valid CLR records.")
    assert_that(all(clr_name %in% names(clr)), "CLR snapshot has an unexpected component schema.")
    assert_that(!any(clr$robust_outlier), "Problem 2 must not contain retained robust outliers.")
    idx <- match(clr$artifact_id, artifact$artifact_id)
    assert_that(!anyNA(idx), "Every valid sampling point must match an artifact record.")
    model <- clr
    model$glass_type <- artifact$glass_type[idx]
    model$decoration <- artifact$decoration[idx]
    model$final_color <- artifact$raw_color[idx]
    model$artifact_weather <- artifact$artifact_weather[idx]
    assert_that(!anyNA(model$glass_type), "Glass type is missing after artifact linkage.")
    counts <- table(model$glass_type)
    assert_that(length(counts) == 2L && sort(as.integer(counts))[1] == 18L && sort(as.integer(counts))[2] == 49L,
                "Expected 18 high-potassium and 49 lead-barium sampling points.")
    assert_that(!any(model$artifact_id %in% c(15, 17)), "Invalid artifacts 15 and 17 entered the model.")
    model
}

run_tree_cv <- function(model, x, y, folds, cp_grid) {
    rows <- list()
    nrow_out <- 0L
    for (cp in cp_grid) {
        predictions <- rep(NA_integer_, nrow(x))
        for (fold in sort(unique(folds))) {
            train <- folds != fold
            fit <- rpart::rpart(
                factor(y[train], levels = c(1, 2)) ~ ., data = data.frame(y = factor(y[train], levels = c(1, 2)), x[train, , drop = FALSE]),
                method = "class", parms = list(split = "information"),
                control = rpart::rpart.control(cp = cp, minbucket = 2, maxdepth = 5, xval = 0)
            )
            predicted <- predict(fit, newdata = data.frame(x[!train, , drop = FALSE]), type = "class")
            predictions[!train] <- as.integer(as.character(predicted))
        }
        metric <- classification_metrics(y, predictions)
        nrow_out <- nrow_out + 1L
        rows[[nrow_out]] <- cbind(cp = cp, metric)
    }
    do.call(rbind, rows)
}

choose_cp <- function(scores) {
    best <- max(scores$balanced_accuracy)
    max(scores$cp[scores$balanced_accuracy >= best - 1e-12])
}

fit_ordered_kmeans <- function(x, centers, seed = 20260804L, nstart = 100L) {
    set.seed(seed)
    fit <- kmeans(x, centers = centers, nstart = nstart, iter.max = 100)
    # Cluster identifiers have no intrinsic meaning.  Ordering by the first selected
    # component yields reproducible labels without using an archaeological interpretation.
    ordering <- order(fit$centers[, 1], fit$centers[, 2], na.last = TRUE)
    remap <- integer(centers)
    remap[ordering] <- seq_len(centers)
    fit$cluster <- remap[fit$cluster]
    fit$centers <- fit$centers[ordering, , drop = FALSE]
    fit$size <- tabulate(fit$cluster, nbins = centers)
    fit
}

cluster_grid <- function(z, k_values, seed) {
    rows <- list()
    for (k in k_values) {
        fit <- fit_ordered_kmeans(z, k, seed)
        sil <- mean(cluster::silhouette(fit$cluster, dist(z))[, 3])
        rows[[length(rows) + 1L]] <- data.frame(
            k = k, withinss = fit$tot.withinss, between_to_total = fit$betweenss / fit$totss,
            average_silhouette = sil, min_cluster_size = min(fit$size),
            cluster_sizes = paste(fit$size, collapse = ";"), stringsAsFactors = FALSE
        )
    }
    do.call(rbind, rows)
}

cluster_sensitivity <- function(z, baseline, feature_sets, k, seed) {
    rows <- list()
    # Initialization sensitivity: nstart=1 reveals local-minimum dependence.
    for (replicate_id in seq_len(100L)) {
        fit <- fit_ordered_kmeans(z, k, seed + replicate_id, nstart = 1L)
        rows[[length(rows) + 1L]] <- data.frame(
            scenario = "initialization", replicate = replicate_id,
            adjusted_rand_index = adjusted_rand_index(baseline$cluster, fit$cluster),
            withinss = fit$tot.withinss, stringsAsFactors = FALSE
        )
    }
    # Small standardized perturbations quantify assignment stability without changing the
    # established CLR preprocessing or reclassifying preprocessing outliers.
    for (replicate_id in seq_len(100L)) {
        set.seed(seed + 1000L + replicate_id)
        perturbed <- z + matrix(rnorm(length(z), sd = 0.03), nrow(z), ncol(z))
        fit <- fit_ordered_kmeans(perturbed, k, seed + 2000L + replicate_id, nstart = 30L)
        rows[[length(rows) + 1L]] <- data.frame(
            scenario = "zclr_noise_sd_0.03", replicate = replicate_id,
            adjusted_rand_index = adjusted_rand_index(baseline$cluster, fit$cluster),
            withinss = fit$tot.withinss, stringsAsFactors = FALSE
        )
    }
    for (name in names(feature_sets)) {
        feature_z <- feature_sets[[name]]
        fit <- fit_ordered_kmeans(feature_z, k, seed, nstart = 100L)
        rows[[length(rows) + 1L]] <- data.frame(
            scenario = name, replicate = 1L,
            adjusted_rand_index = adjusted_rand_index(baseline$cluster, fit$cluster),
            withinss = fit$tot.withinss, stringsAsFactors = FALSE
        )
    }
    do.call(rbind, rows)
}

model <- read_snapshot_inputs()
x_clr <- as.matrix(model[, clr_name])
storage.mode(x_clr) <- "double"
assert_that(max(abs(rowSums(x_clr))) < 1e-10, "CLR rows do not sum to zero.")
lead_label <- names(which.max(table(model$glass_type)))
y <- ifelse(model$glass_type == lead_label, 1L, 2L)
folds <- make_group_folds(model$artifact_id, y)
assert_that(length(unique(model$artifact_id)) == 56L, "Expected 56 artifacts with valid chemistry.")

input_audit <- data.frame(
    item = c("artifact_records", "raw_sampling_points", "valid_sampling_points", "valid_artifacts",
             "lead_barium_points", "high_potassium_points", "retained_robust_outliers", "max_abs_clr_row_sum"),
    value = c(58, 69, nrow(model), length(unique(model$artifact_id)), sum(y == 1L), sum(y == 2L),
              sum(model$robust_outlier), max(abs(rowSums(x_clr)))),
    stringsAsFactors = FALSE
)
write_csv(input_audit, "input_audit.csv")
write_csv(data.frame(sample_name = model$sample_name, artifact_id = model$artifact_id, fold = folds,
                     class_code = y, glass_type = model$glass_type, stringsAsFactors = FALSE),
          "grouped_cv_folds.csv")
plot_matrix <- data.frame(
    sample_name = model$sample_name, artifact_id = model$artifact_id,
    glass_type = model$glass_type, class_code = y, x_clr,
    check.names = FALSE, stringsAsFactors = FALSE
)
names(plot_matrix)[5:ncol(plot_matrix)] <- clr_name
write_csv(plot_matrix, "plot_model_matrix.csv")

# Binary decision tree: information gain, grouping-aware tuning, then refit on all known points.
cp_grid <- c(0, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1)
tree_cv <- run_tree_cv(model, x_clr, y, folds, cp_grid)
tree_cp <- choose_cp(tree_cv)
tree_fit <- rpart::rpart(
    factor(y, levels = c(1, 2)) ~ ., data = data.frame(y = factor(y, levels = c(1, 2)), x_clr),
    method = "class", parms = list(split = "information"),
    control = rpart::rpart.control(cp = tree_cp, minbucket = 2, maxdepth = 5, xval = 0)
)
tree_prob <- predict(tree_fit, newdata = data.frame(x_clr), type = "prob")
tree_class <- as.integer(as.character(predict(tree_fit, newdata = data.frame(x_clr), type = "class")))
assert_that(max(abs(rowSums(tree_prob) - 1)) < 1e-12, "Tree class probabilities do not sum to one.")
root_split <- tree_fit$splits[1, , drop = FALSE]
tree_rule <- data.frame(
    criterion = "information_gain", selected_cp = tree_cp, root_component = rownames(root_split),
    root_threshold = root_split[, "index"], root_improvement = root_split[, "improve"],
    node_count = nrow(tree_fit$frame), stringsAsFactors = FALSE
)
write_csv(tree_cv, "tree_grouped_cv.csv")
write_csv(tree_rule, "tree_rule.csv")

# PLS-DA uses dummy-coded labels.  It is evaluated in exactly the same grouped folds;
# standardization is fitted inside every training fold.
pls_rows <- list()
max_components <- 5L
for (ncomp in seq_len(max_components)) {
    predicted <- rep(NA_integer_, nrow(x_clr))
    for (fold in sort(unique(folds))) {
        train <- folds != fold
        fit <- fit_pls2(x_clr[train, , drop = FALSE], one_hot(y[train]), ncomp)
        score <- predict_pls2(fit, x_clr[!train, , drop = FALSE])
        predicted[!train] <- max.col(score)
    }
    pls_rows[[ncomp]] <- cbind(ncomp = ncomp, classification_metrics(y, predicted))
}
pls_cv <- do.call(rbind, pls_rows)
pls_ncomp <- min(pls_cv$ncomp[pls_cv$balanced_accuracy == max(pls_cv$balanced_accuracy)])
pls_fit <- fit_pls2(x_clr, one_hot(y), pls_ncomp)
pls_score <- predict_pls2(pls_fit, x_clr)
pls_class <- max.col(pls_score)
vip_binary <- pls_vip(pls_fit)
binary_vip <- data.frame(component_id = component_id, chemical = chemical_name, vip = vip_binary,
                         rank = rank(-vip_binary, ties.method = "min"), important = vip_binary > 1)
write_csv(pls_cv, "plsda_binary_grouped_cv.csv")
write_csv(binary_vip[order(binary_vip$rank), ], "plsda_binary_vip.csv")

# Two latent dimensions are exported solely for the requested score plot.  The
# one-component fit above remains the selected classifier and its validation is
# kept separate from this visual display.
pls_display_fit <- fit_pls2(x_clr, one_hot(y), 2L)
pls_display_cv <- pls_cv[pls_cv$ncomp == 2L, , drop = FALSE]
write_csv(data.frame(
    sample_name = model$sample_name, artifact_id = model$artifact_id,
    glass_type = model$glass_type, class_code = y,
    component_1 = pls_display_fit$scores[, 1], component_2 = pls_display_fit$scores[, 2],
    stringsAsFactors = FALSE
), "plsda_binary_display_scores.csv")
write_csv(data.frame(
    purpose = "two_component_score_plot_only", ncomp = 2L,
    grouped_cv_balanced_accuracy = pls_display_cv$balanced_accuracy,
    grouped_cv_macro_f1 = pls_display_cv$macro_f1,
    stringsAsFactors = FALSE
), "plsda_binary_display_diagnostics.csv")

classification <- data.frame(
    sample_name = model$sample_name, artifact_id = model$artifact_id, glass_type = model$glass_type,
    class_code = y, tree_class_code = tree_class, tree_prob_lead_barium = tree_prob[, "1"],
    tree_prob_high_potassium = tree_prob[, "2"], rank_lead_barium = rank(-tree_prob[, "1"], ties.method = "min"),
    rank_high_potassium = rank(-tree_prob[, "2"], ties.method = "min"), plsda_class_code = pls_class,
    plsda_score_lead_barium = pls_score[, 1], plsda_score_high_potassium = pls_score[, 2],
    stringsAsFactors = FALSE
)
write_csv(classification, "binary_classification_points.csv")
write_csv(data.frame(metric = c("tree_grouped_cv_best_balanced_accuracy", "plsda_grouped_cv_balanced_accuracy",
                                "tree_training_balanced_accuracy", "plsda_training_balanced_accuracy"),
                     value = c(max(tree_cv$balanced_accuracy), max(pls_cv$balanced_accuracy),
                               classification_metrics(y, tree_class)$balanced_accuracy,
                               classification_metrics(y, pls_class)$balanced_accuracy)),
          "binary_model_metrics.csv")

# Class-specific K-means.  The five most variable CLR coordinates within each glass type
# are selected before clustering.  This is a fixed, transparent filter, not a label-aware
# classifier feature search.  The selected K values jointly respect the elbow/silhouette
# diagnostics, initialization stability, and a minimum final-cluster size of three points.
cluster_outputs <- list()
subclass_vips <- list()
selection_rows <- list()
sensitivity_rows <- list()
for (class_code in c(1L, 2L)) {
    class_name <- if (class_code == 1L) "lead_barium" else "high_potassium"
    idx <- which(y == class_code)
    x_group <- x_clr[idx, , drop = FALSE]
    dispersion <- apply(x_group, 2, mad, constant = 1)
    selected <- order(dispersion, decreasing = TRUE)[seq_len(5L)]
    selected_names <- clr_name[selected]
    z_group <- scale(x_group[, selected, drop = FALSE])
    z_group <- as.matrix(z_group)
    k_values <- 2:min(6L, floor(nrow(z_group) / 3L))
    grid <- cluster_grid(z_group, k_values, seed = 20260804L + class_code)
    selected_k <- if (class_code == 1L) 5L else 4L
    assert_that(selected_k %in% grid$k, "Selected cluster number is not admissible.")
    baseline <- fit_ordered_kmeans(z_group, selected_k, seed = 20260804L + class_code)
    assert_that(min(baseline$size) >= 3L, "Selected K creates a cluster smaller than three points.")

    center_clr <- matrix(NA_real_, selected_k, ncol(x_clr), dimnames = list(NULL, clr_name))
    for (cluster_id in seq_len(selected_k)) center_clr[cluster_id, ] <- colMeans(x_group[baseline$cluster == cluster_id, , drop = FALSE])
    center_composition <- inverse_clr(center_clr)
    centers <- data.frame(class_name = class_name, cluster_id = seq_len(selected_k), cluster_size = baseline$size,
                          center_composition, check.names = FALSE)
    names(centers)[4:ncol(centers)] <- component_id
    write_csv(centers, paste0(class_name, "_cluster_centers.csv"))

    assigned <- data.frame(sample_name = model$sample_name[idx], artifact_id = model$artifact_id[idx],
                           glass_type = model$glass_type[idx], class_name = class_name,
                           cluster_id = baseline$cluster, stringsAsFactors = FALSE)
    write_csv(assigned[order(assigned$cluster_id, assigned$artifact_id, assigned$sample_name), ],
              paste0(class_name, "_subclass_assignments.csv"))
    cluster_outputs[[class_name]] <- assigned

    feature_table <- data.frame(component_id = component_id, chemical = chemical_name, within_type_mad = dispersion,
                                selected_for_kmeans = seq_along(component_id) %in% selected,
                                selection_rank = rank(-dispersion, ties.method = "min"))
    write_csv(feature_table[order(feature_table$selection_rank), ], paste0(class_name, "_feature_selection.csv"))
    grid$class_name <- class_name
    grid$selected_k <- selected_k
    selection_rows[[class_name]] <- grid

    # PLS-DA describes which components separate the data-driven clusters.  It is not used
    # to validate the clustering, because the cluster labels were inferred from these data.
    subclass_y <- baseline$cluster
    ncomp_subclass <- min(3L, selected_k - 1L)
    subclass_fit <- fit_pls2(x_group, one_hot(subclass_y), ncomp_subclass)
    subclass_vip <- pls_vip(subclass_fit)
    subclass_vips[[class_name]] <- data.frame(
        class_name = class_name, component_id = component_id, chemical = chemical_name,
        vip = subclass_vip, rank = rank(-subclass_vip, ties.method = "min"), important = subclass_vip > 1,
        stringsAsFactors = FALSE
    )
    write_csv(data.frame(
        sample_name = model$sample_name[idx], artifact_id = model$artifact_id[idx],
        glass_type = model$glass_type[idx], class_name = class_name,
        cluster_id = baseline$cluster,
        component_1 = subclass_fit$scores[, 1], component_2 = subclass_fit$scores[, 2],
        stringsAsFactors = FALSE
    ), paste0(class_name, "_subclass_plsda_scores.csv"))

    # PCA is a display-only projection of the already selected K-means space;
    # it neither supplies clusters nor changes their assignments.
    pca_display <- prcomp(z_group, center = FALSE, scale. = FALSE)
    explained <- pca_display$sdev^2 / sum(pca_display$sdev^2)
    write_csv(data.frame(
        sample_name = model$sample_name[idx], artifact_id = model$artifact_id[idx],
        glass_type = model$glass_type[idx], class_name = class_name,
        cluster_id = baseline$cluster,
        pc1 = pca_display$x[, 1], pc2 = pca_display$x[, 2],
        pc1_explained = explained[1], pc2_explained = explained[2],
        stringsAsFactors = FALSE
    ), paste0(class_name, "_cluster_pca_display.csv"))

    top4 <- scale(x_group[, selected[seq_len(4L)], drop = FALSE])
    top6_index <- order(dispersion, decreasing = TRUE)[seq_len(6L)]
    top6 <- scale(x_group[, top6_index, drop = FALSE])
    sensitivity <- cluster_sensitivity(
        z_group, baseline, list(top4_features = as.matrix(top4), top6_features = as.matrix(top6)),
        selected_k, seed = 20260804L + class_code
    )
    sensitivity$class_name <- class_name
    sensitivity_rows[[class_name]] <- sensitivity
}
write_csv(do.call(rbind, selection_rows), "kmeans_selection_metrics.csv")
write_csv(do.call(rbind, subclass_vips)[order(do.call(rbind, subclass_vips)$class_name, do.call(rbind, subclass_vips)$rank), ],
          "subclass_plsda_vip.csv")
sensitivity_all <- do.call(rbind, sensitivity_rows)
write_csv(sensitivity_all, "kmeans_sensitivity.csv")
sensitivity_summary <- aggregate(
    cbind(adjusted_rand_index, withinss) ~ class_name + scenario,
    data = sensitivity_all, FUN = function(x) c(mean = mean(x), min = min(x), max = max(x))
)
sensitivity_summary <- data.frame(
    class_name = sensitivity_summary$class_name,
    scenario = sensitivity_summary$scenario,
    mean_adjusted_rand_index = sensitivity_summary$adjusted_rand_index[, "mean"],
    min_adjusted_rand_index = sensitivity_summary$adjusted_rand_index[, "min"],
    max_adjusted_rand_index = sensitivity_summary$adjusted_rand_index[, "max"],
    mean_withinss = sensitivity_summary$withinss[, "mean"],
    stringsAsFactors = FALSE
)
write_csv(sensitivity_summary, "kmeans_sensitivity_summary.csv")

summary_lines <- c(
    "Problem 2 calculation: PASS",
    "Authoritative calculation: R script run_problem2.R.",
    "Inputs: preprocessing snapshots generated by preprocess.m; original question workbook was not modified.",
    sprintf("Valid sampling points=%d; valid artifacts=%d; lead-barium=%d; high-potassium=%d.",
            nrow(model), length(unique(model$artifact_id)), sum(y == 1L), sum(y == 2L)),
    sprintf("Decision tree: root=%s, threshold=%.10f, nodes=%d, grouped-CV balanced accuracy=%.4f.",
            tree_rule$root_component, tree_rule$root_threshold, tree_rule$node_count, max(tree_cv$balanced_accuracy)),
    sprintf("PLS-DA: components=%d, grouped-CV balanced accuracy=%.4f.", pls_ncomp, max(pls_cv$balanced_accuracy)),
    "Subclass K-means: selected top-5 within-type MAD CLR coordinates; K=5 for lead-barium and K=4 for high-potassium; every final cluster has at least three points.",
    "No figures were generated in this run."
)
writeLines(summary_lines, file.path(out_dir, "run_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
