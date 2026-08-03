#!/usr/bin/env Rscript
# Independent numerical verification using MATLAB's three raw CSV snapshots.
args <- commandArgs(trailingOnly = FALSE)
script.arg <- grep('^--file=', args, value = TRUE)
script.path <- normalizePath(sub('^--file=', '', script.arg[1]))
root <- normalizePath(file.path(dirname(script.path), '..'))
snap <- file.path(root, 'data', 'verification_snapshots')
read.csv8 <- function(path) read.csv(path, check.names = FALSE, fileEncoding = 'UTF-8', stringsAsFactors = FALSE)
s1 <- read.csv8(file.path(snap, 'sheet1_raw_snapshot.csv'))
s2 <- read.csv8(file.path(snap, 'sheet2_raw_snapshot.csv'))
s3 <- read.csv8(file.path(snap, 'sheet3_raw_snapshot.csv'))
matlab <- read.csv8(file.path(snap, 'matlab_key_results.csv'))
x <- as.matrix(s2[, 5:18]); storage.mode(x) <- 'double'
s <- rowSums(x); valid <- s >= 85 & s <= 105
stopifnot(nrow(s1) == 58, nrow(s2) == 69, nrow(s3) == 8, sum(valid) == 67)
stopifnot(all(s3$composition_sum >= 85 & s3$composition_sum <= 105))
close <- function(a) 100 * a / rowSums(a)
replace.zero <- function(a, d = 0.02) {
  ans <- a
  for (i in seq_len(nrow(a))) { z <- a[i, ] == 0; k <- sum(z); if (k > 0) { ans[i, z] <- d; ans[i, !z] <- a[i, !z] * (100-k*d)/100 } }
  ans
}
clr <- function(a) log(a / exp(rowMeans(log(a))))
closed <- close(x[valid, ]); replaced <- replace.zero(closed); z <- clr(replaced)
matlab.z <- as.matrix(matlab[, grep('^CLR_', names(matlab))]); storage.mode(matlab.z) <- 'double'
max.diff <- max(abs(z - matlab.z)); row.err <- max(abs(rowSums(replaced)-100)); nd <- sum(x[valid, ]==0); min.pos <- min(x[valid, ][x[valid, ]>0])
stopifnot(abs(min.pos-0.04)<1e-12, max.diff <= 1e-8, row.err <= 1e-8)
lines <- c('R independent preprocessing verification: PASS', sprintf('Artifacts=%d; raw points=%d; valid=%d; unknown=%d.',nrow(s1),nrow(s2),sum(valid),nrow(s3)), sprintf('Invalid sums: 15=%.2f; 17=%.2f.',s[s2$sample_name=='15'],s[s2$sample_name=='17']), sprintf('Undetected valid cells=%d (%.2f%%); min positive=%.2f%%.',nd,100*nd/length(x[valid,]),min.pos), sprintf('Max MATLAB-R CLR difference=%.3e; replacement row-sum error=%.3e.',max.diff,row.err))
writeLines(lines, file.path(root,'data','r_preprocess_consistency_report.txt'), useBytes=TRUE); writeLines(lines)
