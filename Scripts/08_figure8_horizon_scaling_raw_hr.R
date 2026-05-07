# ============================================================
# Figure8_HorizonScaling_RAWHR.R  (FULL REPLACEMENT)
#
# PURPOSE
#   Build Figure 8 for the paper:
#     "Error scaling with observational horizon"
#
#   Uses:
#     Data/NUBI_Data_<RES>sec_Level_MASTER_CLEAN.csv
#     Results/nubi_ml/<latest matching v12+ run for chosen RES>/
#       predictions_all_models_both_strata.csv
#
# MAIN IDEA
#   For ENet predictions, quantify how prediction error in RAW_HR changes
#   when residuals are averaged over longer temporal horizons.
#
# MAIN FIGURE
#   All strata
#   RAW_HR residuals only
#
# COMPANION OUTPUT
#   Horizon metrics tables
#   Log-log slope table
#   Reduction summary table
#   Caption-friendly numbers
#
# KEY IMPROVEMENTS VS PRIOR VERSION
#   - Interactive resolution picker: 10 / 30 / 60 sec
#   - RAW_HR only (drops redundant NHR)
#   - Auto-detects newest matching ML run for chosen resolution
#   - Prefers highest detected ML version (so v12 over v11 automatically)
#   - Writes block-count diagnostics so long horizons can be judged honestly
#   - Removes inverse-square-root benchmark from plots and tables
#
# OUTPUT
#   Results/paper_figs/<timestamp>_<RES>sec_Figure8_HorizonScaling_RAWHR/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(scales)
})

options(warn = 1)

# ----------------------------
# USER SETTINGS
# ----------------------------
LOCAL_TZ <- "America/Chicago"
USE_COMMON_SUBJECTS <- TRUE
AUTO_DETECT_ML_RUN <- TRUE

# Only used if AUTO_DETECT_ML_RUN = FALSE
ML_RUN_FOLDER_MANUAL <- "Results/nubi_ml/REPLACE_WITH_REAL_FOLDER"

MODEL_KEEP <- "enet"

# Horizons in MINUTES to test
HORIZONS_MIN <- c(1, 2, 5, 10, 15, 30, 60)

# Segment rule
GAP_MULTIPLIER <- 1.5

# Reliability safeguard:
# plot will show all horizons, but sparse ones are flagged
MIN_BLOCKS_WARN <- 30L

# Colors
COL_DRIVING <- "#E69F00"
COL_NOND    <- "#7F7F7F"

# ----------------------------
# Project root
# ----------------------------
this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NA_character_)
if (is.na(this_file)) stop("Could not locate script path (sys.frames()[[1]]$ofile).")

PROJECT_ROOT <- dirname(dirname(this_file))
setwd(PROJECT_ROOT)
message("Working directory set to PROJECT_ROOT: ", normalizePath(getwd()))

# ============================================================
# Helpers
# ============================================================
pick_resolution <- function() {
  cat(
    "\nChoose dataset resolution:\n",
    "  1) 10 sec\n",
    "  2) 30 sec\n",
    "  3) 60 sec\n",
    sep = ""
  )
  ans <- trimws(readline("Enter 10 / 30 / 60 (or 1/2/3): "))
  
  if (ans %in% c("1", "10")) return(10L)
  if (ans %in% c("2", "30")) return(30L)
  if (ans %in% c("3", "60")) return(60L)
  
  stop("Invalid entry: ", ans, " (expected 10/30/60 or 1/2/3)")
}

snakeify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

parse_time_local <- function(x, tz = LOCAL_TZ) {
  if (inherits(x, "POSIXt")) return(with_tz(x, tzone = tz))
  if (!is.character(x)) x <- as.character(x)
  
  x2 <- gsub("Z$", "", x)
  
  tt <- suppressWarnings(lubridate::ymd_hms(x2, tz = tz, quiet = TRUE))
  if (!all(is.na(tt))) return(tt)
  
  suppressWarnings(lubridate::parse_date_time(
    x2,
    orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM"),
    tz = tz
  ))
}

canonicalize_names <- function(dt) {
  stopifnot(is.data.table(dt))
  
  old <- names(dt)
  sn  <- snakeify(old)
  
  canon <- c(
    p_id="p_id", pid="p_id", participant_id="p_id", participantid="p_id",
    time="time", timestamp="time", datetime="time", date_time="time",
    raw_hr="raw_hr",
    nhr="nhr",
    activity="activity",
    activity3="activity3",
    day_num="day_num",
    daynum="day_num",
    days="days",
    bl_hr="bl_hr",
    hr_bl="bl_hr",
    hrbl="bl_hr",
    baseline_hr="bl_hr",
    baseline="bl_hr",
    hr_baseline="bl_hr"
  )
  
  new <- sn
  hit <- sn %in% names(canon)
  new[hit] <- unname(canon[sn[hit]])
  
  if (!identical(old, new)) {
    new <- make.unique(new, sep = "_")
    setnames(dt, old, new)
  }
  dt
}

make_day_key <- function(dt) {
  if ("day_num" %in% names(dt)) {
    x <- dt[["day_num"]]
    if (is.numeric(x) || is.integer(x)) return(as.integer(x))
    xs <- as.character(x)
    dig <- suppressWarnings(as.integer(stringr::str_extract(xs, "\\d+")))
    if (!all(is.na(dig))) return(dig)
  }
  
  if ("days" %in% names(dt)) {
    x <- dt[["days"]]
    if (is.numeric(x) || is.integer(x)) return(as.integer(x))
    xs <- as.character(x)
    dig <- suppressWarnings(as.integer(stringr::str_extract(xs, "\\d+")))
    if (!all(is.na(dig))) return(dig)
  }
  
  if ("dt_time" %in% names(dt) && inherits(dt[["dt_time"]], "POSIXt")) {
    d <- as.Date(dt[["dt_time"]], tz = LOCAL_TZ)
    u <- sort(unique(d))
    return(as.integer(match(d, u)))
  }
  
  NULL
}

safe_save_pdf <- function(plot_obj, path, w = 8, h = 5) {
  ok <- tryCatch({
    ggsave(
      filename = path,
      plot = plot_obj,
      width = w,
      height = h,
      device = grDevices::cairo_pdf
    )
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) {
    ggsave(
      filename = path,
      plot = plot_obj,
      width = w,
      height = h,
      device = "pdf",
      useDingbats = FALSE
    )
  }
}

safe_save_png <- function(plot_obj, path, w = 8, h = 5, dpi = 300) {
  ggsave(filename = path, plot = plot_obj, width = w, height = h, dpi = dpi)
}

fit_loglog <- function(df_metric) {
  ok <- is.finite(df_metric$horizon_min) &
    is.finite(df_metric$value) &
    df_metric$horizon_min > 0 &
    df_metric$value > 0
  
  dd <- df_metric[ok, , drop = FALSE]
  if (nrow(dd) < 3) {
    return(data.frame(
      intercept = NA_real_,
      slope = NA_real_,
      r2 = NA_real_,
      n_horizons = nrow(dd)
    ))
  }
  
  mod <- lm(log(value) ~ log(horizon_min), data = dd)
  ss <- summary(mod)
  
  data.frame(
    intercept = unname(coef(mod)[1]),
    slope = unname(coef(mod)[2]),
    r2 = unname(ss$r.squared),
    n_horizons = nrow(dd)
  )
}

extract_version_num <- function(x) {
  x_low <- tolower(x)
  m <- stringr::str_match(x_low, "v([0-9]+)")
  out <- suppressWarnings(as.numeric(m[, 2]))
  ifelse(is.na(out), -Inf, out)
}

find_latest_ml_dir <- function(res_seconds) {
  base_dir <- file.path("Results", "nubi_ml")
  if (!dir.exists(base_dir)) stop("Missing Results/nubi_ml directory.")
  
  all_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  if (length(all_dirs) == 0) stop("No run folders found under Results/nubi_ml/")
  
  bn <- basename(all_dirs)
  
  res_pat <- paste0("(^|[^0-9])", res_seconds, "sec([^0-9]|$)")
  
  hits <- all_dirs[
    grepl(res_pat, bn, ignore.case = TRUE, perl = TRUE) &
      grepl("compare_drive_vs_nondrive", bn, ignore.case = TRUE, perl = TRUE) &
      grepl("directrawhr", bn, ignore.case = TRUE, perl = TRUE) &
      grepl("importance", bn, ignore.case = TRUE, perl = TRUE) &
      grepl("noaffine", bn, ignore.case = TRUE, perl = TRUE)
  ]
  
  if (length(hits) == 0) {
    message("Folders found under Results/nubi_ml/:")
    print(bn)
    stop("No matching ", res_seconds, "sec ML run folder found.")
  }
  
  rank_df <- data.frame(
    path = hits,
    version_num = extract_version_num(basename(hits)),
    mtime = file.info(hits)$mtime,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(version_num), desc(mtime))
  
  rank_df$path[1]
}

# ============================================================
# Resolution
# ============================================================
RES_SECONDS <- pick_resolution()

DATA_PATH <- file.path("Data", sprintf("NUBI_Data_%dsec_Level_MASTER_CLEAN.csv", RES_SECONDS))
if (!file.exists(DATA_PATH)) stop("Missing input data file: ", DATA_PATH)

# ----------------------------
# Output folder
# ----------------------------
timestamp_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  "Results", "paper_figs",
  paste0(timestamp_tag, "_", RES_SECONDS, "sec_Figure8_HorizonScaling_RAWHR")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("Writing outputs to: ", normalizePath(out_dir))

# ============================================================
# Resolve ML directory
# ============================================================
if (AUTO_DETECT_ML_RUN) {
  ML_DIR <- normalizePath(find_latest_ml_dir(RES_SECONDS), mustWork = TRUE)
  message("AUTO_DETECT_ML_RUN = TRUE")
  message("Using latest ML_DIR: ", ML_DIR)
} else {
  ML_DIR <- normalizePath(ML_RUN_FOLDER_MANUAL, mustWork = FALSE)
  if (!dir.exists(ML_DIR)) {
    message("Folders found under Results/nubi_ml/:")
    print(list.files(file.path("Results", "nubi_ml")))
    stop("ML_RUN_FOLDER_MANUAL does not exist: ", ML_RUN_FOLDER_MANUAL)
  }
  ML_DIR <- normalizePath(ML_DIR, mustWork = TRUE)
  message("AUTO_DETECT_ML_RUN = FALSE")
  message("Using manual ML_DIR: ", ML_DIR)
}

PRED_PATH <- file.path(ML_DIR, "predictions_all_models_both_strata.csv")
if (!file.exists(PRED_PATH)) stop("Missing prediction file: ", PRED_PATH)

# ============================================================
# Step 1: Read MASTER data and reconstruct row mapping
# ============================================================
message("Reading MASTER data ...")
dt_full <- fread(DATA_PATH, showProgress = TRUE)
dt_full <- canonicalize_names(dt_full)

need <- c("p_id", "time", "raw_hr", "activity3", "bl_hr")
miss <- setdiff(need, names(dt_full))
if (length(miss) > 0) {
  stop("Missing required columns after standardization: ", paste(miss, collapse = ", "))
}

dt_full[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
dt_full <- dt_full[!is.na(dt_time)]

dt_full[, activity3_norm := trimws(tolower(as.character(activity3)))]
ACT3_DRIVING <- "driving"
ACT3_ND_SED  <- "non_driving_sedentary"

day_key <- make_day_key(dt_full)
if (is.null(day_key)) stop("Could not construct day key from MASTER dataset.")
dt_full[, day_key := day_key]

dt_drive <- dt_full[activity3_norm == ACT3_DRIVING]
dt_nond  <- dt_full[activity3_norm == ACT3_ND_SED]

if (nrow(dt_drive) == 0) stop("No DRIVING rows found.")
if (nrow(dt_nond) == 0) stop("No NONDRIVING_SEDENTARY rows found.")

if (USE_COMMON_SUBJECTS) {
  common_subj <- intersect(unique(dt_drive$p_id), unique(dt_nond$p_id))
  dt_drive <- dt_drive[p_id %in% common_subj]
  dt_nond  <- dt_nond[p_id %in% common_subj]
  message("USE_COMMON_SUBJECTS = TRUE | common subjects = ", length(common_subj))
}

setorderv(dt_drive, c("p_id", "dt_time"))
setorderv(dt_nond,  c("p_id", "dt_time"))

rowmap_drive <- copy(dt_drive)[
  , .(p_id, dt_time, day_key, stratum = "DRIVING")
][
  , row_id := seq_len(.N)
]

rowmap_nond <- copy(dt_nond)[
  , .(p_id, dt_time, day_key, stratum = "NONDRIVING_SEDENTARY")
][
  , row_id := seq_len(.N)
]

rowmap <- rbindlist(list(rowmap_drive, rowmap_nond), use.names = TRUE)

fwrite(rowmap_drive, file.path(out_dir, "diagnostics_rowmap_DRIVING.csv"))
fwrite(rowmap_nond,  file.path(out_dir, "diagnostics_rowmap_NONDRIVING_SEDENTARY.csv"))

# ============================================================
# Step 2: Read predictions and join row mapping
# ============================================================
message("Reading predictions ...")
pred <- fread(PRED_PATH)

need_pred <- c("model", "stratum", "fold", "row_id", "raw_hr_obs", "raw_hr_hat")
miss_pred <- setdiff(need_pred, names(pred))
if (length(miss_pred) > 0) {
  stop("Prediction file missing columns: ", paste(miss_pred, collapse = ", "))
}

pred <- pred[model == MODEL_KEEP]
if (nrow(pred) == 0) stop("No rows left after filtering to model == ", MODEL_KEEP)

pred <- merge(
  pred,
  rowmap,
  by = c("stratum", "row_id"),
  all.x = TRUE,
  all.y = FALSE
)

if (pred[, any(is.na(p_id) | is.na(dt_time))]) {
  bad_n <- pred[is.na(p_id) | is.na(dt_time), .N]
  stop("Failed to reconstruct row mapping for ", bad_n, " prediction rows.")
}

pred[, raw_resid := as.numeric(raw_hr_obs) - as.numeric(raw_hr_hat)]
fwrite(pred[1:min(.N, 5000)], file.path(out_dir, "diagnostics_joined_prediction_sample.csv"))

# ============================================================
# Step 3: Build contiguous temporal segments within subject
# ============================================================
setorderv(pred, c("stratum", "p_id", "dt_time"))

expected_gap_sec <- RES_SECONDS
gap_cut_sec <- GAP_MULTIPLIER * expected_gap_sec

pred[, dt_diff_sec := as.numeric(difftime(dt_time, shift(dt_time), units = "secs")), by = .(stratum, p_id)]
pred[, day_key_prev := shift(day_key), by = .(stratum, p_id)]

pred[, new_segment := fifelse(
  is.na(dt_diff_sec) |
    dt_diff_sec <= 0 |
    dt_diff_sec > gap_cut_sec |
    is.na(day_key_prev) |
    day_key != day_key_prev,
  1L, 0L
), by = .(stratum, p_id)]

pred[, segment_id := cumsum(new_segment), by = .(stratum, p_id)]

seg_diag <- pred[, .(
  n_rows = .N,
  duration_min = .N * RES_SECONDS / 60,
  t_start = min(dt_time),
  t_end   = max(dt_time)
), by = .(stratum, p_id, segment_id)]

fwrite(seg_diag, file.path(out_dir, "diagnostics_segments.csv"))

seg_summary <- seg_diag[, .(
  n_segments = .N,
  median_duration_min = median(duration_min, na.rm = TRUE),
  mean_duration_min = mean(duration_min, na.rm = TRUE),
  p90_duration_min = as.numeric(quantile(duration_min, 0.90, na.rm = TRUE)),
  max_duration_min = max(duration_min, na.rm = TRUE)
), by = stratum]

fwrite(seg_summary, file.path(out_dir, "diagnostics_segment_summary.csv"))

# ============================================================
# Step 4: Aggregate residuals over horizons
# ============================================================
compute_horizon_blocks <- function(dt_in, horizon_min, res_seconds) {
  k <- as.integer(round((horizon_min * 60) / res_seconds))
  if (k < 1L) stop("Invalid horizon_min -> k < 1")
  
  dd <- copy(dt_in)
  
  block_dt <- dd[
    order(stratum, p_id, dt_time),
    {
      idx <- seq_len(.N)
      block_id <- ((idx - 1L) %/% k) + 1L
      .(
        horizon_min = horizon_min,
        block_id = block_id,
        raw_resid = raw_resid
      )
    },
    by = .(stratum, p_id, segment_id)
  ][
    ,
    .(
      n_rows = .N,
      raw_mean_resid = mean(raw_resid, na.rm = TRUE)
    ),
    by = .(stratum, p_id, segment_id, horizon_min, block_id)
  ][
    n_rows == k
  ]
  
  block_dt[]
}

message("Computing horizon blocks ...")
blocks_list <- lapply(HORIZONS_MIN, function(hm) compute_horizon_blocks(pred, hm, RES_SECONDS))
blocks_all <- rbindlist(blocks_list, use.names = TRUE, fill = TRUE)

if (nrow(blocks_all) == 0) stop("No complete blocks were formed at the requested horizons.")

fwrite(blocks_all, file.path(out_dir, "Figure8_block_means_long.csv"))

block_counts <- blocks_all[, .(
  n_blocks = .N,
  n_subject_segments = uniqueN(paste(stratum, p_id, segment_id, sep = "__"))
), by = .(stratum, horizon_min)]

block_counts[, warn_sparse := n_blocks < MIN_BLOCKS_WARN]

fwrite(block_counts, file.path(out_dir, "Figure8_block_counts_by_horizon.csv"))

# ============================================================
# Step 5: Horizon metrics
# ============================================================
metrics <- blocks_all[
  ,
  .(
    rmse = sqrt(mean(raw_mean_resid^2, na.rm = TRUE)),
    mae  = mean(abs(raw_mean_resid), na.rm = TRUE),
    sd   = sd(raw_mean_resid, na.rm = TRUE),
    n_blocks = .N
  ),
  by = .(stratum, horizon_min)
]

metrics_long <- melt(
  metrics,
  id.vars = c("stratum", "horizon_min", "n_blocks"),
  measure.vars = c("rmse", "mae", "sd"),
  variable.name = "metric",
  value.name = "value"
)

setorder(metrics_long, stratum, metric, horizon_min)

metrics_long <- merge(
  metrics_long,
  block_counts[, .(stratum, horizon_min, warn_sparse)],
  by = c("stratum", "horizon_min"),
  all.x = TRUE
)

metrics_long[, baseline_value := value[horizon_min == min(horizon_min)], by = .(stratum, metric)]
metrics_long[, value_norm := value / baseline_value]

fwrite(metrics_long, file.path(out_dir, "Figure8_horizon_metrics_long.csv"))

# ============================================================
# Step 6: Log-log slopes
# ============================================================
slopes <- metrics_long[
  ,
  {
    fit <- fit_loglog(data.frame(horizon_min = horizon_min, value = value))
    as.list(fit[1, ])
  },
  by = .(stratum, metric)
]

fwrite(slopes, file.path(out_dir, "Figure8_loglog_slopes.csv"))

# ============================================================
# Step 7: Reduction summary
# ============================================================
min_h <- min(HORIZONS_MIN)
max_h <- max(HORIZONS_MIN)

reduction_summary <- metrics_long[
  horizon_min %in% c(min_h, max_h),
  .(value = value[1], n_blocks = n_blocks[1], horizon_min = horizon_min[1]),
  by = .(stratum, metric, horizon_min)
][
  ,
  .(
    value_at_min = value[horizon_min == min_h],
    value_at_max = value[horizon_min == max_h],
    n_blocks_at_min = n_blocks[horizon_min == min_h],
    n_blocks_at_max = n_blocks[horizon_min == max_h]
  ),
  by = .(stratum, metric)
][
  ,
  pct_reduction := 100 * (1 - value_at_max / value_at_min)
]

fwrite(reduction_summary, file.path(out_dir, "Figure8_reduction_summary.csv"))

caption_numbers <- merge(
  slopes[stratum == "DRIVING" & metric == "rmse",
         .(stratum, rmse_loglog_slope = slope, rmse_loglog_r2 = r2, n_horizons)],
  reduction_summary[stratum == "DRIVING" & metric == "rmse",
                    .(stratum, rmse_pct_reduction_1_to_max = pct_reduction,
                      n_blocks_at_min, n_blocks_at_max)],
  by = "stratum",
  all = TRUE
)

fwrite(caption_numbers, file.path(out_dir, "Figure8_caption_numbers.csv"))

# ============================================================
# Step 8: Figure
# ============================================================
plot_all <- metrics_long[metric == "rmse"]

plot_all[, stratum_plot := factor(
  stratum,
  levels = c("DRIVING", "NONDRIVING_SEDENTARY"),
  labels = c("Driving", "Non-driving sedentary")
)]

p_all <- ggplot(
  plot_all,
  aes(x = horizon_min, y = value_norm, color = stratum_plot, group = stratum_plot)
) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(shape = warn_sparse), size = 2.4, stroke = 1.0) +
  scale_color_manual(
    values = c(
      "Driving" = COL_DRIVING,
      "Non-driving sedentary" = COL_NOND
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(`FALSE` = 16, `TRUE` = 1),
    guide = "none"
  ) +
  scale_x_log10(
    breaks = HORIZONS_MIN,
    labels = HORIZONS_MIN
  ) +
  scale_y_log10(
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Averaging horizon [minutes, log scale]",
    y = "Normalized RMSE",
    title = NULL,
    subtitle = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

safe_save_pdf(p_all, file.path(out_dir, "Figure8_HorizonScaling_RAWHR.pdf"), w = 8.8, h = 5.8)
safe_save_png(p_all, file.path(out_dir, "Figure8_HorizonScaling_RAWHR.png"), w = 8.8, h = 5.8, dpi = 300)

# Raw RMSE log-log diagnostic
diag_plot <- copy(metrics_long[metric == "rmse"])
diag_plot[, stratum_plot := factor(
  stratum,
  levels = c("DRIVING", "NONDRIVING_SEDENTARY"),
  labels = c("Driving", "Non-driving sedentary")
)]

p_diag <- ggplot(diag_plot, aes(x = horizon_min, y = value, color = stratum_plot, group = stratum_plot)) +
  geom_line(linewidth = 0.95) +
  geom_point(aes(shape = warn_sparse), size = 2.2, stroke = 1.0) +
  scale_color_manual(
    values = c(
      "Driving" = COL_DRIVING,
      "Non-driving sedentary" = COL_NOND
    ),
    name = NULL
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  scale_x_log10(breaks = HORIZONS_MIN, labels = HORIZONS_MIN) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  labs(
    x = "Averaging horizon (minutes, log scale)",
    y = "RMSE of block-mean residuals (log scale)",
    title = "Log-log diagnostic for RAW_HR horizon scaling"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

safe_save_pdf(p_diag, file.path(out_dir, "Figure8_HorizonScaling_LogLogDiagnostic.pdf"), w = 8.8, h = 5.8)
safe_save_png(p_diag, file.path(out_dir, "Figure8_HorizonScaling_LogLogDiagnostic.png"), w = 8.8, h = 5.8, dpi = 300)

# ============================================================
# Step 9: Minimal console summary
# ============================================================
message("Done.")
message("Resolution used: ", RES_SECONDS, " sec")
message("ML_DIR used: ", ML_DIR)
message("Main figure PDF: ", normalizePath(file.path(out_dir, "Figure8_HorizonScaling_RAWHR.pdf")))
message("Diagnostic PDF:  ", normalizePath(file.path(out_dir, "Figure8_HorizonScaling_LogLogDiagnostic.pdf")))
message("Metrics CSV:     ", normalizePath(file.path(out_dir, "Figure8_horizon_metrics_long.csv")))
message("Block counts CSV:", normalizePath(file.path(out_dir, "Figure8_block_counts_by_horizon.csv")))
message("Slopes CSV:      ", normalizePath(file.path(out_dir, "Figure8_loglog_slopes.csv")))
message("Reduction CSV:   ", normalizePath(file.path(out_dir, "Figure8_reduction_summary.csv")))
message("Caption CSV:     ", normalizePath(file.path(out_dir, "Figure8_caption_numbers.csv")))