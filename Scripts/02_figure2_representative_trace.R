# ============================================================
# 02_figure2_representative_trace.R
#
# PURPOSE
#   Generate Figure 2 for the npj Digital Medicine manuscript:
#
#     From Instantaneous Heart Rate to Long-Horizon
#     Cardiovascular Burden in Naturalistic Daily Life
#
#   The script creates a representative single-participant
#   heart-rate trace across study days 1-7.
#
#   Plot contents:
#     - raw heart rate colored by activity context:
#         driving
#         non_driving_sedentary
#         non_driving_physical_activity
#     - participant-day baseline heart rate overlaid in red
#
# INPUT
#   Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
#
# DEFAULT SUBJECT
#   SUBJECT_ID <- "P62"
#
# REQUIRED ACTIVITY LABELS
#   activity3 == "driving"
#   activity3 == "non_driving_sedentary"
#   activity3 == "non_driving_physical_activity"
#
# X-AXIS
#   Study-day placement over a fixed 7-slot layout:
#
#     day 1 = 0-24 h
#     day 2 = 24-48 h
#     ...
#     day 7 = 144-168 h
#
#   The day_num variable is used as the study-day key.
#   These study-day labels are not necessarily consecutive
#   calendar dates.
#
# MAJOR OUTPUTS
#   The script writes Figure 2 and subject-level diagnostics under:
#
#     Results/paper_figs/<timestamp>_60sec_figure2_<SUBJECT_ID>/
#
#   Main outputs:
#
#     Figures/Figure2_<SUBJECT_ID>.pdf
#     Figures/Figure2_<SUBJECT_ID>.png
#     activity3_levels_detected.csv
#     counts_<SUBJECT_ID>.csv
#     subject_summary_<SUBJECT_ID>.csv
#     baseline_study_day_<SUBJECT_ID>.csv
#     missingness_after_fill_<SUBJECT_ID>.csv
#     run_log.txt
#
# REPOSITORY SCOPE
#   This public repository starts from the curated analysis-ready
#   60-second dataset. It does not reconstruct the dataset from
#   raw wearable, smartphone, vehicle, or ground-truth streams.
#
# PRIVACY NOTE
#   Direct GPS coordinate columns are removed from the public
#   dataset and are not required for this figure.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
})

options(warn = 1)

# ----------------------------
# USER SETTINGS
# ----------------------------
LOCAL_TZ <- "America/Chicago"
SUBJECT_ID <- "P62"

KEEP_DAYS <- 7L
X_END_HRS <- 24 * KEEP_DAYS
x_break_by <- 6

# Final validated activity3 labels
ACT3_DRIVING <- "driving"
ACT3_ND_SED  <- "non_driving_sedentary"
ACT3_ND_PA   <- "non_driving_physical_activity"

# Colors
COL_DRIVE <- "orange"
COL_SED   <- "black"
COL_ACT   <- "springgreen3"
COL_BASE  <- "red"

PAL_STATE <- c(
  "DRIVING" = COL_DRIVE,
  "SEDENTARY_NONDRIVING" = COL_SED,
  "PHYSICAL_ACTIVITY" = COL_ACT,
  "Baseline" = COL_BASE
)

# Break lines across gaps > 2 expected bins
GAP_MULT <- 2L

# Output sizing
PDF_W <- 11
PDF_H <- 4.8
PNG_DPI <- 300

# ----------------------------
# Robust wd = Scripts/
# ----------------------------
this_script <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
if (!is.na(this_script) && file.exists(this_script)) setwd(dirname(this_script))
message("Working directory (Scripts): ", getwd())

project_root <- normalizePath(file.path(getwd(), ".."), mustWork = TRUE)

# ----------------------------
# Resolution picker
# ----------------------------
# The public repository currently includes the curated 60-second dataset:
#
#   Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
#
# The picker is retained so the same script can be reused if 10-sec or
# 30-sec analysis datasets are added later.

pick_resolution <- function(default = 60L) {
  cat(
    "\nChoose dataset resolution:\n",
    "  1) 10 sec  [requires Data/NUBI_Data_10sec_Level_MASTER_CLEAN.csv]\n",
    "  2) 30 sec  [requires Data/NUBI_Data_30sec_Level_MASTER_CLEAN.csv]\n",
    "  3) 60 sec  [included in this repository]\n",
    sep = ""
  )
  
  ans <- trimws(readline(
    sprintf("Enter 10 / 30 / 60 (or 1/2/3). Press Enter for %d sec: ", default)
  ))
  
  if (ans == "") return(as.integer(default))
  if (ans %in% c("1", "10")) return(10L)
  if (ans %in% c("2", "30")) return(30L)
  if (ans %in% c("3", "60")) return(60L)
  
  stop("Invalid entry: ", ans, " (expected 10/30/60 or 1/2/3)")
}

RES_SECONDS <- pick_resolution(default = 60L)

in_path <- file.path(
  project_root, "Data",
  sprintf("NUBI_Data_%dsec_Level_MASTER_CLEAN.csv", RES_SECONDS)
)
if (!file.exists(in_path)) {
  stop(
    "Dataset not found: ", in_path, "\n",
    "The public repository currently includes only the 60-second dataset. ",
    "Use 60 sec, or add the corresponding ", RES_SECONDS,
    "-second dataset under Data/."
  )
}

gap_threshold_secs <- as.integer(GAP_MULT * RES_SECONDS)

# ----------------------------
# Output folder
# ----------------------------
stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  project_root,
  "Results",
  "paper_figs",
  paste0(stamp, "_", RES_SECONDS, "sec_figure2_", SUBJECT_ID)
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(out_dir, "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(fig_dir, paste0("Figure2_", SUBJECT_ID, ".pdf"))
png_path <- file.path(fig_dir, paste0("Figure2_", SUBJECT_ID, ".png"))

log_file <- file.path(out_dir, "run_log.txt")
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = log_file, append = TRUE)
}

safe_save_pdf <- function(plot_obj, path, w = 7, h = 5) {
  ok <- tryCatch({
    ggsave(filename = path, plot = plot_obj, width = w, height = h,
           device = grDevices::cairo_pdf)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    ggsave(filename = path, plot = plot_obj, width = w, height = h,
           device = "pdf", useDingbats = FALSE)
  }
}

# ----------------------------
# Helpers
# ----------------------------
norm_chr <- function(x) {
  trimws(tolower(as.character(x)))
}

parse_time_local <- function(x, tz = LOCAL_TZ) {
  if (inherits(x, "POSIXt")) return(force_tz(x, tzone = tz))
  
  x <- as.character(x)
  x2 <- sub("Z$", "", x)
  
  tt <- suppressWarnings(ymd_hms(x2, tz = tz, quiet = TRUE))
  if (sum(!is.na(tt)) > 0) return(tt)
  
  suppressWarnings(parse_date_time(
    x2,
    orders = c(
      "ymd HMS", "ymd HM",
      "mdy HMS", "mdy HM",
      "dmy HMS", "dmy HM",
      "ymd HMSOS", "mdy HMSOS", "dmy HMSOS"
    ),
    tz = tz
  ))
}

parse_day_num <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "n/a", "null")] <- NA_character_
  out <- suppressWarnings(as.integer(str_extract(x, "\\d+")))
  out[!(out %in% 1:7)] <- NA_integer_
  out
}

state_from_activity3 <- function(activity3_vec) {
  a <- norm_chr(activity3_vec)
  out <- ifelse(
    a == ACT3_DRIVING, "DRIVING",
    ifelse(a == ACT3_ND_PA, "PHYSICAL_ACTIVITY", "SEDENTARY_NONDRIVING")
  )
  factor(out, levels = c("DRIVING", "SEDENTARY_NONDRIVING", "PHYSICAL_ACTIVITY"))
}

mode_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

pad_range <- function(r, frac = 0.06) {
  if (length(r) != 2 || any(!is.finite(r))) return(c(0, 1))
  span <- diff(r)
  if (!is.finite(span) || span <= 0) return(r + c(-1, 1))
  r + c(-1, 1) * span * frac
}

clock_labels_7day <- function(x) {
  out <- character(length(x))
  
  near_end <- abs(x - (X_END_HRS - RES_SECONDS / 3600)) < 1e-9
  out[near_end] <- "24:00\n7"
  
  xx <- x[!near_end]
  day_idx <- floor(xx / 24) + 1
  h <- ((xx %% 24) + 24) %% 24
  hh <- floor(h)
  mm <- round((h - hh) * 60)
  mm[mm == 60] <- 0
  hh[hh == 24] <- 0
  hhmm <- sprintf("%02d:%02d", hh, mm)
  out[!near_end] <- paste0(hhmm, "\n", day_idx)
  
  out
}

fill_missing_grid_one_subject <- function(d, step_secs) {
  setorder(d, day_num_int, dt_time)
  
  slots <- 1:KEEP_DAYS
  out_list <- vector("list", length(slots))
  
  for (ii in seq_along(slots)) {
    dd <- slots[ii]
    d_day <- d[day_num_int == dd]
    
    full <- data.table(
      sec_of_day = seq(0, 24 * 3600 - step_secs, by = step_secs)
    )
    full[, `:=`(
      p_id = d$p_id[1],
      day_num_int = dd
    )]
    
    if (nrow(d_day) > 0) {
      tmp <- copy(d_day[, .(
        p_id, day_num_int, sec_of_day, dt_time,
        raw_hr, baseline_hr, state
      )])
      out <- merge(
        full, tmp,
        by = c("p_id", "day_num_int", "sec_of_day"),
        all.x = TRUE, sort = TRUE
      )
    } else {
      out <- copy(full)
      out[, `:=`(
        dt_time = as.POSIXct(NA),
        raw_hr = NA_real_,
        baseline_hr = NA_real_,
        state = factor(
          NA_character_,
          levels = c("DRIVING","SEDENTARY_NONDRIVING","PHYSICAL_ACTIVITY")
        )
      )]
    }
    
    out[!is.finite(raw_hr), `:=`(baseline_hr = NA_real_, state = NA)]
    out_list[[ii]] <- out
  }
  
  rbindlist(out_list, use.names = TRUE, fill = TRUE)
}

make_subject_plot <- function(dsub) {
  setorder(dsub, x_hr, sec_of_day)
  
  dsub[, dx_hr := x_hr - shift(x_hr, 1)]
  gap_threshold_hours <- gap_threshold_secs / 3600
  
  dsub[, new_seg := is.na(dx_hr) |
         (dx_hr < 0) |
         (dx_hr > gap_threshold_hours) |
         (state != shift(state, 1)) |
         (is.na(raw_hr) != shift(is.na(raw_hr), 1))
  ]
  dsub[, seg_id := cumsum(new_seg)]
  
  y_vals <- c(dsub$raw_hr, dsub$baseline_hr)
  y_vals <- y_vals[is.finite(y_vals)]
  y_lim <- if (length(y_vals) > 0) pad_range(range(y_vals)) else c(0, 1)
  
  ggplot(as.data.frame(dsub), aes(x = x_hr)) +
    geom_line(
      aes(y = raw_hr, color = state, group = seg_id),
      linewidth = 0.38,
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = baseline_hr, color = "Baseline", group = seg_id),
      linewidth = 0.48,
      na.rm = TRUE
    ) +
    scale_color_manual(
      values = PAL_STATE,
      breaks = c("DRIVING", "SEDENTARY_NONDRIVING", "PHYSICAL_ACTIVITY", "Baseline"),
      labels = c("Driving", "Sedentary non-driving", "Physical activity", "Participant-day baseline"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(
        seq(0, X_END_HRS - x_break_by, by = x_break_by),
        X_END_HRS - RES_SECONDS / 3600
      ),
      labels = clock_labels_7day,
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = c(0, X_END_HRS - RES_SECONDS / 3600),
      ylim = y_lim
    ) +
    labs(
      title = paste0("Participant ", SUBJECT_ID, ": Raw HR across study days 1–7"),
      subtitle = paste0("Resolution: ", RES_SECONDS, " sec"),
      x = NULL,
      y = "HR [bpm]",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# ----------------------------
# Main
# ----------------------------
main <- function() {
  log_msg("Script: 02_figure2_representative_trace.R")
  log_msg("Figure 2 subject plot start")
  log_msg("Subject: ", SUBJECT_ID)
  log_msg("Resolution: ", RES_SECONDS, " sec")
  log_msg("Input: ", in_path)
  log_msg("Output: ", out_dir)
  
  dt <- fread(in_path, showProgress = TRUE)
  
  need <- c("p_id", "time", "raw_hr", "bl_hr", "activity3", "day_num")
  miss <- setdiff(need, names(dt))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  
  dt[, p_id := as.character(p_id)]
  dt[, time := as.character(time)]
  dt[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
  
  n_bad_time <- dt[is.na(dt_time), .N]
  if (n_bad_time > 0) log_msg("Rows with unparseable time removed: ", n_bad_time)
  dt <- dt[!is.na(dt_time)]
  
  dt[, raw_hr := suppressWarnings(as.numeric(raw_hr))]
  dt[, baseline_hr := suppressWarnings(as.numeric(bl_hr))]
  dt[, activity3_norm := norm_chr(activity3)]
  dt[, day_num_int := parse_day_num(day_num)]
  
  bad_daynum <- dt[is.na(day_num_int), .N]
  if (bad_daynum > 0) log_msg("Rows with invalid day_num removed: ", bad_daynum)
  dt <- dt[!is.na(day_num_int)]
  
  lvl_tbl <- dt[, .N, by = .(activity3_norm)][order(-N)]
  fwrite(lvl_tbl, file.path(out_dir, "activity3_levels_detected.csv"))
  
  dt[, state := state_from_activity3(activity3_norm)]
  dt[!is.finite(raw_hr), baseline_hr := NA_real_]
  
  # keep only chosen subject
  dt <- dt[p_id == SUBJECT_ID]
  if (nrow(dt) == 0) stop("No rows found for subject: ", SUBJECT_ID)
  
  # collapse any duplicate timestamp rows conservatively
  dup_check <- dt[, .N, by = .(p_id, dt_time)][N > 1]
  fwrite(dup_check, file.path(out_dir, paste0("duplicate_pid_time_", SUBJECT_ID, ".csv")))
  log_msg("Duplicate (p_id, dt_time) rows for subject: ", nrow(dup_check))
  
  if (nrow(dup_check) > 0) {
    dt <- dt[, .(
      raw_hr = if (all(!is.finite(raw_hr))) NA_real_ else mean(raw_hr, na.rm = TRUE),
      baseline_hr = if (all(!is.finite(baseline_hr))) NA_real_ else mean(baseline_hr, na.rm = TRUE),
      state = factor(mode_chr(state), levels = levels(dt$state)),
      day_num_int = suppressWarnings(as.integer(mode_chr(day_num_int)))
    ), by = .(p_id, dt_time)]
  } else {
    dt <- dt[, .(p_id, dt_time, raw_hr, baseline_hr, state, day_num_int)]
  }
  
  # time within study-day slot
  dt[, sec_of_day := hour(dt_time) * 3600L + minute(dt_time) * 60L + second(dt_time)]
  
  # counts
  counts <- dt[, .N, by = .(p_id, state)]
  counts_w <- dcast(counts, p_id ~ state, value.var = "N", fill = 0)
  for (cc in c("DRIVING","SEDENTARY_NONDRIVING","PHYSICAL_ACTIVITY")) {
    if (!cc %in% names(counts_w)) counts_w[, (cc) := 0L]
  }
  counts_w[, TOTAL := DRIVING + SEDENTARY_NONDRIVING + PHYSICAL_ACTIVITY]
  fwrite(counts_w, file.path(out_dir, paste0("counts_", SUBJECT_ID, ".csv")))
  
  # subject summary
  subj_summary <- dt[, .(
    n_rows = .N,
    n_study_days_present = uniqueN(day_num_int),
    days_present = paste(sort(unique(day_num_int)), collapse = ","),
    time_min = min(dt_time, na.rm = TRUE),
    time_max = max(dt_time, na.rm = TRUE),
    n_raw_missing = sum(!is.finite(raw_hr)),
    n_bl_missing = sum(!is.finite(baseline_hr))
  ), by = p_id]
  fwrite(subj_summary, file.path(out_dir, paste0("subject_summary_", SUBJECT_ID, ".csv")))
  
  # baseline study-day diagnostics
  bl_diag <- dt[, .(
    n_rows_observed = .N,
    any_raw_hr = any(is.finite(raw_hr)),
    any_baseline = any(is.finite(baseline_hr)),
    n_nonmiss_baseline_rows = sum(is.finite(baseline_hr)),
    n_unique_baseline_values = uniqueN(baseline_hr[is.finite(baseline_hr)]),
    baseline_values_seen = paste(sort(unique(baseline_hr[is.finite(baseline_hr)])), collapse = ";")
  ), by = .(p_id, day_num_int)][order(day_num_int)]
  fwrite(bl_diag, file.path(out_dir, paste0("baseline_study_day_", SUBJECT_ID, ".csv")))
  
  log_msg("Rows kept for subject before fill: ", nrow(dt))
  
  # fill within slots 1..7
  dt_filled <- fill_missing_grid_one_subject(dt, step_secs = RES_SECONDS)
  setorder(dt_filled, p_id, day_num_int, sec_of_day)
  dt_filled[, x_hr := 24 * (day_num_int - 1L) + sec_of_day / 3600]
  
  fill_diag <- dt_filled[, .(
    n_total_points = .N,
    n_missing_raw = sum(!is.finite(raw_hr)),
    pct_missing_raw = round(100 * mean(!is.finite(raw_hr)), 2)
  ), by = p_id]
  fwrite(fill_diag, file.path(out_dir, paste0("missingness_after_fill_", SUBJECT_ID, ".csv")))
  
  log_msg("Rows after fill: ", nrow(dt_filled))
  
  p <- make_subject_plot(dt_filled)
  
  safe_save_pdf(p, pdf_path, w = PDF_W, h = PDF_H)
  ggsave(png_path, plot = p, width = PDF_W, height = PDF_H, dpi = PNG_DPI)
  
  log_msg("Saved: ", pdf_path)
  log_msg("Saved: ", png_path)
  log_msg("DONE. Outputs in: ", out_dir)
}

main()