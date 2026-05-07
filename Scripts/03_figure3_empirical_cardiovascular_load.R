# ============================================================
# 03_figure3_empirical_cardiovascular_load.R
#
# PURPOSE
#   Generate Figure 3 for the npj Digital Medicine manuscript:
#
#     From Instantaneous Heart Rate to Long-Horizon
#     Cardiovascular Burden in Naturalistic Daily Life
#
#   The script computes and visualizes baseline-referenced
#   cardiovascular load in driving and non-driving sedentary
#   contexts.
#
# ANALYTIC DEFINITION
#   Normalized heart rate (NHR) is defined as:
#
#     NHR = raw_hr - participant-day baseline HR
#
#   Baseline HR is treated as participant-day specific and
#   constant within study day. For each p_id x day_num, the
#   script constructs one daily baseline from bl_hr and uses
#   that value throughout the day.
#
# PANEL DEFINITIONS
#   A. Distribution of NHR
#   B. Percent time with NHR > 0
#   C. Within-subject variability of NHR
#
# INPUT
#   Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
#
# REQUIRED ACTIVITY LABELS
#   activity3 == "driving"
#   activity3 == "non_driving_sedentary"
#
# MAJOR OUTPUTS
#   The script writes Figure 3 and supporting summaries under:
#
#     Results/paper_figs/<timestamp>_60sec_figure3_empirical_cardiovascular_load/
#
#   Main outputs:
#
#     Figures/Figure3_CV_Load.pdf
#     Figures/Figure3_CV_Load.png
#     figure3_analysis_data.csv
#     figure3_subject_level_summary.csv
#     figure3_stratum_summary.csv
#     figure3_row_level_summary.csv
#     figure3_day_baseline_summary.csv
#     figure3_day_baseline_diagnostics.csv
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
  
  library(dplyr)
  library(tidyr)
  library(readr)
  
  library(ggplot2)
  library(scales)
  library(patchwork)
})

options(warn = 1)
set.seed(20260309)

# ----------------------------
# USER TOGGLES
# ----------------------------
LOCAL_TZ <- "America/Chicago"

USE_COMMON_SUBJECTS <- TRUE

ACT3_DRIVING <- "driving"
ACT3_ND_SED  <- "non_driving_sedentary"
ACT3_ND_PA   <- "non_driving_physical_activity"

# Build one constant baseline per p_id × day_num using median(bl_hr)
DAY_BASELINE_FUN <- "median"   # allowed: "median", "mean"

# Warn if bl_hr appears not constant within subject-day
DAY_BASELINE_SD_WARN <- 1.0    # bpm

# Density trimming for plotting only
DENSITY_TRIM_Q <- c(0.001, 0.999)

# Paper-consistent colors
COL_DRIVING    <- "#E69F00"   # orange
COL_NONDRIVING <- "#7F7F7F"   # gray
COL_ERRBAR     <- "#2F2F2F"   # dark neutral for visible skewers

# Slight transparency so skewers remain visible
BAR_ALPHA <- 0.80

PAL <- c(
  "driving" = COL_DRIVING,
  "non_driving_sedentary" = COL_NONDRIVING
)

PDF_W   <- 11.0
PDF_H   <- 4.4
PNG_DPI <- 300

# ----------------------------
# ROBUST wd = Scripts/
# ----------------------------
this_script <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
if (!is.na(this_script) && file.exists(this_script)) {
  setwd(dirname(this_script))
}
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

# ----------------------------
# OUTPUT FOLDER
# ----------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  project_root,
  "Results",
  "paper_figs",
  paste0(stamp, "_", RES_SECONDS, "sec_figure3_empirical_cardiovascular_load")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(out_dir, "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_dir, "run_log.txt")
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = log_file, append = TRUE)
}

safe_save_pdf <- function(plot_obj, path, w = 7, h = 5) {
  ok <- tryCatch({
    ggsave(
      filename = path, plot = plot_obj,
      width = w, height = h,
      device = grDevices::cairo_pdf
    )
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) {
    ggsave(
      filename = path, plot = plot_obj,
      width = w, height = h,
      device = "pdf", useDingbats = FALSE
    )
  }
}

safe_save_png <- function(plot_obj, path, w = 7, h = 5, dpi = 300) {
  ggsave(filename = path, plot = plot_obj, width = w, height = h, dpi = dpi)
}

log_msg("Script: 03_figure3_empirical_cardiovascular_load.R")
log_msg("Figure 3 empirical cardiovascular load analysis start")
log_msg("Resolution: ", RES_SECONDS, " sec")
log_msg("Input: ", in_path)
log_msg("Output: ", out_dir)
log_msg("USE_COMMON_SUBJECTS: ", USE_COMMON_SUBJECTS)
log_msg("DAY_BASELINE_FUN: ", DAY_BASELINE_FUN)

# ============================================================
# HELPERS
# ============================================================
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
  x2 <- trimws(x2)
  
  tt <- suppressWarnings(lubridate::ymd_hms(x2, tz = tz, quiet = TRUE))
  if (!all(is.na(tt))) return(tt)
  
  tt <- suppressWarnings(lubridate::ymd_hm(x2, tz = tz, quiet = TRUE))
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
    p_id = "p_id",
    pid = "p_id",
    participant_id = "p_id",
    participantid = "p_id",
    
    time = "time",
    timestamp = "time",
    datetime = "time",
    date_time = "time",
    
    raw_hr = "raw_hr",
    hr = "raw_hr",
    
    bl_hr = "bl_hr",
    hr_bl = "bl_hr",
    hrbl = "bl_hr",
    baseline_hr = "bl_hr",
    hr_baseline = "bl_hr",
    
    activity3 = "activity3",
    activity = "activity",
    data_source = "data_source",
    
    day_num = "day_num",
    daynum = "day_num",
    days = "days"
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

numify <- function(x) suppressWarnings(as.numeric(x))

make_day_key <- function(dt, tz = LOCAL_TZ) {
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
    d <- as.Date(dt[["dt_time"]], tz = tz)
    u <- sort(unique(d))
    return(as.integer(match(d, u)))
  }
  
  NULL
}

agg_baseline <- function(x, fun = "median") {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  if (fun == "median") return(stats::median(x, na.rm = TRUE))
  if (fun == "mean")   return(mean(x, na.rm = TRUE))
  stop("Unsupported DAY_BASELINE_FUN: ", fun)
}

q05 <- function(x) as.numeric(quantile(x, 0.05, na.rm = TRUE))
q50 <- function(x) as.numeric(quantile(x, 0.50, na.rm = TRUE))
q95 <- function(x) as.numeric(quantile(x, 0.95, na.rm = TRUE))

# ============================================================
# READ + STANDARDIZE
# ============================================================
log_msg("Reading data ...")
dt <- fread(in_path, showProgress = TRUE)
dt <- canonicalize_names(dt)

need <- c("p_id", "time", "raw_hr", "bl_hr")
miss <- setdiff(need, names(dt))
if (length(miss) > 0) {
  stop("Missing required columns after standardization: ", paste(miss, collapse = ", "))
}

if (!("activity3" %in% names(dt))) {
  stop("Required column `activity3` not found.")
}

dt[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
dt <- dt[!is.na(dt_time)]

dt[, raw_hr_num := numify(raw_hr)]
dt[, bl_hr_num  := numify(bl_hr)]
dt[, activity3  := trimws(tolower(as.character(activity3)))]

dt[, day_num_num := make_day_key(.SD, tz = LOCAL_TZ)]

if (all(is.na(dt$day_num_num))) {
  stop("Could not construct day key from day_num/days/time.")
}

log_msg("Rows after time parse: ", nrow(dt))
log_msg("Subjects after time parse: ", uniqueN(dt$p_id))

# ============================================================
# KEEP ONLY TARGET CONTEXTS
# ============================================================
dt <- dt[activity3 %in% c(ACT3_DRIVING, ACT3_ND_SED)]

if (nrow(dt) == 0) {
  stop("No rows remain after filtering to activity3 in {driving, non_driving_sedentary}.")
}

log_msg("Rows after activity3 filter: ", nrow(dt))
capture.output(
  print(dt[, .N, by = activity3][order(activity3)]),
  file = log_file, append = TRUE
)

# ============================================================
# COMPLETE CASES + PERSON-DAY BASELINE
# ============================================================
dd0 <- dt[
  is.finite(raw_hr_num) & is.finite(bl_hr_num) & !is.na(day_num_num),
  .(
    p_id,
    dt_time,
    raw_hr = raw_hr_num,
    bl_hr  = bl_hr_num,
    activity3,
    day_num = as.integer(day_num_num)
  )
]

if (nrow(dd0) == 0) {
  stop("No complete rows with finite raw_hr, bl_hr, and day_num.")
}

# Diagnostics: how constant is bl_hr within subject-day?
day_bl_diag <- dd0[
  ,
  .(
    n_rows = .N,
    bl_hr_day_median = median(bl_hr, na.rm = TRUE),
    bl_hr_day_mean   = mean(bl_hr, na.rm = TRUE),
    bl_hr_day_sd     = sd(bl_hr, na.rm = TRUE),
    bl_hr_day_min    = min(bl_hr, na.rm = TRUE),
    bl_hr_day_max    = max(bl_hr, na.rm = TRUE),
    bl_hr_day_range  = max(bl_hr, na.rm = TRUE) - min(bl_hr, na.rm = TRUE)
  ),
  by = .(p_id, day_num)
]

write_csv(as.data.frame(day_bl_diag), file.path(out_dir, "figure3_day_baseline_diagnostics.csv"))

n_warn <- day_bl_diag[is.finite(bl_hr_day_sd) & bl_hr_day_sd > DAY_BASELINE_SD_WARN, .N]
log_msg("Subject-day baseline diagnostic rows: ", nrow(day_bl_diag))
log_msg("Subject-days with bl_hr_day_sd > ", DAY_BASELINE_SD_WARN, " bpm: ", n_warn)

# Construct one constant baseline per subject-day
day_bl <- dd0[
  ,
  .(
    bl_hr_day = agg_baseline(bl_hr, fun = DAY_BASELINE_FUN),
    n_rows_day = .N
  ),
  by = .(p_id, day_num)
]

write_csv(as.data.frame(day_bl), file.path(out_dir, "figure3_day_baseline_summary.csv"))

dd <- merge(dd0, day_bl, by = c("p_id", "day_num"), all.x = TRUE)
dd <- dd[is.finite(bl_hr_day)]

if (nrow(dd) == 0) {
  stop("No rows remain after merging person-day baseline.")
}

# NHR here is the day-baseline-referenced quantity used for this descriptive figure
dd[, nhr := raw_hr - bl_hr_day]
dd[, nhr_gt_zero := nhr > 0]

# Optional common-subject restriction
if (USE_COMMON_SUBJECTS) {
  subj_drive <- unique(dd[activity3 == ACT3_DRIVING, p_id])
  subj_sed   <- unique(dd[activity3 == ACT3_ND_SED,  p_id])
  common_subj <- intersect(subj_drive, subj_sed)
  
  dd <- dd[p_id %in% common_subj]
  
  log_msg("Common-subject restriction applied.")
  log_msg("Common subjects: ", length(common_subj))
  
  if (length(common_subj) < 5) {
    stop("Too few common subjects after USE_COMMON_SUBJECTS restriction.")
  }
}

log_msg("Analysis rows: ", nrow(dd))
log_msg("Analysis subjects: ", uniqueN(dd$p_id))

fwrite(dd, file.path(out_dir, "figure3_analysis_data.csv"))

# ============================================================
# SUBJECT-LEVEL SUMMARIES
# ============================================================
subj_sum <- dd[
  ,
  .(
    n_rows = .N,
    pct_nhr_gt_zero = mean(nhr_gt_zero, na.rm = TRUE),
    sd_nhr = sd(nhr, na.rm = TRUE),
    mean_nhr = mean(nhr, na.rm = TRUE),
    median_nhr = median(nhr, na.rm = TRUE),
    q05_nhr = q05(nhr),
    q50_nhr = q50(nhr),
    q95_nhr = q95(nhr)
  ),
  by = .(p_id, activity3)
]

write_csv(as.data.frame(subj_sum), file.path(out_dir, "figure3_subject_level_summary.csv"))
log_msg("Wrote figure3_subject_level_summary.csv")

stratum_sum <- subj_sum %>%
  group_by(activity3) %>%
  summarise(
    n_subj = n_distinct(p_id),
    
    mean_pct_nhr_gt_zero = mean(pct_nhr_gt_zero, na.rm = TRUE),
    q05_pct_nhr_gt_zero  = as.numeric(quantile(pct_nhr_gt_zero, 0.05, na.rm = TRUE)),
    q50_pct_nhr_gt_zero  = as.numeric(quantile(pct_nhr_gt_zero, 0.50, na.rm = TRUE)),
    q95_pct_nhr_gt_zero  = as.numeric(quantile(pct_nhr_gt_zero, 0.95, na.rm = TRUE)),
    
    mean_sd_nhr = mean(sd_nhr, na.rm = TRUE),
    q05_sd_nhr  = as.numeric(quantile(sd_nhr, 0.05, na.rm = TRUE)),
    q50_sd_nhr  = as.numeric(quantile(sd_nhr, 0.50, na.rm = TRUE)),
    q95_sd_nhr  = as.numeric(quantile(sd_nhr, 0.95, na.rm = TRUE)),
    
    mean_mean_nhr = mean(mean_nhr, na.rm = TRUE),
    q05_mean_nhr  = as.numeric(quantile(mean_nhr, 0.05, na.rm = TRUE)),
    q50_mean_nhr  = as.numeric(quantile(mean_nhr, 0.50, na.rm = TRUE)),
    q95_mean_nhr  = as.numeric(quantile(mean_nhr, 0.95, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(stratum_sum, file.path(out_dir, "figure3_stratum_summary.csv"))
log_msg("Wrote figure3_stratum_summary.csv")

row_sum <- dd %>%
  group_by(activity3) %>%
  summarise(
    n_rows = n(),
    n_subj = n_distinct(p_id),
    mean_nhr = mean(nhr, na.rm = TRUE),
    sd_nhr_rows = sd(nhr, na.rm = TRUE),
    pct_nhr_gt_zero_rows = mean(nhr_gt_zero, na.rm = TRUE),
    q05_nhr = as.numeric(quantile(nhr, 0.05, na.rm = TRUE)),
    q50_nhr = as.numeric(quantile(nhr, 0.50, na.rm = TRUE)),
    q95_nhr = as.numeric(quantile(nhr, 0.95, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(row_sum, file.path(out_dir, "figure3_row_level_summary.csv"))

# ============================================================
# PLOTTING PREP
# ============================================================
xlims <- quantile(dd$nhr, probs = DENSITY_TRIM_Q, na.rm = TRUE)
dd_plot <- dd[nhr >= xlims[1] & nhr <= xlims[2]]

pretty_labels <- c(
  "driving" = "DRIVING",
  "non_driving_sedentary" = "NONDRIVING_SEDENTARY"
)

panelB_df <- stratum_sum %>%
  transmute(
    activity3,
    xpos = case_when(
      activity3 == ACT3_DRIVING ~ 1.0,
      activity3 == ACT3_ND_SED  ~ 3.0,
      TRUE ~ NA_real_
    ),
    y    = mean_pct_nhr_gt_zero,
    ymin = q05_pct_nhr_gt_zero,
    ymax = q95_pct_nhr_gt_zero
  )

panelC_df <- stratum_sum %>%
  transmute(
    activity3,
    xpos = case_when(
      activity3 == ACT3_DRIVING ~ 1.0,
      activity3 == ACT3_ND_SED  ~ 3.0,
      TRUE ~ NA_real_
    ),
    y    = mean_sd_nhr,
    ymin = q05_sd_nhr,
    ymax = q95_sd_nhr
  )

# ============================================================
# PANEL A
# ============================================================
pA <- ggplot(dd_plot, aes(x = nhr, fill = activity3, color = activity3)) +
  geom_density(alpha = 0.28, adjust = 1.0, linewidth = 1.0) +
  scale_fill_manual(values = PAL, labels = pretty_labels) +
  scale_color_manual(values = PAL, labels = pretty_labels) +
  labs(
    title = "A. Distribution of NHR",
    x = "NHR [bpm]",
    y = "PDF",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "plain")
  )

# ============================================================
# PANEL B
# ============================================================
pB <- ggplot(panelB_df, aes(x = xpos, y = y, fill = activity3)) +
  geom_col(
    width = 0.9,
    linewidth = 0.35,
    alpha = BAR_ALPHA,
    color = NA
  ) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.18,
    linewidth = 1.15,
    color = COL_ERRBAR
  ) +
  scale_fill_manual(values = PAL, labels = pretty_labels) +
  scale_x_continuous(
    breaks = c(1, 3),
    labels = c(pretty_labels[ACT3_DRIVING], pretty_labels[ACT3_ND_SED]),
    limits = c(0.3, 4.1)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.03),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "B. % time NHR > 0",
    x = NULL,
    y = "% time NHR > 0"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "plain"),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(5.5, 14, 5.5, 5.5)
  )

# ============================================================
# PANEL C
# ============================================================
pC <- ggplot(panelC_df, aes(x = xpos, y = y, fill = activity3)) +
  geom_col(
    width = 0.9,
    linewidth = 0.35,
    alpha = BAR_ALPHA,
    color = NA
  ) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.18,
    linewidth = 1.15,
    color = COL_ERRBAR
  ) +
  scale_fill_manual(values = PAL, labels = pretty_labels) +
  scale_x_continuous(
    breaks = c(1, 3),
    labels = c(pretty_labels[ACT3_DRIVING], pretty_labels[ACT3_ND_SED]),
    limits = c(0.3, 4.1)
  ) +
  labs(
    title = "C. Variability of NHR",
    x = NULL,
    y = "SD(NHR) [bpm]"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "plain"),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(5.5, 14, 5.5, 5.5)
  )

# ============================================================
# COMBINE FIGURE 3
# ============================================================
figure3 <- pA + pB + pC +
  plot_layout(ncol = 3, widths = c(1.25, 0.9, 0.9)) +
  plot_annotation(
    title = NULL
  )

pdf_path <- file.path(fig_dir, "Figure3_CV_Load.pdf")
png_path <- file.path(fig_dir, "Figure3_CV_Load.png")

safe_save_pdf(figure3, pdf_path, w = PDF_W, h = PDF_H)
safe_save_png(figure3, png_path, w = PDF_W, h = PDF_H, dpi = PNG_DPI)

log_msg("Saved: ", pdf_path)
log_msg("Saved: ", png_path)

# ============================================================
# LOG HEADLINE NUMBERS
# ============================================================
log_msg("Row-level summary:")
capture.output(print(as.data.frame(row_sum), row.names = FALSE), file = log_file, append = TRUE)

log_msg("Subject-level stratum summary:")
capture.output(print(as.data.frame(stratum_sum), row.names = FALSE), file = log_file, append = TRUE)

log_msg("DONE. Outputs in: ", out_dir)