# ============================================================
# 01_figure1_table1_exploratory_inventory.R
#
# PURPOSE
#   Generate Figure 1, Table 1, and the key-variable missingness
#   summary for the npj Digital Medicine manuscript:
#
#     From Instantaneous Heart Rate to Long-Horizon
#     Cardiovascular Burden in Naturalistic Daily Life
#
#   The script summarizes the curated NUBI analysis dataset,
#   including participant coverage, activity-context distribution,
#   participant-level traits, NASA-TLX subscales, baseline and
#   raw heart-rate summaries, temporal context, weather support,
#   and driving-dynamics variables.
#
# INPUT
#   Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
#
# REQUIRED ACTIVITY LABELS
#   activity3 == "driving"
#   activity3 == "non_driving_sedentary"
#   activity3 == "non_driving_physical_activity"
#
# MAJOR OUTPUTS
#   The script writes Figure 1, Table 1, a missingness summary,
#   and diagnostic files under:
#
#     Results/paper_figs/<timestamp>_60sec_figure1_table1_exploratory_inventory/
#
#   Main outputs:
#
#     Figures/Figure1-Table1.pdf
#     Figures/Figure1-Table1.png
#     Tables/Table1_CohortDataInventory.csv
#     Tables/Table1_CohortDataInventory.tex
#     Tables/Table1_Missingness_KeyVars.csv
#     Tables/Table1_Missingness_KeyVars.tex
#     Diagnostics/Diagnostic_DrivingOnlyLeakage_ByContext.csv
#
# REPOSITORY SCOPE
#   This public repository starts from the curated analysis-ready
#   60-second dataset. It does not reconstruct the dataset from
#   raw wearable, smartphone, vehicle, or ground-truth streams.
#
# PRIVACY NOTE
#   Direct GPS coordinate columns are removed from the public
#   dataset and are not required for this figure or table.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(scales)
  library(patchwork)
  library(ggtext)
})

options(warn = 1)
set.seed(20260225)

# ----------------------------
# USER TOGGLES
# ----------------------------
LOCAL_TZ <- "America/Chicago"
SHOW_BOX_IN_VIOLIN <- TRUE
BASE_FONT <- 13

SHOW_PID_LABELS   <- FALSE
PID_LABEL_SIZE    <- 7
LABEL_EVERY_K_PID <- 1L

# Final validated activity3 labels
ACT3_DRIVING <- "driving"
ACT3_ND_SED  <- "non_driving_sedentary"
ACT3_ND_PA   <- "non_driving_physical_activity"

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

# ----------------------------
# Output folders
# ----------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  project_root,
  "Results",
  "paper_figs",
  paste0(stamp, "_", RES_SECONDS, "sec_figure1_table1_exploratory_inventory")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fig_dir <- file.path(out_dir, "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

table_dir <- file.path(out_dir, "Tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

diag_dir <- file.path(out_dir, "Diagnostics")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_dir, "run_log.txt")
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = log_file, append = TRUE)
}

# ----------------------------
# Input file mapping
# ----------------------------
in_path <- switch(
  as.character(RES_SECONDS),
  "10" = file.path(project_root, "Data", "NUBI_Data_10sec_Level_MASTER_CLEAN.csv"),
  "30" = file.path(project_root, "Data", "NUBI_Data_30sec_Level_MASTER_CLEAN.csv"),
  "60" = file.path(project_root, "Data", "NUBI_Data_60sec_Level_MASTER_CLEAN.csv")
)
if (!file.exists(in_path)) {
  stop(
    "Dataset not found: ", in_path, "\n",
    "The public repository currently includes only the 60-second dataset. ",
    "Use 60 sec, or add the corresponding ", RES_SECONDS,
    "-second dataset under Data/."
  )
}

log_msg("Script: 01_figure1_table1_exploratory_inventory.R")
log_msg("Resolution: ", RES_SECONDS, " sec")
log_msg("Input: ", in_path)
log_msg("Output directory: ", out_dir)

# ============================================================
# Helpers
# ============================================================
norm_ds <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub("\\s+", "_", x)
  x <- gsub("-", "_", x)
  x
}

parse_time_local <- function(x, tz = LOCAL_TZ) {
  if (inherits(x, "POSIXt")) return(force_tz(x, tzone = tz))
  if (!is.character(x)) x <- as.character(x)
  x2 <- trimws(x)
  x2 <- gsub("Z$", "", x2, ignore.case = TRUE)
  
  tt <- suppressWarnings(lubridate::ymd_hms(x2, tz = tz, quiet = TRUE))
  if (!all(is.na(tt))) return(tt)
  
  suppressWarnings(lubridate::parse_date_time(
    x2,
    orders = c("ymd HMS","ymd HM","ymdT HMS","ymdT HM",
               "mdy HMS","mdy HM","dmy HMS","dmy HM",
               "Ymd HMS","Ymd HM","YmdT HMS","YmdT HM"),
    tz = tz
  ))
}

make_day_key <- function(x) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d+")))
}

safe_save_pdf <- function(plot_obj, path, w = 16, h = 12) {
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

safe_save_png <- function(plot_obj, path, w = 16, h = 12, dpi = 300) {
  ggsave(filename = path, plot = plot_obj, width = w, height = h, dpi = dpi)
}

violin_with_box <- function(data, xcol, ycol, title, ylab = NULL) {
  p <- ggplot(data, aes(x = .data[[xcol]], y = .data[[ycol]])) +
    geom_violin(trim = TRUE) +
    coord_flip() +
    labs(title = title, x = NULL, y = ylab) +
    theme_minimal(base_size = BASE_FONT)
  if (SHOW_BOX_IN_VIOLIN) p <- p + geom_boxplot(width = 0.15, outlier.size = 0.6)
  p
}

fmt_int <- function(x) format(as.integer(round(x)), big.mark = ",",
                              scientific = FALSE, trim = TRUE)
fmt_num <- function(x, d = 2) format(round(as.numeric(x), d), big.mark = ",",
                                     scientific = FALSE, trim = TRUE, nsmall = d)
fmt_pct <- function(x, d = 1) paste0(fmt_num(100 * x, d), "\\%")

is_unknown_id <- function(x) {
  x <- trimws(tolower(as.character(x)))
  is.na(x) | x %in% c("", "na", "n/a", "null", "unknown")
}
is_no_trip <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x %in% c("no_trip","no trip","notrip")
}
is_real_trip_id <- function(x) {
  !(is_unknown_id(x) | is_no_trip(x))
}

# ============================================================
# Read + harmonize key columns
# ============================================================
log_msg("Reading dataset...")
dt <- fread(in_path, showProgress = TRUE)
log_msg("Rows read: ", format(nrow(dt), big.mark = ","))
log_msg("Columns read: ", ncol(dt))

need <- c("p_id","time","raw_hr","activity3","bl_hr",
          "md","pd","td","p","e","f",
          "openness","conscientiousness","extraversion","agreeableness","neuroticism",
          "day_num","day_period")
miss <- setdiff(need, names(dt))
if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

dt[, time := as.character(time)]
dt[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
n_bad_time <- dt[is.na(dt_time), .N]
if (n_bad_time > 0) log_msg("Rows with unparseable time removed: ", n_bad_time)
dt <- dt[!is.na(dt_time)]

dt[, activity3_norm := norm_ds(activity3)]
log_msg("activity3_norm levels: ", paste(sort(unique(dt$activity3_norm)), collapse = ", "))

# ============================================================
# Context mapping from final activity3
# ============================================================
dt[, strata3 := fifelse(
  activity3_norm == ACT3_DRIVING, "DRIVING",
  fifelse(activity3_norm == ACT3_ND_SED, "NONDRIVING_SEDENTARY",
          fifelse(activity3_norm == ACT3_ND_PA, "PHYSICAL_ACTIVITY", NA_character_))
)]
if (anyNA(dt$strata3)) {
  bad <- sort(unique(dt[is.na(strata3), activity3_norm]))
  stop("Unrecognized activity3_norm level(s): ", paste(bad, collapse = ", "))
}
dt[, strata3 := factor(strata3, levels = c("DRIVING","NONDRIVING_SEDENTARY","PHYSICAL_ACTIVITY"))]

# ============================================================
# Study day key + participant baseline summary
# ============================================================
dt[, bl_hr_num := suppressWarnings(as.numeric(bl_hr))]
dt[, day_key := make_day_key(day_num)]
if (all(is.na(dt$day_key))) stop("Could not construct day_key from day_num.")

baseline_by_subj_day <- dt[
  is.finite(bl_hr_num) & !is.na(day_key),
  .(bl_hr_day = unique(bl_hr_num)[1]),
  by = .(p_id, day_key)
]

baseline_by_subj <- baseline_by_subj_day[
  , .(bl_hr_person = mean(bl_hr_day, na.rm = TRUE)),
  by = .(p_id)
]

dt <- merge(dt, baseline_by_subj, by = "p_id", all.x = TRUE)

# ============================================================
# (0) Diagnostic: driving-only leakage across contexts
# ============================================================
drive_only_vars <- intersect(
  c("speed","ff","ff_speed","atp","rtp","jf","energy_acc","energy_rot",
    "distance","trip_distance","trip_duration","trip_id"),
  names(dt)
)

diag_list <- list()

for (v in drive_only_vars) {
  
  if (v == "trip_id") {
    
    tmp <- dt[, {
      n_rows <- .N
      n_real <- sum(is_real_trip_id(trip_id), na.rm = TRUE)
      
      current_ctx <- unique(as.character(strata3))
      if (length(current_ctx) != 1L) current_ctx <- current_ctx[1]
      
      n_app <- if (identical(current_ctx, "DRIVING")) n_rows else 0L
      n_skip <- n_rows - n_app
      n_real_drv <- if (identical(current_ctx, "DRIVING")) n_real else 0L
      
      .(
        n_rows = n_rows,
        n_applicable = n_app,
        n_skip_not_applicable = n_skip,
        n_real_tripid = n_real,
        n_real_tripid_driving = n_real_drv,
        pct_real_tripid_of_all = 100 * n_real / n_rows,
        pct_real_tripid_of_applicable = ifelse(n_app > 0, 100 * n_real_drv / n_app, NA_real_)
      )
    }, by = strata3]
    
  } else {
    
    tmp <- dt[, {
      n_rows <- .N
      nn <- sum(is.finite(suppressWarnings(as.numeric(get(v)))))
      .(
        n_rows = n_rows,
        n_nonmiss = nn,
        pct_nonmiss = 100 * nn / n_rows
      )
    }, by = strata3]
  }
  
  tmp[, variable := v]
  diag_list[[v]] <- tmp
}

diag_dt <- rbindlist(diag_list, use.names = TRUE, fill = TRUE)
setcolorder(diag_dt, c("variable","strata3", setdiff(names(diag_dt), c("variable","strata3"))))
out_diag_csv <- file.path(diag_dir, "Diagnostic_DrivingOnlyLeakage_ByContext.csv")
fwrite(diag_dt, out_diag_csv)

log_msg("Wrote diagnostic CSV: ", out_diag_csv)
print(diag_dt)

# ============================================================
# (1) Context-consistent masking for numeric driving-only vars
# ============================================================
dt[, trip_id_driving_only := if ("trip_id" %in% names(dt)) fifelse(strata3 == "DRIVING", as.character(trip_id), NA_character_) else NA_character_]

drive_only_numeric <- intersect(
  c("speed","ff","ff_speed","atp","rtp","jf","energy_acc","energy_rot",
    "distance","trip_distance","trip_duration"),
  names(dt)
)
if (length(drive_only_numeric) > 0) {
  dt[strata3 != "DRIVING", (drive_only_numeric) := NA]
}
log_msg("Masked numeric driving-only variables outside DRIVING.")

# ============================================================
# Prepare DRIVING-only view and trip-level collapse
# ============================================================
dt[, date_local := as.Date(dt_time, tz = LOCAL_TZ)]
dt_drive <- dt[strata3 == "DRIVING"]

trip_col_candidates <- c("trip_id","trip_id_driving_only","trips","tripid","trip",
                         "trip_num","trip_index","trip_uid","trip_uuid")
trip_col_candidates <- trip_col_candidates[trip_col_candidates %in% names(dt_drive)]
trip_col <- if (length(trip_col_candidates) > 0) trip_col_candidates[1] else NULL

trip_level_dt <- NULL
driving_trips_n <- NA_integer_
driving_trip_median_min <- NA_real_

if (nrow(dt_drive) == 0) {
  
  trip_level_dt <- data.table()
  driving_trips_n <- 0L
  driving_trip_median_min <- NA_real_
  
} else if (!is.null(trip_col)) {
  
  tmp <- copy(dt_drive)
  tmp[, trip_id_any := as.character(get(trip_col))]
  tmp[!is_real_trip_id(trip_id_any), trip_id_any := NA_character_]
  tmp <- tmp[!is.na(trip_id_any) & !is.na(day_key)]
  setorder(tmp, p_id, day_key, trip_id_any, dt_time)
  
  if (nrow(tmp) > 0) {
    trip_level_dt <- tmp[, .(
      start = min(dt_time),
      end   = max(dt_time),
      rows  = .N,
      trip_distance_last = if ("trip_distance" %in% names(tmp)) suppressWarnings(as.numeric(trip_distance[.N])) else NA_real_,
      trip_duration_first = if ("trip_duration" %in% names(tmp)) suppressWarnings(as.numeric(trip_duration[1])) else NA_real_
    ), by = .(p_id, day_key, trip_id_any)]
    
    trip_level_dt[, dur_sec := as.numeric(difftime(end, start, units = "secs")) + RES_SECONDS]
    trip_level_dt <- trip_level_dt[is.finite(dur_sec) & dur_sec > 0]
    
    driving_trips_n <- nrow(trip_level_dt)
    driving_trip_median_min <- if (driving_trips_n > 0) median(trip_level_dt$dur_sec, na.rm = TRUE) / 60 else NA_real_
  } else {
    trip_level_dt <- data.table()
    driving_trips_n <- 0L
    driving_trip_median_min <- NA_real_
  }
  
} else {
  
  tmp <- copy(dt_drive)
  setorder(tmp, p_id, dt_time)
  tmp[, dt_gap_sec := as.numeric(difftime(dt_time, shift(dt_time), units = "secs")), by = p_id]
  gap_thr <- max(5 * RES_SECONDS, 300)
  tmp[, new_trip := fifelse(is.na(dt_gap_sec) | dt_gap_sec > gap_thr, 1L, 0L), by = p_id]
  tmp[, trip_run := cumsum(new_trip), by = p_id]
  setorder(tmp, p_id, trip_run, dt_time)
  
  trip_level_dt <- tmp[, .(
    start = min(dt_time),
    end   = max(dt_time),
    rows  = .N,
    trip_distance_last = if ("trip_distance" %in% names(tmp)) suppressWarnings(as.numeric(trip_distance[.N])) else NA_real_,
    trip_duration_first = if ("trip_duration" %in% names(tmp)) suppressWarnings(as.numeric(trip_duration[1])) else NA_real_
  ), by = .(p_id, trip_run)]
  
  trip_level_dt[, dur_sec := as.numeric(difftime(end, start, units = "secs")) + RES_SECONDS]
  trip_level_dt <- trip_level_dt[is.finite(dur_sec) & dur_sec > 0]
  
  driving_trips_n <- nrow(trip_level_dt)
  driving_trip_median_min <- if (driving_trips_n > 0) median(trip_level_dt$dur_sec, na.rm = TRUE) / 60 else NA_real_
}

# ============================================================
# Table 1: Cohort & data inventory
# ============================================================
overall_tbl <- data.table(
  group = "OVERALL",
  participants = uniqueN(dt$p_id),
  participant_days = uniqueN(dt[, .(p_id, day_key)]),
  calendar_days = uniqueN(dt$date_local),
  rows = nrow(dt),
  hours = nrow(dt) * RES_SECONDS / 3600,
  pct_rows = 1
)

ctx_tbl <- dt[, .(
  participants = uniqueN(p_id),
  participant_days = uniqueN(data.table(p_id, day_key)),
  calendar_days = uniqueN(date_local),
  rows = .N
), by = .(strata3)]

ctx_tbl[, hours := rows * RES_SECONDS / 3600]
ctx_tbl[, pct_rows := rows / nrow(dt)]
ctx_tbl[, group := as.character(strata3)]
ctx_tbl <- ctx_tbl[, .(group, participants, participant_days, calendar_days, rows, hours, pct_rows)]

table1_rows <- rbind(overall_tbl, ctx_tbl, fill = TRUE)
table1_rows[, driving_trips := NA_integer_]
table1_rows[, median_trip_duration_min := NA_real_]
table1_rows[group %in% c("OVERALL","DRIVING"), `:=`(
  driving_trips = driving_trips_n,
  median_trip_duration_min = driving_trip_median_min
)]

table1_rows[, group := factor(group, levels = c("OVERALL","DRIVING","NONDRIVING_SEDENTARY","PHYSICAL_ACTIVITY"))]
setorder(table1_rows, group)

out_table1_csv <- file.path(table_dir, "Table1_CohortDataInventory.csv")
fwrite(table1_rows, out_table1_csv)

out_table1_tex <- file.path(table_dir, "Table1_CohortDataInventory.tex")
t1 <- copy(table1_rows)
t1[, hours_fmt := fmt_num(hours, 1)]
t1[, rows_fmt  := fmt_int(rows)]
t1[, pct_fmt   := fmt_pct(pct_rows, 1)]
t1[, participants_fmt := fmt_int(participants)]
t1[, participant_days_fmt := fmt_int(participant_days)]
t1[, calendar_days_fmt := fmt_int(calendar_days)]
setorder(t1, group)

latex_table_lines <- c(
  "% ============================================================",
  "% Table 1 --- Cohort & data inventory (auto-generated)",
  "% ============================================================",
  "",
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{\\textbf{Table 1 --- Cohort \\& data inventory.} Overall and by context at the chosen sampling resolution. Context-specific support counts are computed within each context.}",
  "\\label{tab:cohort_inventory}",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Group & Participants & Part.-days & Cal.-days & Rows & Hours & \\% rows \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(t1))) {
  latex_table_lines <- c(
    latex_table_lines,
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
            as.character(t1$group[i]),
            t1$participants_fmt[i],
            t1$participant_days_fmt[i],
            t1$calendar_days_fmt[i],
            t1$rows_fmt[i],
            t1$hours_fmt[i],
            t1$pct_fmt[i])
  )
}
latex_table_lines <- c(
  latex_table_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  sprintf("%% DRIVING trips counted from collapsed real trips only. Trips: %s",
          ifelse(is.na(driving_trips_n), "NA", fmt_int(driving_trips_n))),
  sprintf("%% Median trip duration (min): %s",
          ifelse(is.na(driving_trip_median_min), "NA", fmt_num(driving_trip_median_min, 1))),
  "\\end{table}",
  ""
)
writeLines(latex_table_lines, con = out_table1_tex)

log_msg("Wrote Table 1 CSV: ", out_table1_csv)
log_msg("Wrote Table 1 LaTeX: ", out_table1_tex)

# ============================================================
# Missingness: CSV + LaTeX
# ============================================================
miss_rows <- list()

tmp <- dt[, .(
  unit = "row",
  n_units = .N,
  miss_pct = 100 * mean(!(is.finite(suppressWarnings(as.numeric(raw_hr)))))
), by = .(strata3)]
tmp[, variable := "raw_hr"]
miss_rows[["raw_hr"]] <- tmp

bl_day <- dt[, .(
  has_bl = any(is.finite(suppressWarnings(as.numeric(bl_hr))))
), by = .(p_id, day_key)]

overall <- data.table(
  variable = "bl_hr",
  strata3 = "OVERALL",
  unit = "participant-day",
  n_units = nrow(bl_day),
  miss_pct = 100 * (1 - mean(bl_day$has_bl))
)

byctx <- dt[, .(
  has_bl = any(is.finite(suppressWarnings(as.numeric(bl_hr))))
), by = .(p_id, day_key, strata3)]
byctx_sum <- byctx[, .(
  variable = "bl_hr",
  unit = "participant-day",
  n_units = .N,
  miss_pct = 100 * (1 - mean(has_bl))
), by = .(strata3)]
miss_rows[["bl_hr"]] <- rbind(overall, byctx_sum, fill = TRUE)

nasa_vars <- intersect(c("md","pd","td","p","e","f"), names(dt))
if (length(nasa_vars) > 0 && "day_period" %in% names(dt)) {
  
  dt[, day_period_clean := trimws(as.character(day_period))]
  dt[tolower(day_period_clean) %in% c("", "na", "n/a", "null", "unknown"),
     day_period_clean := NA_character_]
  
  eligible <- dt[!is.na(day_period_clean),
                 .(drove = any(strata3 == "DRIVING")),
                 by = .(p_id, day_key, day_period_clean)]
  eligible <- eligible[drove == TRUE]
  setkey(eligible, p_id, day_key, day_period_clean)
  
  if (nrow(eligible) > 0) {
    for (v in nasa_vars) {
      tmpv <- dt[!is.na(day_period_clean), .(
        has = any(is.finite(suppressWarnings(as.numeric(get(v)))))
      ), by = .(p_id, day_key, day_period_clean)]
      setkey(tmpv, p_id, day_key, day_period_clean)
      
      tmp2 <- tmpv[eligible, nomatch = 0]
      
      miss_rows[[paste0("nasa_",v)]] <- tmp2[, .(
        variable = v,
        strata3  = "OVERALL",
        unit     = "participant-day-period (DRIVING-present)",
        n_units  = .N,
        miss_pct = 100 * (1 - mean(has))
      )]
    }
  } else {
    for (v in nasa_vars) {
      miss_rows[[paste0("nasa_",v)]] <- data.table(
        variable = v,
        strata3 = "OVERALL",
        unit = "participant-day-period (DRIVING-present)",
        n_units = 0,
        miss_pct = NA_real_
      )
    }
  }
}

drive_vars_for_miss <- intersect(
  c("speed","ff","ff_speed","atp","rtp","jf","energy_acc","energy_rot","distance"),
  names(dt)
)
if (length(drive_vars_for_miss) > 0) {
  for (v in drive_vars_for_miss) {
    tmpv <- dt[strata3 == "DRIVING", .(
      unit = "row (DRIVING only)",
      n_units = .N,
      miss_pct = 100 * mean(!(is.finite(suppressWarnings(as.numeric(get(v))))))
    )]
    tmpv[, `:=`(variable = v, strata3 = "DRIVING")]
    miss_rows[[paste0("drive_",v)]] <- tmpv
  }
}

# Trip-level missingness (collapsed unique trips)
if (!is.null(trip_level_dt) && nrow(trip_level_dt) > 0) {
  
  miss_rows[["trip_id"]] <- data.table(
    variable = "trip_id",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = nrow(trip_level_dt),
    miss_pct = 0
  )
  
  miss_rows[["trip_distance"]] <- data.table(
    variable = "trip_distance",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = nrow(trip_level_dt),
    miss_pct = 100 * mean(!is.finite(trip_level_dt$trip_distance_last))
  )
  
  miss_rows[["trip_duration"]] <- data.table(
    variable = "trip_duration",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = nrow(trip_level_dt),
    miss_pct = 100 * mean(!is.finite(trip_level_dt$trip_duration_first))
  )
  
} else {
  
  miss_rows[["trip_id"]] <- data.table(
    variable = "trip_id",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = 0,
    miss_pct = NA_real_
  )
  
  miss_rows[["trip_distance"]] <- data.table(
    variable = "trip_distance",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = 0,
    miss_pct = NA_real_
  )
  
  miss_rows[["trip_duration"]] <- data.table(
    variable = "trip_duration",
    strata3 = "DRIVING",
    unit = "trip",
    n_units = 0,
    miss_pct = NA_real_
  )
}

miss_tbl <- rbindlist(miss_rows, use.names = TRUE, fill = TRUE)
miss_tbl[, strata3 := factor(as.character(strata3),
                             levels = c("OVERALL","DRIVING","NONDRIVING_SEDENTARY","PHYSICAL_ACTIVITY"))]
miss_tbl[, variable := as.character(variable)]
setorder(miss_tbl, variable, strata3)

out_miss_csv <- file.path(table_dir, "Table1_Missingness_KeyVars.csv")
fwrite(miss_tbl, out_miss_csv)

out_miss_tex <- file.path(table_dir, "Table1_Missingness_KeyVars.tex")
mw <- copy(miss_tbl)
mw[, miss_pct_fmt := sprintf("%.1f\\%%", miss_pct)]
mw[is.na(miss_pct), miss_pct_fmt := ""]
mw[, n_units_fmt := ifelse(is.na(n_units), "", format(as.integer(n_units), big.mark = ","))]
mw[, strata3_chr := as.character(strata3)]
mw[is.na(strata3_chr), strata3_chr := ""]

latex_miss <- c(
  "% ============================================================",
  "% Table 1 (companion) --- Missingness summary (granularity-aware)",
  "% NOTE: NASA-TLX missingness computed only for DRIVING-present periods.",
  "% ============================================================",
  "",
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{\\textbf{Table 1 (companion) --- Missingness summary.} Missingness is computed at the natural granularity of each variable. Driving-dynamics variables are evaluated on DRIVING rows only, whereas trip-level variables are evaluated on collapsed DRIVING trips. NASA-TLX missingness is computed only for participant-day-period units where DRIVING occurred.}",
  "\\label{tab:missingness_keyvars}",
  "\\begin{tabular}{lllr r}",
  "\\toprule",
  "Variable & Context & Unit & $n$ & Missing (\\%) \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(mw))) {
  latex_miss <- c(
    latex_miss,
    sprintf("%s & %s & %s & %s & %s \\\\",
            mw$variable[i],
            mw$strata3_chr[i],
            mw$unit[i],
            mw$n_units_fmt[i],
            mw$miss_pct_fmt[i])
  )
}
latex_miss <- c(
  latex_miss,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  ""
)
writeLines(latex_miss, con = out_miss_tex)

log_msg("Wrote missingness CSV: ", out_miss_csv)
log_msg("Wrote missingness LaTeX: ", out_miss_tex)

# ============================================================
# Figure 1 panels
# ============================================================
big5_cols <- c("openness","conscientiousness","extraversion","agreeableness","neuroticism")
nasa_cols <- c("md","pd","td","p","e","f")

nasa_pretty <- c(
  md = "Mental Demand",
  pd = "Physical Demand",
  td = "Temporal Demand",
  p  = "Performance",
  e  = "Effort",
  f  = "Frustration"
)

person_df <- dt[, c(
  list(
    raw_hr_med   = median(suppressWarnings(as.numeric(raw_hr)), na.rm = TRUE),
    bl_hr_person = median(suppressWarnings(as.numeric(bl_hr_person)), na.rm = TRUE)
  ),
  lapply(.SD, function(x) median(suppressWarnings(as.numeric(x)), na.rm = TRUE))
), by = .(p_id), .SDcols = c(big5_cols, nasa_cols)]

big5_long <- person_df %>%
  select(p_id, all_of(big5_cols)) %>%
  pivot_longer(cols = all_of(big5_cols), names_to = "trait", values_to = "score") %>%
  mutate(trait = recode(trait,
                        openness = "Openness",
                        conscientiousness = "Conscientiousness",
                        extraversion = "Extraversion",
                        agreeableness = "Agreeableness",
                        neuroticism = "Neuroticism")) %>%
  filter(is.finite(score))
pA <- violin_with_box(big5_long, "trait", "score",
                      "Big Five trait distributions (participant-level)",
                      ylab = "score")

nasa_long <- person_df %>%
  select(p_id, all_of(nasa_cols)) %>%
  pivot_longer(cols = all_of(nasa_cols), names_to = "subscale", values_to = "score") %>%
  filter(is.finite(score)) %>%
  mutate(subscale = recode(subscale, !!!nasa_pretty, .default = subscale))
pB <- violin_with_box(nasa_long, "subscale", "score",
                      "NASA-TLX subscales (participant-level)",
                      ylab = "score") +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7)

phys_long <- person_df %>%
  select(p_id, raw_hr_med, bl_hr_person) %>%
  pivot_longer(cols = c(raw_hr_med, bl_hr_person),
               names_to = "var", values_to = "value") %>%
  filter(is.finite(value))

phys_long$var <- factor(
  phys_long$var,
  levels = c("raw_hr_med", "bl_hr_person"),
  labels = c("raw_hr_med", "bl_hr_person")
)

pD <- violin_with_box(
  phys_long, "var", "value",
  "Physiology overview (participant-level distributions)",
  ylab = "bpm"
) +
  scale_x_discrete(labels = c(
    raw_hr_med   = expression(widetilde(italic(HR))[plain(raw) * "," * i]),
    bl_hr_person = expression(italic(HR)[plain(base) * "," * i])
  ))

mix_n <- dt[, .(n_rows = .N), by = .(p_id, strata3)]
drive_n <- mix_n[strata3 == "DRIVING", .(drive_n = sum(n_rows)), by = p_id]
tot_n   <- mix_n[, .(tot_n = sum(n_rows)), by = p_id]
drive_n <- merge(tot_n, drive_n, by = "p_id", all.x = TRUE)
drive_n[is.na(drive_n), drive_n := 0]
drive_n[, drive_pct := drive_n / tot_n]

pid_levels <- drive_n[order(-drive_pct), as.character(p_id)]
mix_n[, p_id := factor(as.character(p_id), levels = pid_levels)]
mix_n[, pct := n_rows / sum(n_rows), by = p_id]
mix_df <- as.data.frame(mix_n)

pC <- ggplot(mix_df, aes(y = p_id, x = pct, fill = strata3)) +
  geom_col(
    position = "stack",
    width = 0.90,
    colour = "white",
    linewidth = 0.08
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "DRIVING"              = "orange",
    "NONDRIVING_SEDENTARY" = "grey85",
    "PHYSICAL_ACTIVITY"    = "springgreen3"
  )) +
  labs(title = "Exposure mix by participant", x = "% of rows", y = NULL, fill = "Context") +
  theme_minimal(base_size = BASE_FONT) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

if (!SHOW_PID_LABELS) {
  pC <- pC + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
} else {
  lab_vec <- levels(mix_df$p_id)
  if (LABEL_EVERY_K_PID > 1L) {
    keep_idx <- seq(1, length(lab_vec), by = LABEL_EVERY_K_PID)
    lab_vec[-keep_idx] <- ""
  }
  pC <- pC +
    scale_y_discrete(labels = lab_vec, guide = guide_axis(n.dodge = 2)) +
    theme(axis.text.y = element_text(size = PID_LABEL_SIZE))
}

if ("day_period" %in% names(dt)) {
  dt[, day_period_clean2 := trimws(as.character(day_period))]
  dt[tolower(day_period_clean2) %in% c("", "na", "n/a", "null", "unknown"),
     day_period_clean2 := NA_character_]
  dt[!is.na(day_period_clean2), day_period_clean2 := stringr::str_to_title(day_period_clean2)]
  
  dt[day_period_clean2 == "Morning",   day_period_clean2 := "Early-Day"]
  dt[day_period_clean2 == "Afternoon", day_period_clean2 := "Late-Day"]
  
  pp <- dt[!is.na(day_period_clean2), .N, by = .(day_period_clean2)][order(-N)] |> as.data.frame()
  pp$day_period_clean2 <- factor(pp$day_period_clean2, levels = pp$day_period_clean2[order(pp$N)])
  
  pE <- ggplot(pp, aes(x = day_period_clean2, y = N)) +
    geom_col() + coord_flip() +
    labs(title = "Period of day", x = NULL, y = "count") +
    theme_minimal(base_size = BASE_FONT)
} else {
  pE <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) + labs(title = "Period of day")
}

if ("weather_info" %in% names(dt)) {
  
  weather_dt <- copy(dt[strata3 == "DRIVING"])
  
  weather_dt[, weather_clean := trimws(as.character(weather_info))]
  weather_dt[tolower(weather_clean) %in% c("", "na", "n/a", "null", "unknown"),
             weather_clean := NA_character_]
  weather_dt[!is.na(weather_clean), weather_clean := tolower(weather_clean)]
  
  # Driving-specific relabeling:
  # "other" is the catch-all adverse-weather category in DRIVING.
  weather_dt[weather_clean == "other",   weather_clean := "adverse_weather"]
  
  # Optional but recommended: collapse extremely rare drizzle into adverse weather
  # so panel F does not show a meaningless singleton bar.
  weather_dt[weather_clean == "drizzle", weather_clean := "adverse_weather"]
  
  weather_dt[!is.na(weather_clean), weather_clean := recode(
    weather_clean,
    clear            = "Clear",
    clouds           = "Clouds",
    rain             = "Rain",
    adverse_weather  = "Adverse weather",
    .default         = stringr::str_to_title(weather_clean)
  )]
  
  ww <- weather_dt[!is.na(weather_clean), .N, by = .(weather_clean)][order(-N)] |> as.data.frame()
  
  if (nrow(ww) > 0) {
    ww$weather_clean <- factor(
      ww$weather_clean,
      levels = c("Adverse weather", "Rain", "Clouds", "Clear")
    )
    ww$weather_clean <- droplevels(ww$weather_clean)
    
    pF <- ggplot(ww, aes(x = weather_clean, y = N)) +
      geom_col() +
      coord_flip() +
      labs(title = "Driving weather conditions", x = NULL, y = "count") +
      theme_minimal(base_size = BASE_FONT)
  } else {
    pF <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) +
      labs(title = "Driving weather conditions")
  }
  
} else {
  pF <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) +
    labs(title = "Driving weather conditions")
}

dt_drive2 <- dt[strata3 == "DRIVING"]

dyn_pick_main <- intersect(c("atp","ff","speed"), names(dt_drive2))
dyn_pick_main <- dyn_pick_main[vapply(dyn_pick_main, function(nm) {
  x <- suppressWarnings(as.numeric(dt_drive2[[nm]]))
  any(is.finite(x))
}, logical(1))]
dyn_pick_main <- dyn_pick_main[match(c("atp","ff","speed"), dyn_pick_main)]
dyn_pick_main <- dyn_pick_main[!is.na(dyn_pick_main)]

if (nrow(dt_drive2) == 0 || length(dyn_pick_main) == 0) {
  pG_main <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) +
    labs(title = "Driving dynamics")
} else {
  dyn_long <- dt_drive2[, c(dyn_pick_main), with = FALSE] |> as.data.frame()
  dyn_long <- dyn_long %>%
    pivot_longer(cols = all_of(dyn_pick_main), names_to = "var", values_to = "value") %>%
    mutate(value = suppressWarnings(as.numeric(value))) %>%
    filter(is.finite(value))
  
  clip_tbl <- dyn_long %>%
    group_by(var) %>%
    summarise(
      lo = as.numeric(quantile(value, 0.01, na.rm = TRUE)),
      hi = as.numeric(quantile(value, 0.99, na.rm = TRUE)),
      .groups = "drop"
    )
  
  dyn_long <- dyn_long %>%
    left_join(clip_tbl, by = "var") %>%
    mutate(value_clip = pmin(pmax(value, lo), hi),
           var_pretty = recode(var, atp = "ATP", ff = "FF", speed = "Speed", .default = var))
  
  dyn_long$var_pretty <- factor(dyn_long$var_pretty, levels = c("Speed","FF","ATP"))
  
  pG_main <- ggplot(dyn_long, aes(x = var_pretty, y = value_clip)) +
    geom_boxplot(width = 0.5, outlier.size = 0.25) +
    coord_flip() +
    labs(title = "Driving dynamics", x = NULL, y = "value") +
    theme_minimal(base_size = BASE_FONT)
}

if ("energy_rot" %in% names(dt_drive2)) {
  rot <- dt_drive2[, .(energy_rot = suppressWarnings(as.numeric(energy_rot)))]
  rot <- rot[is.finite(energy_rot)]
  if (nrow(rot) > 0) {
    lo <- as.numeric(quantile(rot$energy_rot, 0.01, na.rm = TRUE))
    hi <- as.numeric(quantile(rot$energy_rot, 0.99, na.rm = TRUE))
    rot[, energy_rot_clip := pmin(pmax(energy_rot, lo), hi)]
    
    pH <- ggplot(rot, aes(x = energy_rot_clip)) +
      geom_histogram(bins = 40) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      labs(title = "Hand Energy Rotation", x = "value (clipped 1–99%)", y = "count") +
      theme_minimal(base_size = BASE_FONT)
  } else {
    pH <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) +
      labs(title = "Hand Energy Rotation")
  }
} else {
  pH <- ggplot() + geom_blank() + theme_minimal(base_size = BASE_FONT) +
    labs(title = "Hand Energy Rotation")
}

pG <- pG_main / pH + plot_layout(heights = c(2, 1.2))

Figure1 <- (pA | pB) / (pC | pD) / (pE | pF | pG) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = BASE_FONT + 2),
    plot.subtitle = element_text(size = BASE_FONT),
    strip.text = element_text(size = BASE_FONT),
    axis.title = element_text(size = BASE_FONT),
    axis.text  = element_text(size = BASE_FONT - 1),
    legend.title = element_text(size = BASE_FONT),
    legend.text  = element_text(size = BASE_FONT - 1)
  )

out_pdf <- file.path(fig_dir, "Figure1-Table1.pdf")
out_png <- file.path(fig_dir, "Figure1-Table1.png")
safe_save_pdf(Figure1, out_pdf, w = 16, h = 12)
safe_save_png(Figure1, out_png, w = 16, h = 12, dpi = 300)

log_msg("Wrote Figure 1 PDF: ", out_pdf)
log_msg("Wrote Figure 1 PNG: ", out_png)
log_msg("DONE.")