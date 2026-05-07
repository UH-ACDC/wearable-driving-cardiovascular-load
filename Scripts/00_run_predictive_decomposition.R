# ============================================================
# NUBI-ML-v12_00-COMPARE_driving_vs_nondriving_DIRECTRAWHR_STRATASPEC_MASTER_NOAFFINE_WITH_IMPORTANCE.R
#
# FULL REPLACEMENT
#
# PURPOSE
#   Compare predictability of RAW_HR in:
#     (A) DRIVING rows
#     (B) NONDRIVING_SEDENTARY rows
#
#   using the current MASTER datasets:
#     Data/NUBI_Data_<RES>sec_Level_MASTER_CLEAN.csv
#
# KEY UPDATES VS LEGACY SCRIPT
#   - Uses MASTER_CLEAN datasets (not old HRBL_CLEANED_v2 files)
#   - Uses activity3 directly to define strata:
#       driving
#       non_driving_sedentary
#   - Includes morning_anxiety as a predictor
#   - Includes newer driving variables when present:
#       rtp, ff_speed, energy_acc
#   - Keeps trip_distance, drops distance from predictor pool
#   - Keeps NONDRIVING_SEDENTARY model lean and conceptually clean
#   - Uses weather_info only in DRIVING; excludes weather from NONDRIVING_SEDENTARY
#   - Retains fold-safe baselines:
#       baseline0       : raw_hat = bl_hr_person
#       baseline_offset : raw_hat = bl_hr_person + c_off
#   - ENet evaluated as:
#       Stage 1 tune_grid -> Stage 2 tune_grid -> FINAL fit_resamples()
#       with OOF predictions from FINAL fit_resamples()
#
# OUTPUT
#   Results/nubi_ml/<timestamp>_<RES>sec_v12_00_compare_drive_vs_nondrive_directRAWHR_MASTER_NOAFFINE_IMPORTANCE/
#
# MAIN OUTPUT FILE FOR FIGURE 5
#   predictions_all_models_both_strata.csv
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  
  library(tidymodels)
  library(doParallel)
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(rlang)
  library(stringr)
  library(scales)
  library(forcats)
})

options(warn = 1)
tidymodels_prefer()
set.seed(20260309)

# ----------------------------
# User toggles
# ----------------------------
LOCAL_TZ <- "America/Chicago"

USE_COMMON_SUBJECTS <- TRUE
V_OUTER <- 5

# Driving dynamics windows (minutes)
DYN_WINDOWS_MIN <- c(1, 3, 5)

# Applicability pruning thresholds
KEEP_MAX_NA   <- 0.95
KEEP_MIN_UNIQ <- 2

# If TRUE, include trip_id when it is not extremely high-cardinality
ALLOW_TRIP_ID_IF_REASONABLE <- FALSE
TRIP_ID_MAX_LEVEL_SHARE <- 0.50   # require n_unique_trip_id <= 50% of n_rows

# MASTER strata labels
ACT3_DRIVING <- "driving"
ACT3_ND_SED  <- "non_driving_sedentary"

# ----------------------------
# Robust wd = Scripts/
# ----------------------------
this_script <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)

script_dir <- if (!is.na(this_script) && file.exists(this_script)) {
  dirname(this_script)
} else {
  getwd()
}
script_dir <- normalizePath(script_dir, mustWork = TRUE)
setwd(script_dir)

project_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
results_root <- normalizePath(file.path(project_root, "Results"), mustWork = TRUE)
data_root    <- normalizePath(file.path(project_root, "Data"), mustWork = TRUE)

message("Working directory (Scripts): ", getwd())
message("Project root: ", project_root)

# ----------------------------
# Interactive resolution picker
# ----------------------------
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

RES_SECONDS <- pick_resolution()

in_path <- file.path(
  data_root,
  sprintf("NUBI_Data_%dsec_Level_MASTER_CLEAN.csv", RES_SECONDS)
)
stopifnot(file.exists(in_path))

# ----------------------------
# Output folder
# ----------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  results_root, "nubi_ml",
  paste0(
    stamp, "_", RES_SECONDS,
    "sec_v12_00_compare_drive_vs_nondrive_directRAWHR_MASTER_NOAFFINE_IMPORTANCE"
  )
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

log_msg("Script: v12.00 compare DRIVING vs NONDRIVING_SEDENTARY DIRECT RAW_HR (MASTER)")
log_msg("Resolution: ", RES_SECONDS, " sec")
log_msg("Input: ", in_path)
log_msg("Output dir: ", out_dir)
log_msg("Figure dir: ", fig_dir)
log_msg("LOCAL_TZ: ", LOCAL_TZ)
log_msg("USE_COMMON_SUBJECTS: ", USE_COMMON_SUBJECTS)
log_msg("V_OUTER: ", V_OUTER)

# ============================================================
# Helpers
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
  
  # Important: treat strings as local clock time
  # If there is a trailing Z, strip it rather than interpreting as UTC.
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
    expert_pa="expert_pa",
    
    data_source="data_source", datasource="data_source", source="data_source",
    
    trip_time="trip_time",
    triptime="trip_time",
    trip_id="trip_id",
    trips="trips",
    
    day_num="day_num",
    daynum="day_num",
    days="days",
    day_period="day_period",
    day_type="day_type",
    
    weather_info="weather_info",
    weather="weather_info",
    in_radius="in_radius",
    
    bl_hr="bl_hr",
    hr_bl="bl_hr",
    hrbl="bl_hr",
    baseline_hr="bl_hr",
    baseline="bl_hr",
    hr_baseline="bl_hr",
    
    nr_hr_sd="nr_hr_sd",
    nr_hr_2sd="nr_hr_2sd",
    
    speed="speed",
    atp="atp",
    jf="jf",
    ff="ff",
    ff_speed="ff_speed",
    rtp="rtp",
    energy_acc="energy_acc",
    energy_rot="energy_rot",
    distance="distance",
    trip_distance="trip_distance",
    trip_duration="trip_duration",
    
    live_lat="live_lat",
    live_long="live_long",
    live_lon="live_long",
    home_lat="home_lat",
    home_long="home_long",
    home_lon="home_long",
    first_point_lat="first_point_lat",
    first_point_lon="first_point_lon",
    first_point_long="first_point_lon",
    
    gender="gender",
    age="age",
    trait_anxiety="trait_anxiety",
    morning_anxiety="morning_anxiety",
    
    openness="openness",
    neuroticism="neuroticism",
    conscientiousness="conscientiousness",
    agreeableness="agreeableness",
    extraversion="extraversion",
    
    md="md", pd="pd", td="td", p="p", e="e", f="f"
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

safe_save_png <- function(plot_obj, path, w = 7, h = 5, dpi = 300) {
  ggsave(filename = path, plot = plot_obj, width = w, height = h, dpi = dpi)
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

compute_metrics_vec <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  if (length(y) < 5) return(tibble(rmse = NA_real_, mae = NA_real_, rsq = NA_real_, n = length(y)))
  rmse_v <- sqrt(mean((y - p)^2))
  mae_v  <- mean(abs(y - p))
  rsq_v  <- if (sd(y) == 0 || sd(p) == 0) NA_real_ else cor(y, p)^2
  tibble(rmse = rmse_v, mae = mae_v, rsq = rsq_v, n = length(y))
}

infer_fold_id <- function(pred_df) {
  if (".id" %in% names(pred_df)) return(as.character(pred_df[[".id"]]))
  if ("id" %in% names(pred_df)) return(as.character(pred_df[["id"]]))
  if (".config" %in% names(pred_df)) return(as.character(pred_df[[".config"]]))
  rep("fold_unknown", nrow(pred_df))
}

detect_fold_col <- function(pred_df) {
  for (nm in c("id", ".id", "id2", ".id2")) {
    if (nm %in% names(pred_df)) return(nm)
  }
  NA_character_
}

predictor_applicability <- function(df, pred_cols) {
  tibble(var = pred_cols) %>%
    mutate(
      na_rate = map_dbl(var, ~{
        x <- df[[.x]]
        if (is.character(x) || is.factor(x) || is.logical(x)) {
          mean(is.na(x), na.rm = FALSE)
        } else {
          xx <- suppressWarnings(as.numeric(x))
          mean(is.na(xx) | !is.finite(xx), na.rm = FALSE)
        }
      }),
      uniq_n = map_int(var, ~{
        x <- df[[.x]]
        if (is.character(x) || is.factor(x) || is.logical(x)) return(length(unique(x)))
        xx <- suppressWarnings(as.numeric(x))
        length(unique(xx[is.finite(xx)]))
      }),
      class = map_chr(var, ~paste(class(df[[.x]]), collapse = "|"))
    ) %>%
    arrange(desc(na_rate), uniq_n)
}

# ============================================================
# HARD leakage guards (allow ONLY bl_hr_person)
# ============================================================
FORBIDDEN_PRED_REGEX <- paste0(
  "(",
  "^raw_hr($|_)", "|",
  "^nhr($|_)", "|",
  "^hr_bl($|_)", "|",
  "^bl_hr_num($|_)", "|",
  "^bl_hr($|_(?!person$))", "|",
  "^nr_hr_sd($|_)", "|",
  "^nr_hr_2sd($|_)", "|",
  "(_obs$|_hat$|\\.pred$|^\\.pred$|^pred$|^prediction$)",
  ")"
)

assert_no_forbidden_predictors <- function(predictor_names, context = "") {
  bad <- predictor_names[grepl(FORBIDDEN_PRED_REGEX, predictor_names, perl = TRUE)]
  if (length(bad) > 0) {
    stop(
      "LEAKAGE GUARD TRIGGERED", if (nzchar(context)) paste0(" [", context, "]") else "",
      ": forbidden predictors present: ",
      paste(unique(bad), collapse = ", ")
    )
  }
  invisible(TRUE)
}

assert_no_forbidden_in_baked <- function(prepped_recipe, te_df, outcome_name = "raw_hr", context = "") {
  baked <- bake(prepped_recipe, new_data = te_df)
  cols_pred <- setdiff(colnames(baked), outcome_name)
  bad <- cols_pred[grepl(FORBIDDEN_PRED_REGEX, cols_pred, perl = TRUE)]
  if (length(bad) > 0) {
    stop(
      "LEAKAGE GUARD TRIGGERED", if (nzchar(context)) paste0(" [", context, "]") else "",
      ": forbidden columns present in baked predictors: ",
      paste(unique(bad), collapse = ", ")
    )
  }
  invisible(TRUE)
}

# ============================================================
# Dynamics (DRIVING ONLY)
# ============================================================
dyn_base_vars <- c(
  "speed", "ff", "ff_speed", "atp", "rtp", "jf",
  "energy_acc", "energy_rot"
)

add_dynamics <- function(dt,
                         id_col = "p_id",
                         time_col = "dt_time",
                         vars,
                         res_seconds,
                         windows_min = c(1, 3, 5)) {
  stopifnot(is.data.table(dt))
  stopifnot(id_col %in% names(dt), time_col %in% names(dt))
  stopifnot(all(vars %in% names(dt)))
  
  setorderv(dt, c(id_col, time_col))
  
  for (v in vars) {
    dt[, (v) := suppressWarnings(as.numeric(get(v)))]
    
    dt[, paste0(v, "_lag1")  := shift(get(v), 1L, type = "lag"), by = id_col]
    dt[, paste0(v, "_diff1") := get(v) - get(paste0(v, "_lag1")), by = id_col]
    
    for (wm in windows_min) {
      k <- max(2L, as.integer(round((wm * 60) / res_seconds)))
      tag <- paste0("_", wm, "m")
      
      rm_name <- paste0(v, "_rm", tag)
      rs_name <- paste0(v, "_rs", tag)
      sl_name <- paste0(v, "_slope", tag)
      
      dt[, (rm_name) := frollmean(get(v), n = k, align = "right", fill = NA_real_), by = id_col]
      dt[, (rs_name) := {
        m1 <- frollmean(get(v), n = k, align = "right", fill = NA_real_)
        m2 <- frollmean(get(v)^2, n = k, align = "right", fill = NA_real_)
        s2 <- m2 - m1^2
        sqrt(pmax(s2, 0))
      }, by = id_col]
      dt[, (sl_name) := (get(v) - shift(get(v), k - 1L, type = "lag")) /
           ((k - 1L) * res_seconds),
         by = id_col]
    }
  }
  
  dt
}

# ============================================================
# Feature importance (ENet): |standardized coefficients|
# ============================================================
extract_enet_importance <- function(fitted_wf, penalty_value, predictors_final) {
  eng <- workflows::extract_fit_engine(fitted_wf)
  
  cc <- tryCatch({
    as.matrix(stats::coef(eng, s = penalty_value))
  }, error = function(e) {
    as.matrix(stats::coef(eng))
  })
  
  coef_tbl <- tibble(
    term = rownames(cc),
    estimate = as.numeric(cc[, 1])
  ) %>%
    filter(term != "(Intercept)", is.finite(estimate)) %>%
    mutate(importance = abs(estimate)) %>%
    arrange(desc(importance))
  
  predictors_final <- unique(predictors_final)
  
  coef_tbl <- coef_tbl %>%
    mutate(group = purrr::map_chr(term, function(tt) {
      if (tt %in% predictors_final) return(tt)
      for (p in predictors_final) {
        if (startsWith(tt, paste0(p, "_"))) return(p)
      }
      tt
    }))
  
  group_tbl <- coef_tbl %>%
    group_by(group) %>%
    summarise(
      importance = sum(importance, na.rm = TRUE),
      n_terms = dplyr::n(),
      .groups = "drop"
    ) %>%
    arrange(desc(importance))
  
  list(term_tbl = coef_tbl, group_tbl = group_tbl)
}

plot_importance_bar <- function(tbl, name_col, value_col = "importance",
                                title = "", top_n = 25) {
  stopifnot(name_col %in% names(tbl), value_col %in% names(tbl))
  
  dd <- tbl %>%
    arrange(desc(.data[[value_col]])) %>%
    slice_head(n = top_n) %>%
    mutate(name = .data[[name_col]]) %>%
    mutate(name = forcats::fct_reorder(name, .data[[value_col]]))
  
  ggplot(dd, aes(x = name, y = .data[[value_col]])) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "|standardized coefficient| (sum if grouped)", title = title) +
    theme_minimal(base_size = 11)
}

# ============================================================
# Read + standardize schema
# ============================================================
log_msg("Reading FULL data ...")
dt_full <- fread(in_path, showProgress = TRUE)
dt_full <- canonicalize_names(dt_full)

need <- c("p_id", "time", "raw_hr", "activity3", "bl_hr")
miss <- setdiff(need, names(dt_full))
if (length(miss) > 0) {
  stop("Missing required columns after standardization: ", paste(miss, collapse = ", "))
}

dt_full[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
dt_full <- dt_full[!is.na(dt_time)]
log_msg("Rows after time parse: ", nrow(dt_full), " | subjects: ", uniqueN(dt_full$p_id))

dt_full[, activity3_norm := trimws(tolower(as.character(activity3)))]
act3_levels <- sort(unique(dt_full$activity3_norm))
write_csv(tibble(activity3_level = act3_levels), file.path(out_dir, "activity3_levels.csv"))
log_msg("Wrote activity3_levels.csv (n=", length(act3_levels), "). Levels: ", paste(act3_levels, collapse = ", "))

act3_counts <- dt_full[, .(n_rows = .N, n_subj = uniqueN(p_id)), by = .(activity3_norm)][order(-n_rows)]
write_csv(as.data.frame(act3_counts), file.path(out_dir, "activity3_counts.csv"))
log_msg("Wrote activity3_counts.csv")

# ============================================================
# Person-level baseline from subject-day medians of bl_hr
# ============================================================
dt_full[, bl_hr_num := suppressWarnings(as.numeric(bl_hr))]
day_key <- make_day_key(dt_full)
if (is.null(day_key)) stop("Could not construct day key. Need day_num/days or dt_time.")
dt_full[, day_key := day_key]

baseline_by_subj_day <- dt_full[
  is.finite(bl_hr_num) & !is.na(day_key),
  .(
    bl_hr_day = median(bl_hr_num, na.rm = TRUE),
    n_rows_used = .N
  ),
  by = .(p_id, day_key)
]
write_csv(as.data.frame(baseline_by_subj_day), file.path(out_dir, "baseline_by_subject_day.csv"))
log_msg("Wrote baseline_by_subject_day.csv: rows=", nrow(baseline_by_subj_day))
if (nrow(baseline_by_subj_day) == 0) stop("baseline_by_subject_day has 0 rows. Check bl_hr values.")

baseline_by_subj <- baseline_by_subj_day[
  ,
  .(
    bl_hr_person = mean(bl_hr_day, na.rm = TRUE),
    n_days_with_bl = .N,
    bl_hr_day_sd = sd(bl_hr_day, na.rm = TRUE)
  ),
  by = .(p_id)
]
write_csv(as.data.frame(baseline_by_subj), file.path(out_dir, "baseline_by_subject.csv"))
log_msg("Wrote baseline_by_subject.csv: n_subjects=", nrow(baseline_by_subj))

dt_full <- merge(
  dt_full,
  baseline_by_subj[, .(p_id, bl_hr_person, n_days_with_bl)],
  by = "p_id",
  all.x = TRUE
)
dt_full <- dt_full[is.finite(bl_hr_person)]
log_msg("After dropping missing-baseline subjects: rows=", nrow(dt_full), " | subjects=", uniqueN(dt_full$p_id))

# ============================================================
# Split into strata using activity3
# ============================================================
dt_drive <- dt_full[activity3_norm == ACT3_DRIVING]
dt_nond  <- dt_full[activity3_norm == ACT3_ND_SED]

log_msg("DRIVING rows: ", nrow(dt_drive), " | subjects: ", uniqueN(dt_drive$p_id))
log_msg("NONDRIVING_SEDENTARY rows: ", nrow(dt_nond), " | subjects: ", uniqueN(dt_nond$p_id))

if (nrow(dt_drive) == 0) stop("No DRIVING rows. Check activity3 levels.")
if (nrow(dt_nond) == 0) stop("No NONDRIVING_SEDENTARY rows. Check activity3 levels.")

if (USE_COMMON_SUBJECTS) {
  common_subj <- intersect(unique(dt_drive$p_id), unique(dt_nond$p_id))
  dt_drive <- dt_drive[p_id %in% common_subj]
  dt_nond  <- dt_nond[p_id %in% common_subj]
  
  log_msg("COMMON subjects enforced: ", length(common_subj))
  log_msg("DRIVING rows (common): ", nrow(dt_drive), " | subjects: ", uniqueN(dt_drive$p_id))
  log_msg("NONDRIVING rows (common): ", nrow(dt_nond), " | subjects: ", uniqueN(dt_nond$p_id))
  
  if (uniqueN(dt_drive$p_id) < V_OUTER || uniqueN(dt_nond$p_id) < V_OUTER) {
    stop("After enforcing common subjects, too few subjects for CV. Consider USE_COMMON_SUBJECTS <- FALSE.")
  }
}

# ============================================================
# Add dynamics to DRIVING ONLY
# ============================================================
add_dynamics_if_possible <- function(dt, label) {
  have <- intersect(dyn_base_vars, names(dt))
  if (length(have) == 0) {
    log_msg(label, ": No base vars for dynamics present; skipping dynamics.")
    return(dt)
  }
  
  log_msg(label, ": Adding dynamics for: ", paste(have, collapse = ", "))
  dt <- add_dynamics(
    dt,
    id_col = "p_id",
    time_col = "dt_time",
    vars = have,
    res_seconds = RES_SECONDS,
    windows_min = DYN_WINDOWS_MIN
  )
  log_msg(label, ": Dynamics added.")
  dt
}

dt_drive <- add_dynamics_if_possible(dt_drive, "DRIVING")
log_msg("NONDRIVING_SEDENTARY: dynamics intentionally skipped.")

# ============================================================
# Parallel
# ============================================================
n_cores <- max(1, parallel::detectCores() - 1)
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)
log_msg("Parallel workers: ", n_cores)

# ============================================================
# Predictor selection per stratum
# ============================================================
base_predictors_common <- c(
  "day_num", "days", "day_period", "day_type",
  "trip_time", "in_radius",
  "gender", "age", "trait_anxiety", "morning_anxiety",
  "openness", "neuroticism", "conscientiousness",
  "agreeableness", "extraversion",
  "bl_hr_person"
)

driving_only_core <- c(
  "weather_info",
  "speed", "atp", "jf", "ff", "ff_speed", "rtp",
  "energy_acc", "energy_rot",
  "trip_distance", "trip_duration",
  "md", "pd", "td", "p", "e", "f"
)

select_predictors_for_stratum <- function(dt_stratum, stratum_name, out_dir) {
  cand <- base_predictors_common
  
  if (stratum_name == "DRIVING") {
    cand <- unique(c(cand, driving_only_core))
    
    if (ALLOW_TRIP_ID_IF_REASONABLE && "trip_id" %in% names(dt_stratum)) {
      trip_n_unique <- uniqueN(as.character(dt_stratum$trip_id))
      trip_share <- trip_n_unique / max(1, nrow(dt_stratum))
      if (is.finite(trip_share) && trip_share <= TRIP_ID_MAX_LEVEL_SHARE) {
        cand <- unique(c(cand, "trip_id"))
        log_msg("DRIVING: trip_id allowed as predictor (unique share=", signif(trip_share, 4), ")")
      } else {
        log_msg("DRIVING: trip_id excluded for high cardinality (unique share=", signif(trip_share, 4), ")")
      }
    }
    
    have_dyn <- intersect(dyn_base_vars, names(dt_stratum))
    dyn_cols <- if (length(have_dyn) > 0) {
      grep(
        pattern = paste0("^(", paste(have_dyn, collapse = "|"), ")_"),
        x = names(dt_stratum),
        value = TRUE
      )
    } else {
      character(0)
    }
    cand <- unique(c(cand, dyn_cols))
  }
  
  drop_exact <- c(
    "nr_hr_sd", "nr_hr_2sd",
    "dt_time", "time",
    "data_source", "activity", "activity3", "activity3_norm", "expert_pa",
    "day_key", "n_days_with_bl",
    "bl_hr", "bl_hr_num", "nhr",
    "distance",
    "live_lat", "live_long", "home_lat", "home_long", "first_point_lat", "first_point_lon"
  )
  
  predictors0 <- intersect(cand, names(dt_stratum))
  predictors0 <- setdiff(predictors0, drop_exact)
  predictors0 <- predictors0[!grepl(FORBIDDEN_PRED_REGEX, predictors0, perl = TRUE)]
  
  if (stratum_name != "DRIVING") {
    driving_regex <- "^(md|pd|td|p$|e$|f$|speed|atp|jf|ff|ff_speed|rtp|energy_acc|energy_rot|trip_distance|trip_duration|trip_id|weather_info)($|_)"
    predictors0 <- predictors0[!grepl(driving_regex, predictors0)]
    predictors0 <- setdiff(predictors0, "weather_info")
  }
  
  df_tmp <- dt_stratum[, c("p_id", "raw_hr", predictors0), with = FALSE] |> as.data.frame()
  
  appl <- predictor_applicability(df_tmp, predictors0) %>%
    mutate(stratum = stratum_name, .before = 1)
  
  write_csv(appl, file.path(out_dir, paste0("predictor_applicability_raw_", stratum_name, ".csv")))
  
  good_vars <- appl %>%
    filter(na_rate <= KEEP_MAX_NA, uniq_n >= KEEP_MIN_UNIQ) %>%
    pull(var)
  
  predictors <- good_vars
  assert_no_forbidden_predictors(predictors, context = paste0("predictor_selection_", stratum_name))
  
  out_name <- paste0("predictor_list_", stratum_name, ".csv")
  write_csv(tibble(stratum = stratum_name, predictor = predictors), file.path(out_dir, out_name))
  log_msg(stratum_name, ": FINAL predictor count = ", length(predictors), " (written to ", out_name, ")")
  
  predictors
}

log_msg("Selecting predictors for DRIVING ...")
preds_drive <- select_predictors_for_stratum(dt_drive, "DRIVING", out_dir)

log_msg("Selecting predictors for NONDRIVING_SEDENTARY ...")
preds_nond <- select_predictors_for_stratum(dt_nond, "NONDRIVING_SEDENTARY", out_dir)

write_csv(
  bind_rows(
    tibble(stratum = "DRIVING", predictor = preds_drive),
    tibble(stratum = "NONDRIVING_SEDENTARY", predictor = preds_nond)
  ),
  file.path(out_dir, "predictor_list_BOTH.csv")
)

# ============================================================
# Core evaluation for one stratum
# ============================================================
run_stratum <- function(dt_stratum, stratum_name, predictors_final) {
  log_msg("---- Stratum START: ", stratum_name, " ----")
  
  id_col <- "p_id"
  target_raw <- "raw_hr"
  
  assert_no_forbidden_predictors(predictors_final, context = paste0("run_stratum_", stratum_name))
  
  use_cols <- unique(c(id_col, target_raw, predictors_final))
  df <- dt_stratum[, use_cols, with = FALSE] |> as.data.frame()
  
  df[[id_col]]     <- as.factor(df[[id_col]])
  df[[target_raw]] <- suppressWarnings(as.numeric(df[[target_raw]]))
  df$row_id <- seq_len(nrow(df))
  
  if ("bl_hr_person" %in% names(df)) {
    df$bl_hr_person <- suppressWarnings(as.numeric(df$bl_hr_person))
  }
  
  df <- df[is.finite(df[[target_raw]]), , drop = FALSE]
  
  if (nlevels(df[[id_col]]) < V_OUTER) {
    stop(stratum_name, ": too few subjects for CV (need >= ", V_OUTER, ").")
  }
  if (!("bl_hr_person" %in% names(df))) {
    stop(stratum_name, ": bl_hr_person missing; baselines require it.")
  }
  
  log_msg(
    stratum_name, ": rows=", nrow(df),
    " | subjects=", nlevels(df[[id_col]]),
    " | predictors=", length(predictors_final)
  )
  
  # Distribution diagnostics
  nhr_tmp <- df[[target_raw]] - df$bl_hr_person
  dist_diag <- tibble(
    stratum = stratum_name,
    n_rows = nrow(df),
    n_subj = nlevels(df[[id_col]]),
    pct_raw_above_baseline = mean(df[[target_raw]] > df$bl_hr_person, na.rm = TRUE),
    mean_nhr = mean(nhr_tmp, na.rm = TRUE),
    sd_nhr   = sd(nhr_tmp, na.rm = TRUE),
    p05_nhr  = as.numeric(quantile(nhr_tmp, 0.05, na.rm = TRUE)),
    p50_nhr  = as.numeric(quantile(nhr_tmp, 0.50, na.rm = TRUE)),
    p95_nhr  = as.numeric(quantile(nhr_tmp, 0.95, na.rm = TRUE))
  )
  
  # Outer CV folds grouped by subject
  set.seed(20260309)
  outer_folds <- rsample::group_vfold_cv(df, group = !!sym(id_col), v = V_OUTER)
  
  # Fold-safe baselines
  offsets_df <- purrr::map2_dfr(
    outer_folds$splits,
    outer_folds$id,
    function(spl, fold_id) {
      tr <- rsample::analysis(spl)
      c_off <- mean(tr[[target_raw]] - tr$bl_hr_person, na.rm = TRUE)
      
      tibble(
        stratum = stratum_name,
        fold = fold_id,
        c_off = c_off,
        n_tr = nrow(tr),
        n_te = nrow(rsample::assessment(spl)),
        n_subj_tr = nlevels(droplevels(tr[[id_col]])),
        n_subj_te = nlevels(droplevels(rsample::assessment(spl)[[id_col]]))
      )
    }
  )
  
  preds_baseline <- purrr::map2_dfr(
    outer_folds$splits,
    outer_folds$id,
    function(spl, fold_id) {
      tr <- rsample::analysis(spl)
      te <- rsample::assessment(spl)
      
      c_off <- mean(tr[[target_raw]] - tr$bl_hr_person, na.rm = TRUE)
      
      bind_rows(
        tibble(
          model = "baseline0",
          stratum = stratum_name,
          fold = fold_id,
          row_id = te$row_id,
          bl_hr_person = te$bl_hr_person,
          raw_hr_obs = te[[target_raw]],
          raw_hr_hat = te$bl_hr_person
        ),
        tibble(
          model = "baseline_offset",
          stratum = stratum_name,
          fold = fold_id,
          row_id = te$row_id,
          bl_hr_person = te$bl_hr_person,
          raw_hr_obs = te[[target_raw]],
          raw_hr_hat = te$bl_hr_person + c_off
        )
      ) %>%
        mutate(
          nhr_obs = raw_hr_obs - bl_hr_person,
          nhr_hat = raw_hr_hat - bl_hr_person
        )
    }
  )
  
  # Recipe
  rec <- recipe(raw_hr ~ ., data = df) %>%
    update_role(all_of(c(id_col, "row_id")), new_role = "id") %>%
    step_string2factor(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors(), new_level = "Unknown") %>%
    step_novel(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  # Leak check on representative split
  prepped0 <- prep(rec, training = rsample::analysis(outer_folds$splits[[1]]), verbose = FALSE)
  assert_no_forbidden_in_baked(
    prepped0,
    te_df = rsample::assessment(outer_folds$splits[[1]]),
    outcome_name = "raw_hr",
    context = paste0(stratum_name, "_baked_check_outer1")
  )
  
  # ENet spec
  enet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")
  enet_wf   <- workflow() %>% add_recipe(rec) %>% add_model(enet_spec)
  
  metrics_raw <- metric_set(rmse, mae, rsq)
  
  ctrl_grid <- control_grid(
    save_pred = TRUE,
    save_workflow = FALSE,
    parallel_over = "resamples",
    verbose = FALSE
  )
  
  # ----------------------------
  # Stage 1 tuning
  # ----------------------------
  set.seed(20260309)
  grid1 <- tidyr::crossing(
    penalty = 10^seq(-6, -1, length.out = 12),
    mixture = seq(0, 1, length.out = 6)
  )
  
  log_msg(stratum_name, ": ENet Stage1 tuning | grid=", nrow(grid1))
  enet_res1 <- tune_grid(
    enet_wf,
    resamples = outer_folds,
    grid = grid1,
    metrics = metrics_raw,
    control = ctrl_grid
  )
  saveRDS(enet_res1, file.path(out_dir, paste0("enet_stage1_", stratum_name, ".rds")))
  
  best1 <- select_best(enet_res1, metric = "rmse")
  log_msg(
    stratum_name, ": Stage1 best penalty=", signif(best1$penalty, 6),
    " mixture=", signif(best1$mixture, 6)
  )
  
  # ----------------------------
  # Stage 2 tuning
  # ----------------------------
  pen <- best1$penalty
  mix <- best1$mixture
  
  pen_log <- log10(pen)
  pen_grid2 <- 10^seq(pen_log - 0.75, pen_log + 0.75, length.out = 12)
  mix_grid2 <- sort(unique(pmin(1, pmax(0, mix + seq(-0.3, 0.3, length.out = 7)))))
  
  grid2 <- tidyr::crossing(
    penalty = pen_grid2,
    mixture = mix_grid2
  )
  
  set.seed(20260309)
  log_msg(stratum_name, ": ENet Stage2 tuning | grid=", nrow(grid2))
  enet_res2 <- tune_grid(
    enet_wf,
    resamples = outer_folds,
    grid = grid2,
    metrics = metrics_raw,
    control = ctrl_grid
  )
  saveRDS(enet_res2, file.path(out_dir, paste0("enet_stage2_", stratum_name, ".rds")))
  
  best2 <- select_best(enet_res2, metric = "rmse")
  best_df <- tibble(stratum = stratum_name, penalty = best2$penalty, mixture = best2$mixture)
  write_csv(best_df, file.path(out_dir, paste0("best_params_", stratum_name, ".csv")))
  
  log_msg(
    stratum_name, ": Stage2 best penalty=", signif(best2$penalty, 6),
    " mixture=", signif(best2$mixture, 6)
  )
  
  # ----------------------------
  # FINAL evaluation
  # ----------------------------
  final_wf <- finalize_workflow(enet_wf, best2)
  
  set.seed(20260309)
  log_msg(stratum_name, ": Final fit_resamples (fixed best2)")
  final_rs <- fit_resamples(
    final_wf,
    resamples = outer_folds,
    metrics = metrics_raw,
    control = control_resamples(save_pred = TRUE)
  )
  
  saveRDS(final_rs, file.path(out_dir, paste0("enet_cv_fit_", stratum_name, ".rds")))
  collect_metrics(final_rs) |> readr::write_csv(file.path(out_dir, paste0("enet_cv_metrics_", stratum_name, ".csv")))
  
  pred_enet <- collect_predictions(final_rs)
  if (!(".row" %in% names(pred_enet))) stop(stratum_name, ": collect_predictions(final_rs) missing .row")
  if (!(".pred" %in% names(pred_enet))) stop(stratum_name, ": collect_predictions(final_rs) missing .pred")
  
  fold_col <- detect_fold_col(pred_enet)
  if (is.na(fold_col)) {
    pred_enet$fold_tmp <- infer_fold_id(pred_enet)
    fold_col <- "fold_tmp"
  }
  
  df_map <- df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::transmute(
      .row,
      row_id,
      bl_hr_person,
      raw_hr_obs_map = .data[[target_raw]]
    )
  
  preds_enet <- pred_enet %>%
    dplyr::left_join(df_map, by = ".row") %>%
    dplyr::mutate(
      model = "enet",
      stratum = stratum_name,
      fold = .data[[fold_col]],
      raw_hr_obs = raw_hr_obs_map,
      raw_hr_hat = .pred,
      nhr_obs = raw_hr_obs - bl_hr_person,
      nhr_hat = raw_hr_hat - bl_hr_person
    ) %>%
    dplyr::select(model, stratum, fold, row_id, bl_hr_person, raw_hr_obs, raw_hr_hat, nhr_obs, nhr_hat)
  
  # ----------------------------
  # Feature importance: refit full stratum
  # ----------------------------
  log_msg(stratum_name, ": refit final ENet on FULL data for feature importance")
  final_fit_full <- fit(final_wf, data = df)
  
  imp <- extract_enet_importance(
    fitted_wf = final_fit_full,
    penalty_value = best2$penalty,
    predictors_final = predictors_final
  )
  
  write_csv(imp$term_tbl,  file.path(out_dir, paste0("feature_importance_terms_", stratum_name, ".csv")))
  write_csv(imp$group_tbl, file.path(out_dir, paste0("feature_importance_grouped_", stratum_name, ".csv")))
  
  p_imp_terms <- plot_importance_bar(
    imp$term_tbl, name_col = "term", top_n = 30,
    title = paste0("ENet feature importance (term-level) — ", stratum_name)
  )
  safe_save_pdf(p_imp_terms, file.path(fig_dir, paste0("Fig_FeatureImportance_Terms_", stratum_name, ".pdf")), w = 10.0, h = 7.0)
  safe_save_png(p_imp_terms, file.path(fig_dir, paste0("Fig_FeatureImportance_Terms_", stratum_name, ".png")), w = 10.0, h = 7.0, dpi = 300)
  
  p_imp_group <- plot_importance_bar(
    imp$group_tbl, name_col = "group", top_n = 30,
    title = paste0("ENet feature importance (grouped) — ", stratum_name)
  )
  safe_save_pdf(p_imp_group, file.path(fig_dir, paste0("Fig_FeatureImportance_Grouped_", stratum_name, ".pdf")), w = 10.0, h = 7.0)
  safe_save_png(p_imp_group, file.path(fig_dir, paste0("Fig_FeatureImportance_Grouped_", stratum_name, ".png")), w = 10.0, h = 7.0, dpi = 300)
  
  # Combine predictions
  preds_df <- bind_rows(preds_baseline, preds_enet)
  
  # Metrics
  m_raw_byfold <- preds_df %>%
    group_by(stratum, model, fold) %>%
    group_modify(~{
      mm <- compute_metrics_vec(.x$raw_hr_obs, .x$raw_hr_hat)
      tibble(.metric = c("rmse", "mae", "rsq"),
             .estimate = c(mm$rmse, mm$mae, mm$rsq),
             n = mm$n)
    }) %>%
    ungroup()
  
  m_raw_overall <- preds_df %>%
    group_by(stratum, model) %>%
    group_modify(~{
      mm <- compute_metrics_vec(.x$raw_hr_obs, .x$raw_hr_hat)
      tibble(.metric = c("rmse", "mae", "rsq"),
             .estimate = c(mm$rmse, mm$mae, mm$rsq),
             n = mm$n)
    }) %>%
    ungroup()
  
  m_nhr_byfold <- preds_df %>%
    group_by(stratum, model, fold) %>%
    group_modify(~{
      mm <- compute_metrics_vec(.x$nhr_obs, .x$nhr_hat)
      tibble(.metric = c("rmse", "mae", "rsq"),
             .estimate = c(mm$rmse, mm$mae, mm$rsq),
             n = mm$n)
    }) %>%
    ungroup()
  
  m_nhr_overall <- preds_df %>%
    group_by(stratum, model) %>%
    group_modify(~{
      mm <- compute_metrics_vec(.x$nhr_obs, .x$nhr_hat)
      tibble(.metric = c("rmse", "mae", "rsq"),
             .estimate = c(mm$rmse, mm$mae, mm$rsq),
             n = mm$n)
    }) %>%
    ungroup()
  
  log_msg("---- Stratum END: ", stratum_name, " ----")
  
  list(
    dist_diag = dist_diag,
    offsets = offsets_df,
    best = best_df,
    preds = preds_df,
    m_raw_byfold = m_raw_byfold,
    m_raw_overall = m_raw_overall,
    m_nhr_byfold = m_nhr_byfold,
    m_nhr_overall = m_nhr_overall
  )
}

# ============================================================
# Run both strata
# ============================================================
res_drive <- run_stratum(dt_drive, "DRIVING", preds_drive)
res_nond  <- run_stratum(dt_nond,  "NONDRIVING_SEDENTARY", preds_nond)

# ============================================================
# Save outputs
# ============================================================
dist_all <- bind_rows(res_drive$dist_diag, res_nond$dist_diag)
write_csv(dist_all, file.path(out_dir, "distribution_diagnostics_by_stratum.csv"))

offsets_all <- bind_rows(res_drive$offsets, res_nond$offsets)
write_csv(offsets_all, file.path(out_dir, "outer_fold_offsets_by_stratum.csv"))

best_all <- bind_rows(res_drive$best, res_nond$best)
write_csv(best_all, file.path(out_dir, "best_params_by_stratum.csv"))

preds_all <- bind_rows(res_drive$preds, res_nond$preds)
write_csv(preds_all, file.path(out_dir, "predictions_all_models_both_strata.csv"))

m_raw_byfold_all  <- bind_rows(res_drive$m_raw_byfold,  res_nond$m_raw_byfold)
m_raw_overall_all <- bind_rows(res_drive$m_raw_overall, res_nond$m_raw_overall)
m_nhr_byfold_all  <- bind_rows(res_drive$m_nhr_byfold,  res_nond$m_nhr_byfold)
m_nhr_overall_all <- bind_rows(res_drive$m_nhr_overall, res_nond$m_nhr_overall)

write_csv(m_raw_byfold_all,  file.path(out_dir, "compare_metrics_rawhr_by_fold_by_stratum.csv"))
write_csv(m_raw_overall_all, file.path(out_dir, "compare_metrics_rawhr_overall_by_stratum.csv"))
write_csv(m_nhr_byfold_all,  file.path(out_dir, "compare_metrics_nhrhat_by_fold_by_stratum.csv"))
write_csv(m_nhr_overall_all, file.path(out_dir, "compare_metrics_nhrhat_overall_by_stratum.csv"))

log_msg("Saved all result CSVs.")

# ============================================================
# Figures (performance)
# ============================================================
p_above <- dist_all %>%
  ggplot(aes(x = stratum, y = pct_raw_above_baseline)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "% RAW_HR > baseline",
    title = "How often is HR above baseline? (Driving vs Sedentary Non-driving)"
  ) +
  theme_minimal(base_size = 12)
safe_save_pdf(p_above, file.path(fig_dir, "Fig_AboveBaselineRate_byStratum.pdf"), w = 7.4, h = 4.2)
safe_save_png(p_above, file.path(fig_dir, "Fig_AboveBaselineRate_byStratum.png"), w = 7.4, h = 4.2, dpi = 300)

p_perf_raw <- m_raw_overall_all %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() +
  facet_grid(.metric ~ stratum, scales = "free_y") +
  labs(
    x = NULL,
    y = "Estimate",
    title = "Overall CV performance — RAW_HR (Driving vs Sedentary Non-driving)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
safe_save_pdf(p_perf_raw, file.path(fig_dir, "Fig_PerformanceOverall_RAWHR_byStratum.pdf"), w = 10.0, h = 5.2)
safe_save_png(p_perf_raw, file.path(fig_dir, "Fig_PerformanceOverall_RAWHR_byStratum.png"), w = 10.0, h = 5.2, dpi = 300)

p_perf_nhr <- m_nhr_overall_all %>%
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() +
  facet_grid(.metric ~ stratum, scales = "free_y") +
  labs(
    x = NULL,
    y = "Estimate",
    title = "Overall CV performance — NHR_HAT (Driving vs Sedentary Non-driving)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
safe_save_pdf(p_perf_nhr, file.path(fig_dir, "Fig_PerformanceOverall_NHRHAT_byStratum.pdf"), w = 10.0, h = 5.2)
safe_save_png(p_perf_nhr, file.path(fig_dir, "Fig_PerformanceOverall_NHRHAT_byStratum.png"), w = 10.0, h = 5.2, dpi = 300)

log_msg("DONE. Outputs in: ", out_dir)