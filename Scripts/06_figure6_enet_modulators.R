# ============================================================
# Figure6_ENet_Modulators_v12.R  (FULL REPLACEMENT)
#
# PURPOSE
#   Build Figure 6 for the paper:
#   "ENet modulators beyond baseline and context-level tax"
#
# FIGURE PANELS (2 x 2)
#   A. Grouped feature importance by stratum
#      (excluding baseline-dominant terms)
#
#   B. Signed coefficients for interpretable terms
#      (sex / day / period-of-day / driving weather / workload / personality)
#
#   C. Top continuous modulators
#      shown as coefficient-implied effect curves
#
#   D. ENet gain beyond baseline+offset
#      (Delta RMSE, Delta R^2)
#
# INPUTS
#   1) Final clean MASTER dataset:
#        Data/NUBI_Data_<RES>sec_Level_MASTER_CLEAN.csv
#
#   2) Existing ML output folder under:
#        Results/nubi_ml/<RUN_FOLDER>/
#
#      Expected files:
#        feature_importance_grouped_DRIVING.csv
#        feature_importance_grouped_NONDRIVING_SEDENTARY.csv
#        feature_importance_terms_DRIVING.csv
#        feature_importance_terms_NONDRIVING_SEDENTARY.csv
#        compare_metrics_rawhr_overall_by_stratum.csv
#
# OUTPUT
#   Results/paper_figs/<STAMP>_<RES>sec_Figure6_ENet_Modulators/
#      Figure6_ENet_Modulators.pdf
#      Figure6_ENet_Modulators.png
#      diagnostics_summary.txt
#      diag_counts_by_stratum.csv
#      diag_panelA_grouped_features.csv
#      diag_panelB_terms.csv
#      diag_panelC_selected_modulators.csv
#      diag_panelC_effect_curves.csv
#      diag_panelD_incremental_gain.csv
#
# NOTES
#   - Relative directory structure assumed:
#       project_root/
#         Scripts/
#         Data/
#         Results/
#            nubi_ml/
#            paper_figs/
#   - This script assumes the ENet outputs were fit to direct RAW_HR.
#   - Panel C is a coefficient-implied linear effect summary, not a PDP/ALE.
#   - Aligned with v12 upstream ML script:
#       weather_info is treated as DRIVING-specific and excluded from
#       NONDRIVING_SEDENTARY displays as a defensive guard.
#   - Display labels are cleaned for paper consistency:
#       weather_info_other -> adverse_weather
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(scales)
  library(patchwork)
})

options(warn = 1)
set.seed(20260309)

# ============================================================
# USER SETTINGS
# ============================================================
LOCAL_TZ <- "America/Chicago"

# Use exact folder name inside Results/nubi_ml, or leave NULL to auto-pick
RUN_DIR_NAME <- NULL

# Optional extra filter when auto-picking; usually leave blank
RUN_DIR_SUFFIX_REGEX <- "v12_00_compare_drive_vs_nondrive_directRAWHR_MASTER_NOAFFINE_IMPORTANCE"

TOPK_GROUPED  <- 12L
TOPK_TERMS    <- 14L
N_GRID        <- 100L
Q_LO          <- 0.05
Q_HI          <- 0.95

REL_CUT_GROUPED <- 0.01
REL_CUT_TERMABS <- 0.01

# Paper-consistent colors
COL_DRIVE <- "#E69F00"   # orange
COL_NOND  <- "gray45"    # non-driving sedentary
COL_POS   <- "#D55E00"   # positive
COL_NEG   <- "#0072B2"   # negative

# Exclude baseline-dominant / structural terms from "modulator" emphasis
EXCLUDE_GROUPS <- c(
  "bl_hr",
  "bl_hr_person",
  "baseline",
  "baseline_hr",
  "offset",
  "context_offset",
  "raw_hr_offset"
)

# Interpretable term prefixes for Panel B
TERM_REGEX <- paste0(
  "^(",
  paste(
    c(
      "sex_",
      "gender_",
      "day_num_",
      "days_",
      "weather_info_",
      "trip_time_",
      "md$",
      "pd$",
      "td$",
      "p$",
      "e$",
      "f$",
      "age$",
      "trait_anxiety$",
      "morning_anxiety$",
      "neuroticism$",
      "extraversion$",
      "openness$",
      "agreeableness$",
      "conscientiousness$"
    ),
    collapse = "|"
  ),
  ")"
)

# ============================================================
# ROBUST wd = Scripts/
# ============================================================
this_script <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
if (!is.na(this_script) && file.exists(this_script)) {
  setwd(dirname(this_script))
}
message("Working directory (Scripts): ", getwd())

project_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)

paper_fig_root <- file.path(project_root, "Results", "paper_figs")
dir.create(paper_fig_root, recursive = TRUE, showWarnings = FALSE)

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
  x <- as.character(x)
  x <- gsub("Z$", "", x)
  
  tt <- suppressWarnings(ymd_hms(x, tz = tz, quiet = TRUE))
  if (!all(is.na(tt))) return(tt)
  
  suppressWarnings(parse_date_time(
    x,
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
    
    raw_hr="raw_hr", hr="raw_hr",
    bl_hr="bl_hr", baseline_hr="bl_hr", hr_bl="bl_hr", hrbl="bl_hr",
    
    activity="activity", activity3="activity3",
    data_source="data_source", datasource="data_source", source="data_source",
    
    trip_time="trip_time", triptime="trip_time",
    day_num="day_num", daynum="day_num",
    days="days",
    weather_info="weather_info", weather="weather_info",
    in_radius="in_radius", inradius="in_radius",
    
    sex="sex", gender="sex",
    age="age",
    trait_anxiety="trait_anxiety",
    morning_anxiety="morning_anxiety",
    
    openness="openness",
    neuroticism="neuroticism",
    conscientiousness="conscientiousness",
    agreeableness="agreeableness",
    extraversion="extraversion",
    
    md="md", pd="pd", td="td", p="p", e="e", f="f",
    
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
    trip_duration="trip_duration"
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

pretty_term_label <- function(x) {
  x <- as.character(x)
  
  x <- dplyr::recode(
    x,
    "weather_info"        = "driving_weather",
    "weather_info_other"  = "adverse_weather",
    "weather_info_clouds" = "cloudy",
    "gender_female"       = "female",
    "gender_male"         = "male",
    .default = x
  )
  
  x
}

norm_chr <- function(x) {
  trimws(tolower(as.character(x)))
}

make_day_key <- function(dt) {
  if ("day_num" %in% names(dt)) {
    x <- dt[["day_num"]]
    if (is.numeric(x) || is.integer(x)) return(as.integer(x))
    xs <- as.character(x)
    dig <- suppressWarnings(as.integer(str_extract(xs, "\\d+")))
    if (!all(is.na(dig))) return(dig)
  }
  
  if ("days" %in% names(dt)) {
    x <- dt[["days"]]
    if (is.numeric(x) || is.integer(x)) return(as.integer(x))
    xs <- as.character(x)
    dig <- suppressWarnings(as.integer(str_extract(xs, "\\d+")))
    if (!all(is.na(dig))) return(dig)
  }
  
  if ("dt_time" %in% names(dt) && inherits(dt[["dt_time"]], "POSIXt")) {
    dd <- as.Date(dt[["dt_time"]], tz = LOCAL_TZ)
    uu <- sort(unique(dd))
    return(as.integer(match(dd, uu)))
  }
  
  NULL
}

pick_context_column <- function(dt) {
  if ("activity3" %in% names(dt)) return("activity3")
  if ("activity"  %in% names(dt)) return("activity")
  if ("data_source" %in% names(dt)) return("data_source")
  stop("Could not find context column. Need one of: activity3, activity, data_source.")
}

label_strata <- function(dt, context_col) {
  ctx <- norm_chr(dt[[context_col]])
  
  driving_levels <- c("driving")
  nond_levels    <- c(
    "non_driving_sedentary",
    "non driving sedentary",
    "non-driving-sedentary",
    "non_driving",
    "non-driving",
    "nondriving",
    "non driving"
  )
  
  dt[, stratum_label := NA_character_]
  dt[ctx %in% driving_levels, stratum_label := "DRIVING"]
  dt[ctx %in% nond_levels,    stratum_label := "NONDRIVING_SEDENTARY"]
  
  dt
}

add_bl_hr_person <- function(dt) {
  stopifnot(is.data.table(dt))
  stopifnot("p_id" %in% names(dt))
  stopifnot("bl_hr" %in% names(dt))
  
  dt[, bl_hr_num := suppressWarnings(as.numeric(bl_hr))]
  dt <- dt[is.finite(bl_hr_num)]
  
  day_key <- make_day_key(dt)
  if (is.null(day_key)) {
    stop("Could not construct day key from day_num/days/dt_time.")
  }
  dt[, day_key := day_key]
  
  by_day <- dt[
    is.finite(bl_hr_num) & !is.na(day_key),
    .(bl_hr_day = median(bl_hr_num, na.rm = TRUE)),
    by = .(p_id, day_key)
  ]
  
  by_person <- by_day[
    , .(bl_hr_person = mean(bl_hr_day, na.rm = TRUE)),
    by = p_id
  ]
  
  merge(dt, by_person, by = "p_id", all.x = TRUE)
}

is_numericish <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(TRUE)
  xx <- suppressWarnings(as.numeric(x))
  !all(is.na(xx))
}

dyn_base_vars <- c(
  "speed", "atp", "jf", "ff", "ff_speed", "rtp", "energy_acc", "energy_rot",
  "distance", "trip_distance", "trip_duration"
)

add_dynamics <- function(dt,
                         id_col = "p_id",
                         time_col = "dt_time",
                         vars,
                         res_seconds,
                         windows_min = c(1, 3, 5)) {
  stopifnot(is.data.table(dt))
  stopifnot(all(c(id_col, time_col) %in% names(dt)))
  
  vars <- intersect(vars, names(dt))
  if (length(vars) == 0) return(dt)
  
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
      
      dt[, (sl_name) := (get(v) - shift(get(v), k - 1L, type = "lag")) / ((k - 1L) * res_seconds),
         by = id_col]
    }
  }
  
  for (dv in intersect(c("distance", "trip_distance", "trip_duration"), vars)) {
    dt[, paste0(dv, "_inc") := get(dv) - shift(get(dv), 1L, type = "lag"), by = id_col]
  }
  
  dt
}

pretty_stratum <- function(x) {
  dplyr::recode(
    x,
    "DRIVING" = "Driving",
    "NONDRIVING_SEDENTARY" = "Non-driving sedentary",
    .default = x
  )
}

# ============================================================
# LOCATE RUN FOLDER + RESOLUTION
# ============================================================
auto_pick_run_dir <- function(project_root, suffix_regex = "") {
  base <- file.path(project_root, "Results", "nubi_ml")
  if (!dir.exists(base)) stop("Missing folder: ", base)
  
  cand <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  cand <- cand[file.info(cand)$isdir %in% TRUE]
  
  bn <- basename(cand)
  
  keep <- grepl("compare_drive", bn, ignore.case = TRUE) &
    grepl("directRAWHR", bn, ignore.case = TRUE) &
    grepl("NOAFFINE_IMPORTANCE", bn, ignore.case = TRUE)
  
  if (!is.null(suffix_regex) && nzchar(suffix_regex)) {
    keep <- keep & grepl(suffix_regex, bn, ignore.case = TRUE)
  }
  
  cand <- cand[keep]
  
  if (length(cand) == 0) {
    stop(
      "Could not find a matching ML run folder under Results/nubi_ml/.\n",
      "Available folders:\n  ",
      paste(basename(list.dirs(base, recursive = FALSE, full.names = TRUE)), collapse = "\n  ")
    )
  }
  
  cand[which.max(file.info(cand)$mtime)]
}

if (!is.null(RUN_DIR_NAME)) {
  RUN_DIR <- file.path(project_root, "Results", "nubi_ml", RUN_DIR_NAME)
  if (!dir.exists(RUN_DIR)) {
    stop("Specified RUN_DIR_NAME does not exist: ", RUN_DIR)
  }
} else {
  RUN_DIR <- auto_pick_run_dir(project_root, RUN_DIR_SUFFIX_REGEX)
}

RUN_DIR <- normalizePath(RUN_DIR, mustWork = TRUE)
message("Using RUN_DIR: ", RUN_DIR)

bn <- basename(RUN_DIR)
m <- stringr::str_match(bn, "_(\\d+)sec_")
if (is.na(m[1, 2])) {
  stop("Could not parse resolution from RUN_DIR name: ", bn)
}
RES_SECONDS <- as.integer(m[1, 2])
message("Parsed RES_SECONDS = ", RES_SECONDS)

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
fig_subdir_name <- sprintf("%s_%dsec_Figure6_ENet_Modulators", STAMP, RES_SECONDS)
fig_out_dir <- file.path(paper_fig_root, fig_subdir_name)
dir.create(fig_out_dir, recursive = TRUE, showWarnings = FALSE)

message("Figure output directory: ", fig_out_dir)

data_path <- file.path(
  project_root, "Data",
  sprintf("NUBI_Data_%dsec_Level_MASTER_CLEAN.csv", RES_SECONDS)
)
if (!file.exists(data_path)) {
  stop("Missing final MASTER dataset: ", data_path)
}

# ============================================================
# READ MODEL OUTPUTS
# ============================================================
read_imp <- function(run_dir, kind = c("grouped", "terms"), stratum) {
  kind <- match.arg(kind)
  
  fn <- if (kind == "grouped") {
    paste0("feature_importance_grouped_", stratum, ".csv")
  } else {
    paste0("feature_importance_terms_", stratum, ".csv")
  }
  
  path <- file.path(run_dir, fn)
  if (!file.exists(path)) stop("Missing importance file: ", path)
  
  read_csv(path, show_col_types = FALSE)
}

STRATA <- c("DRIVING", "NONDRIVING_SEDENTARY")

imp_grouped_all <- bind_rows(
  read_imp(RUN_DIR, "grouped", "DRIVING") %>% mutate(stratum = "DRIVING"),
  read_imp(RUN_DIR, "grouped", "NONDRIVING_SEDENTARY") %>% mutate(stratum = "NONDRIVING_SEDENTARY")
)

imp_terms_all <- bind_rows(
  read_imp(RUN_DIR, "terms", "DRIVING") %>% mutate(stratum = "DRIVING"),
  read_imp(RUN_DIR, "terms", "NONDRIVING_SEDENTARY") %>% mutate(stratum = "NONDRIVING_SEDENTARY")
)

metrics_path <- file.path(RUN_DIR, "compare_metrics_rawhr_overall_by_stratum.csv")
if (!file.exists(metrics_path)) {
  stop("Missing metrics file: ", metrics_path)
}
metrics_raw <- read_csv(metrics_path, show_col_types = FALSE)

# ============================================================
# LOAD FINAL MASTER DATA
# ============================================================
dt <- fread(data_path, showProgress = TRUE)
dt <- canonicalize_names(dt)

req_cols <- c("p_id", "time", "raw_hr", "bl_hr")
miss <- setdiff(req_cols, names(dt))
if (length(miss) > 0) {
  stop("Missing required columns after standardization: ", paste(miss, collapse = ", "))
}

dt[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
dt <- dt[!is.na(dt_time)]

context_col <- pick_context_column(dt)
dt <- label_strata(dt, context_col)

dt <- dt[stratum_label %in% STRATA]
if (nrow(dt) == 0) {
  stop("After context labeling, no rows matched DRIVING/NONDRIVING_SEDENTARY.")
}

dt <- add_bl_hr_person(dt)

subj_drive <- unique(dt[stratum_label == "DRIVING", p_id])
subj_nond  <- unique(dt[stratum_label == "NONDRIVING_SEDENTARY", p_id])
common_subj <- intersect(subj_drive, subj_nond)

if (length(common_subj) == 0) {
  stop("No common subjects found between DRIVING and NONDRIVING_SEDENTARY.")
}

dt <- dt[p_id %in% common_subj]

dt_drive <- copy(dt[stratum_label == "DRIVING"])
dt_nond  <- copy(dt[stratum_label == "NONDRIVING_SEDENTARY"])

have_dyn <- intersect(dyn_base_vars, names(dt_drive))
if (length(have_dyn) > 0) {
  dt_drive <- add_dynamics(
    dt_drive,
    id_col = "p_id",
    time_col = "dt_time",
    vars = have_dyn,
    res_seconds = RES_SECONDS,
    windows_min = c(1, 3, 5)
  )
}

keep_common_names <- union(names(dt_drive), names(dt_nond))
for (nm in setdiff(keep_common_names, names(dt_nond))) dt_nond[, (nm) := NA]
for (nm in setdiff(keep_common_names, names(dt_drive))) dt_drive[, (nm) := NA]

dt_model <- rbindlist(list(dt_drive, dt_nond), use.names = TRUE, fill = TRUE)

# ============================================================
# DIAGNOSTIC COUNTS
# ============================================================
diag_counts <- bind_rows(
  tibble(
    stratum = "DRIVING",
    n_rows = nrow(dt_drive),
    n_subjects = data.table::uniqueN(dt_drive$p_id),
    n_features_available = ncol(dt_drive)
  ),
  tibble(
    stratum = "NONDRIVING_SEDENTARY",
    n_rows = nrow(dt_nond),
    n_subjects = data.table::uniqueN(dt_nond$p_id),
    n_features_available = ncol(dt_nond)
  )
)

# ============================================================
# PANEL A: GROUPED IMPORTANCE
# ============================================================
prep_grouped <- function(df) {
  df <- as_tibble(df)
  
  if (!("group" %in% names(df))) {
    names(df)[1] <- "group"
  }
  
  out <- df %>%
    select(stratum, group, importance) %>%
    filter(!is.na(group), is.finite(importance)) %>%
    filter(!(group %in% EXCLUDE_GROUPS)) %>%
    filter(!(stratum == "NONDRIVING_SEDENTARY" & group == "weather_info")) %>%
    mutate(group_label = pretty_term_label(group)) %>%
    group_by(stratum) %>%
    mutate(thr = max(importance, na.rm = TRUE) * REL_CUT_GROUPED) %>%
    ungroup() %>%
    filter(importance >= thr) %>%
    group_by(stratum) %>%
    slice_max(order_by = importance, n = TOPK_GROUPED, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(stratum_pretty = pretty_stratum(stratum))
  
  if (nrow(out) == 0) {
    stop("Panel A: no rows remained after grouped-importance filtering.")
  }
  
  out
}

grpA <- prep_grouped(imp_grouped_all)

grpA_plot <- bind_rows(
  grpA %>%
    filter(stratum == "DRIVING") %>%
    arrange(importance) %>%
    mutate(group_ord = factor(group_label, levels = group_label)),
  grpA %>%
    filter(stratum == "NONDRIVING_SEDENTARY") %>%
    arrange(importance) %>%
    mutate(group_ord = factor(group_label, levels = group_label))
)

pA <- ggplot(grpA_plot, aes(x = group_ord, y = importance, fill = stratum_pretty)) +
  geom_col(width = 0.75) +
  coord_flip() +
  facet_wrap(~stratum_pretty, scales = "free_y") +
  scale_fill_manual(values = c("Driving" = COL_DRIVE, "Non-driving sedentary" = COL_NOND)) +
  labs(
    title = "A. Grouped ENet importance beyond baseline+tax",
    x = NULL,
    y = "Grouped importance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ============================================================
# PANEL B: SIGNED INTERPRETABLE COEFFICIENTS
# ============================================================
prep_terms <- function(df) {
  out <- df %>%
    filter(is.finite(estimate)) %>%
    filter(str_detect(term, TERM_REGEX)) %>%
    filter(!(stratum == "NONDRIVING_SEDENTARY" & str_detect(term, "^weather_info_"))) %>%
    mutate(abscoef = abs(estimate)) %>%
    group_by(stratum) %>%
    mutate(thr = max(abscoef, na.rm = TRUE) * REL_CUT_TERMABS) %>%
    ungroup() %>%
    filter(abscoef >= thr) %>%
    group_by(stratum) %>%
    slice_max(order_by = abscoef, n = TOPK_TERMS, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      stratum_pretty = pretty_stratum(stratum),
      sign_dir = ifelse(estimate >= 0, "Positive", "Negative"),
      term_label = pretty_term_label(term)
    )
  
  if (nrow(out) == 0) {
    stop("Panel B: no interpretable term coefficients remained after filtering.")
  }
  
  out
}

termB <- prep_terms(imp_terms_all)

termB_plot <- bind_rows(
  termB %>%
    filter(stratum == "DRIVING") %>%
    arrange(abscoef) %>%
    mutate(term_ord = factor(term_label, levels = term_label)),
  termB %>%
    filter(stratum == "NONDRIVING_SEDENTARY") %>%
    arrange(abscoef) %>%
    mutate(term_ord = factor(term_label, levels = term_label))
)

pB <- ggplot(termB_plot, aes(x = term_ord, y = estimate, fill = sign_dir)) +
  geom_col(width = 0.75) +
  coord_flip() +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  facet_wrap(~stratum_pretty, scales = "free_y") +
  scale_fill_manual(
    values = c("Negative" = COL_NEG, "Positive" = COL_POS)
  ) +
  labs(
    title = "B. Signed coefficients for interpretable predictors",
    x = NULL,
    y = "Standardized coefficient"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ============================================================
# PANEL C: TOP CONTINUOUS MODULATORS
# ============================================================
pick_top_continuous <- function(stratum_name, imp_grouped, imp_terms, dt_stratum) {
  cand <- imp_grouped %>%
    as_tibble() %>%
    {
      if (!("group" %in% names(.))) rename(., group = 1) else .
    } %>%
    filter(stratum == stratum_name) %>%
    filter(!(group %in% EXCLUDE_GROUPS)) %>%
    filter(!(stratum == "NONDRIVING_SEDENTARY" & group == "weather_info")) %>%
    arrange(desc(importance)) %>%
    pull(group)
  
  drop_like <- c(
    "sex", "gender", "day_num", "days", "weather_info", "trip_time",
    "p_id", "activity", "activity3", "data_source", "stratum_label"
  )
  cand <- cand[!(cand %in% drop_like)]
  
  for (g in cand) {
    if (!(g %in% names(dt_stratum))) next
    if (!is_numericish(dt_stratum[[g]])) next
    
    beta_row <- imp_terms %>%
      filter(stratum == stratum_name, term == g)
    
    if (nrow(beta_row) == 0) next
    
    beta <- beta_row$estimate[1]
    if (!is.finite(beta) || beta == 0) next
    
    x <- suppressWarnings(as.numeric(dt_stratum[[g]]))
    x <- x[is.finite(x)]
    if (length(x) < 50) next
    if (!is.finite(sd(x)) || sd(x) == 0) next
    
    return(list(feature = g, beta = beta))
  }
  
  NULL
}

make_effect_curve <- function(dt_stratum, feature, beta, stratum_name) {
  x <- suppressWarnings(as.numeric(dt_stratum[[feature]]))
  x <- x[is.finite(x)]
  
  if (length(x) < 50) return(NULL)
  
  mu  <- mean(x)
  sdv <- sd(x)
  if (!is.finite(sdv) || sdv == 0) return(NULL)
  
  qlo <- as.numeric(quantile(x, Q_LO, na.rm = TRUE))
  qhi <- as.numeric(quantile(x, Q_HI, na.rm = TRUE))
  if (!is.finite(qlo) || !is.finite(qhi) || qlo == qhi) return(NULL)
  
  grid <- seq(qlo, qhi, length.out = N_GRID)
  effect <- beta * ((grid - mu) / sdv)
  
  tibble(
    stratum = stratum_name,
    stratum_pretty = pretty_stratum(stratum_name),
    feature = feature,
    feature_label = pretty_term_label(feature),
    x = grid,
    effect_bpm = effect
  )
}

top_drive <- pick_top_continuous(
  "DRIVING",
  imp_grouped_all,
  imp_terms_all,
  dt_drive
)

top_nond <- pick_top_continuous(
  "NONDRIVING_SEDENTARY",
  imp_grouped_all,
  imp_terms_all,
  dt_nond
)

panelC_selected <- bind_rows(
  if (!is.null(top_drive)) tibble(stratum = "DRIVING", feature = top_drive$feature, beta = top_drive$beta),
  if (!is.null(top_nond))  tibble(stratum = "NONDRIVING_SEDENTARY", feature = top_nond$feature, beta = top_nond$beta)
)

message("Panel C top continuous modulator (DRIVING): ",
        if (!is.null(top_drive)) top_drive$feature else "NONE")
message("Panel C top continuous modulator (NONDRIVING_SEDENTARY): ",
        if (!is.null(top_nond)) top_nond$feature else "NONE")

curve_list <- list()
if (!is.null(top_drive)) {
  curve_list[[length(curve_list) + 1]] <- make_effect_curve(
    dt_drive, top_drive$feature, top_drive$beta, "DRIVING"
  )
}
if (!is.null(top_nond)) {
  curve_list[[length(curve_list) + 1]] <- make_effect_curve(
    dt_nond, top_nond$feature, top_nond$beta, "NONDRIVING_SEDENTARY"
  )
}
curveC <- bind_rows(curve_list)

if (nrow(curveC) == 0) {
  pC <- ggplot() +
    geom_blank() +
    labs(
      title = "C. Top continuous modulators",
      subtitle = "No suitable continuous modulator found in one or both strata"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold")
    )
} else {
  curveC <- curveC %>%
    mutate(panel = paste0(stratum_pretty, ": ", feature_label))
  
  pC <- ggplot(
    curveC,
    aes(x = x, y = effect_bpm, color = stratum_pretty)
  ) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    facet_wrap(~panel, scales = "free_x") +
    scale_color_manual(values = c("Driving" = COL_DRIVE, "Non-driving sedentary" = COL_NOND)) +
    labs(
      title = "C. Top continuous modulators",
      x = "Feature value (5th-95th percentile range)",
      y = expression(Delta * "Predicted HR [bpm]")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

# ============================================================
# PANEL D: ENET GAIN BEYOND BASELINE+OFFSET
# ============================================================
get_metric <- function(metrics_df, model_name, metric_name, stratum_name) {
  metrics_df %>%
    filter(
      .data$model == model_name,
      .data$.metric == metric_name,
      .data$stratum == stratum_name
    ) %>%
    summarise(v = mean(.data$.estimate, na.rm = TRUE), .groups = "drop") %>%
    pull(v)
}

gain_rows <- list()
for (s in STRATA) {
  rmse_b <- get_metric(metrics_raw, "baseline_offset", "rmse", s)
  rmse_e <- get_metric(metrics_raw, "enet",            "rmse", s)
  
  rsq_b  <- get_metric(metrics_raw, "baseline_offset", "rsq",  s)
  rsq_e  <- get_metric(metrics_raw, "enet",            "rsq",  s)
  
  gain_rows[[length(gain_rows) + 1]] <- tibble(
    stratum = s,
    stratum_pretty = pretty_stratum(s),
    metric = "d_rmse",
    delta = rmse_b - rmse_e
  )
  
  gain_rows[[length(gain_rows) + 1]] <- tibble(
    stratum = s,
    stratum_pretty = pretty_stratum(s),
    metric = "d_rsq",
    delta = rsq_e - rsq_b
  )
}

gainD <- bind_rows(gain_rows) %>%
  filter(is.finite(delta))

if (nrow(gainD) == 0) {
  stop(
    "Panel D has zero rows after computing deltas.\n",
    "Check compare_metrics_rawhr_overall_by_stratum.csv for:\n",
    "  model in {baseline_offset, enet}\n",
    "  .metric in {rmse, rsq}\n",
    "  stratum in {DRIVING, NONDRIVING_SEDENTARY}"
  )
}

pD <- ggplot(
  gainD,
  aes(x = stratum_pretty, y = delta, fill = stratum_pretty)
) +
  geom_col(width = 0.7) +
  facet_wrap(
    ~metric,
    scales = "free_y",
    labeller = as_labeller(c(
      d_rmse = "\u0394RMSE [bpm]",
      d_rsq  = "\u0394R\u00b2"
    ))
  ) +
  scale_fill_manual(values = c("Driving" = COL_DRIVE, "Non-driving sedentary" = COL_NOND)) +
  labs(
    title = "D. ENet gain beyond baseline+tax",
    x = NULL,
    y = "Improvement (positive = better)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ============================================================
# COMPOSE FIGURE
# ============================================================
fig7 <- (pA | pB) / (pC | pD)

out_pdf <- file.path(fig_out_dir, "Figure6_ENet_Modulators.pdf")
out_png <- file.path(fig_out_dir, "Figure6_ENet_Modulators.png")

ggsave(out_pdf, fig7, width = 14, height = 10, device = grDevices::cairo_pdf)
ggsave(out_png, fig7, width = 14, height = 10, dpi = 300)

# ============================================================
# WRITE DIAGNOSTICS
# ============================================================
write_csv(diag_counts, file.path(fig_out_dir, "diag_counts_by_stratum.csv"))
write_csv(as_tibble(grpA), file.path(fig_out_dir, "diag_panelA_grouped_features.csv"))
write_csv(as_tibble(termB), file.path(fig_out_dir, "diag_panelB_terms.csv"))
write_csv(as_tibble(panelC_selected), file.path(fig_out_dir, "diag_panelC_selected_modulators.csv"))
write_csv(as_tibble(curveC), file.path(fig_out_dir, "diag_panelC_effect_curves.csv"))
write_csv(as_tibble(gainD), file.path(fig_out_dir, "diag_panelD_incremental_gain.csv"))

diag_lines <- c(
  "Figure 6 diagnostics summary",
  "============================",
  "",
  paste0("Timestamp: ", STAMP),
  paste0("RUN_DIR: ", RUN_DIR),
  paste0("RES_SECONDS: ", RES_SECONDS),
  paste0("Data file: ", data_path),
  paste0("Output directory: ", fig_out_dir),
  "",
  "Counts by stratum:"
)

for (i in seq_len(nrow(diag_counts))) {
  diag_lines <- c(
    diag_lines,
    paste0(
      "  - ", diag_counts$stratum[i],
      ": n_rows=", diag_counts$n_rows[i],
      ", n_subjects=", diag_counts$n_subjects[i],
      ", n_features_available=", diag_counts$n_features_available[i]
    )
  )
}

diag_lines <- c(
  diag_lines,
  "",
  paste0("Panel A selected grouped rows: ", nrow(grpA)),
  paste0("Panel B selected term rows: ", nrow(termB)),
  paste0("Panel C selected modulators: ", nrow(panelC_selected)),
  paste0("Panel C effect-curve rows: ", nrow(curveC)),
  paste0("Panel D gain rows: ", nrow(gainD)),
  "",
  paste0("Panel C top modulator (DRIVING): ",
         if (!is.null(top_drive)) top_drive$feature else "NONE"),
  paste0("Panel C top modulator (NONDRIVING_SEDENTARY): ",
         if (!is.null(top_nond)) top_nond$feature else "NONE")
)

writeLines(diag_lines, con = file.path(fig_out_dir, "diagnostics_summary.txt"))

message("Wrote: ", out_pdf)
message("Wrote: ", out_png)
message("Diagnostics written under: ", fig_out_dir)
message("DONE.")