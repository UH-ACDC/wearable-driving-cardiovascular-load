# ============================================================
# Figure7_ENetHorizon.R  (FULL REPLACEMENT)
#
# Figure 7 – ENet Horizon Simulation: Cumulative Cardiovascular Exposure
#
# PANEL A
#   Weather contrast (ENet, observed DRIVING rows only):
#   Clouds vs Adverse weather
#   NOTE:
#     In the DRIVING stratum, weather_info == "other" is relabeled
#     as "Adverse weather" for the figure.
#
# PANEL B
#   Trait-anxiety extremes (ENet, matched typical driving profile):
#   Lowest vs Highest trait anxiety
#
# CORE IDEA
#   Small per-unit differences in predicted NHR can accumulate into
#   substantial annual cardiovascular burden as daily driving duration rises.
#
# DATA SOURCE
#   Uses DRIVING rows from:
#     Data/NUBI_Data_<RES>sec_Level_MASTER_CLEAN.csv
#
# MODEL
#   - reads DRIVING artifacts from the selected nubi_ml run folder:
#       predictor_list_DRIVING.csv
#       best_params_DRIVING.csv
#   - refits ENet on FULL DRIVING data
#   - predicts RAW_HR_hat
#   - converts to NHR_hat = RAW_HR_hat - bl_hr_person
#
# OUTPUTS
#   Results/paper_figs/<timestamp>_<RES>sec_Figure7_ENetHorizon_Weather_and_TraitTypical/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(stringr)
  
  library(tidymodels)
  library(dplyr)
  library(tidyr)
  library(readr)
  
  library(ggplot2)
  library(scales)
  library(patchwork)
})

options(warn = 1)
tidymodels_prefer()
set.seed(20260309)

# ----------------------------
# User toggles
# ----------------------------
LOCAL_TZ <- "America/Chicago"
DRIVING_LABELS <- c("driving")

# Panel A: weather labels in DRIVING stratum
WEATHER_LABEL_CLOUDS_CANDIDATES  <- c("clouds", "cloudy")
WEATHER_LABEL_ADVERSE_CANDIDATES <- c("other")

WEATHER_LABEL_CLOUDS_FIG  <- "Clouds"
WEATHER_LABEL_ADVERSE_FIG <- "Adverse weather"

MIN_WEATHER_SUBJ <- 3
MIN_WEATHER_ROWS <- 20

# Panel B: preferred trait variable
TRAIT_VAR_CANDIDATES <- c(
  "trait_anxiety",
  "stai_trait",
  "traitanxiety",
  "stai_trait_total"
)

TRAIT_LABEL_LOW  <- "Lowest trait anxiety"
TRAIT_LABEL_HIGH <- "Highest trait anxiety"

# For a cleaner trait-only counterfactual, hold baseline fixed
USE_COMMON_BASELINE_FOR_TRAIT_PANEL <- TRUE

# Annualization
DAYS_PER_YEAR <- 365
SCENARIOS_HOURS_PER_DAY <- c(
  "30 min/day commute"           = 0.5,
  "2 hr/day commute"             = 2.0,
  "8 hr/day professional driver" = 8.0
)

# Simulation
N_SIM <- 20000

# Colors
PAL_WEATHER <- c(
  "Clouds"          = "#4DD3D3",
  "Adverse weather" = "#F4A3A3"
)
PAL_TRAIT <- c(
  "Lowest trait anxiety"  = "#4DD3D3",
  "Highest trait anxiety" = "#F4A3A3"
)

# Output sizing
PDF_W <- 11.3
PDF_H <- 7.8
PNG_DPI <- 300

# Auto-pick latest run folder for this resolution
AUTO_PICK_LATEST_RUN <- TRUE

# ----------------------------
# Robust wd = Scripts/
# ----------------------------
this_script <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
if (!is.na(this_script) && file.exists(this_script)) setwd(dirname(this_script))
message("Working directory (Scripts): ", getwd())
project_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)

# ----------------------------
# Resolution picker
# ----------------------------
pick_resolution <- function() {
  cat("\nChoose dataset resolution:\n",
      "  1) 10 sec\n",
      "  2) 30 sec\n",
      "  3) 60 sec\n", sep = "")
  ans <- trimws(readline("Enter 10 / 30 / 60 (or 1/2/3): "))
  if (ans %in% c("1", "10")) return(10L)
  if (ans %in% c("2", "30")) return(30L)
  if (ans %in% c("3", "60")) return(60L)
  stop("Invalid entry: ", ans)
}
RES_SECONDS <- pick_resolution()

# ----------------------------
# Input dataset
# ----------------------------
in_path <- file.path(
  project_root, "Data",
  sprintf("NUBI_Data_%dsec_Level_MASTER_CLEAN.csv", RES_SECONDS)
)
stopifnot(file.exists(in_path))

# ----------------------------
# Locate run folder
# ----------------------------
nubi_ml_root <- file.path(project_root, "Results", "nubi_ml")
stopifnot(dir.exists(nubi_ml_root))

extract_version_num <- function(x) {
  x_low <- tolower(x)
  m <- stringr::str_match(x_low, "v([0-9]+)")
  out <- suppressWarnings(as.numeric(m[, 2]))
  ifelse(is.na(out), -Inf, out)
}

pick_latest_run_folder <- function(res_seconds) {
  all_dirs <- list.dirs(nubi_ml_root, full.names = FALSE, recursive = FALSE)
  
  res_pat    <- paste0("(^|[^0-9])", res_seconds, "sec([^0-9]|$)")
  drive_pat  <- "compare_drive_vs_nondrive_directRAWHR"
  affine_pat <- "NOAFFINE_IMPORTANCE$"
  
  cand <- all_dirs[
    grepl(res_pat, all_dirs, ignore.case = TRUE, perl = TRUE) &
      grepl(drive_pat, all_dirs, ignore.case = TRUE, perl = TRUE) &
      grepl(affine_pat, all_dirs, ignore.case = TRUE, perl = TRUE)
  ]
  
  if (length(cand) == 0) return(NA_character_)
  
  rank_df <- data.frame(
    run_folder  = cand,
    version_num = extract_version_num(cand),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(version_num), desc(run_folder))
  
  rank_df$run_folder[1]
}

run_folder <- NA_character_
if (isTRUE(AUTO_PICK_LATEST_RUN)) {
  run_folder <- pick_latest_run_folder(RES_SECONDS)
  if (!is.na(run_folder)) message("AUTO_PICK_LATEST_RUN selected: ", run_folder)
}
if (is.na(run_folder)) {
  run_folder <- trimws(readline("RUN folder under Results/nubi_ml/: "))
  if (run_folder == "") stop("No RUN folder provided.")
}

run_dir <- file.path(nubi_ml_root, run_folder)
stopifnot(dir.exists(run_dir))

pred_list_path <- file.path(run_dir, "predictor_list_DRIVING.csv")
best_path      <- file.path(run_dir, "best_params_DRIVING.csv")
stopifnot(file.exists(pred_list_path))
stopifnot(file.exists(best_path))

# ----------------------------
# Output folder
# ----------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(
  project_root, "Results", "paper_figs",
  paste0(stamp, "_", RES_SECONDS, "sec_Figure7_ENetHorizon_Weather_and_TraitTypical")
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

safe_save_png <- function(plot_obj, path, w = 7, h = 5, dpi = 300) {
  ggsave(filename = path, plot = plot_obj, width = w, height = h, dpi = dpi)
}

log_msg("Figure7 start | RES=", RES_SECONDS, "sec | N_SIM=", N_SIM)
log_msg("Input: ", in_path)
log_msg("Run dir: ", run_dir)
log_msg("Output: ", out_dir)

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

norm_ds <- function(x) trimws(tolower(as.character(x)))
norm_label <- function(x) trimws(tolower(as.character(x)))

parse_time_local <- function(x, tz = LOCAL_TZ) {
  if (inherits(x, "POSIXt")) return(with_tz(x, tzone = tz))
  if (!is.character(x)) x <- as.character(x)
  tt <- suppressWarnings(lubridate::ymd_hms(x, tz = tz, quiet = TRUE))
  if (!all(is.na(tt))) return(tt)
  suppressWarnings(lubridate::parse_date_time(
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
    p_id             = "p_id",
    pid              = "p_id",
    participant_id   = "p_id",
    participantid    = "p_id",
    
    time             = "time",
    timestamp        = "time",
    datetime         = "time",
    date_time        = "time",
    
    dt_time          = "dt_time",
    raw_hr           = "raw_hr",
    
    activity         = "activity",
    data_source      = "activity",
    datasource       = "activity",
    source           = "activity",
    
    activity3        = "activity3",
    
    bl_hr            = "bl_hr",
    hr_bl            = "bl_hr",
    hrbl             = "bl_hr",
    baseline         = "bl_hr",
    
    day_num          = "day_num",
    daynum           = "day_num",
    days             = "days",
    
    speed            = "speed",
    weather_info     = "weather_info",
    weather          = "weather_info",
    
    trait_anxiety    = "trait_anxiety",
    traitanxiety     = "trait_anxiety",
    stai_trait       = "stai_trait",
    stai_trait_total = "stai_trait_total"
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
    d <- as.Date(dt[["dt_time"]], tz = LOCAL_TZ)
    u <- sort(unique(d))
    return(as.integer(match(d, u)))
  }
  
  NULL
}

boot_mu_subjects <- function(df_subj, B) {
  ids <- unique(df_subj$p_id)
  if (length(ids) < 3) stop("Too few subjects for subject bootstrap in this condition.")
  replicate(B, {
    samp <- sample(ids, size = length(ids), replace = TRUE)
    mean(df_subj$mu_i[df_subj$p_id %in% samp], na.rm = TRUE)
  })
}

simulate_exposure <- function(mu_draws, sigma_bpm, scenarios_tbl) {
  bind_rows(lapply(seq_len(nrow(scenarios_tbl)), function(j) {
    H <- scenarios_tbl$H_year[j]
    mean_E <- mu_draws * H
    sd_E   <- sigma_bpm * sqrt(H / 60)
    tibble(
      scenario = scenarios_tbl$scenario[j],
      sim = seq_along(mu_draws),
      exposure_nhr_hours = rnorm(length(mu_draws), mean = mean_E, sd = sd_E)
    )
  }))
}

mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

make_typical_row <- function(df_train) {
  nd <- df_train[1, , drop = FALSE]
  
  for (nm in names(nd)) {
    if (nm %in% c("raw_hr")) next
    if (nm == "p_id") next
    
    v <- df_train[[nm]]
    
    if (is.numeric(v) || is.integer(v)) {
      nd[[nm]] <- as.numeric(stats::median(v, na.rm = TRUE))
    } else if (is.factor(v)) {
      vv <- as.character(v)
      vv <- vv[!is.na(vv)]
      if (length(vv) == 0) {
        nd[[nm]] <- factor(NA, levels = levels(v))
      } else {
        nd[[nm]] <- factor(mode1(vv), levels = levels(v))
      }
    } else if (is.character(v)) {
      vv <- v[!is.na(v) & nzchar(v)]
      nd[[nm]] <- if (length(vv) == 0) NA_character_ else mode1(vv)
    } else if (is.logical(v)) {
      vv <- v[!is.na(v)]
      nd[[nm]] <- if (length(vv) == 0) NA else as.logical(mode1(vv))
    } else {
      idx <- which(!is.na(v))[1]
      nd[[nm]] <- if (length(idx) == 0) NA else v[idx]
    }
  }
  
  nd
}

find_trait_var <- function(all_names, candidates) {
  hit <- candidates[candidates %in% all_names]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

find_present_label <- function(x, candidates) {
  xn <- norm_label(x)
  cand <- norm_label(candidates)
  hit <- cand[cand %in% unique(xn)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ============================================================
# Read predictors + best params
# ============================================================
pred_list <- readr::read_csv(pred_list_path, show_col_types = FALSE)
preds_drive <- unique(pred_list$predictor[!is.na(pred_list$predictor) & nzchar(pred_list$predictor)])

best_df <- readr::read_csv(best_path, show_col_types = FALSE)
penalty_val <- as.numeric(best_df$penalty[1])
mixture_val <- as.numeric(best_df$mixture[1])

if (!is.finite(penalty_val) || !is.finite(mixture_val)) {
  stop("Invalid best params in best_params_DRIVING.csv")
}

write_csv(
  tibble(
    RES_SECONDS = RES_SECONDS,
    input_file  = basename(in_path),
    run_folder  = run_folder,
    penalty     = penalty_val,
    mixture     = mixture_val
  ),
  file.path(out_dir, "Figure7_enet_params_used.csv")
)

# ============================================================
# Read data + build bl_hr_person + driving df
# ============================================================
dt <- fread(in_path, showProgress = TRUE)
dt <- canonicalize_names(dt)

need <- c("p_id", "time", "raw_hr", "activity", "bl_hr", "weather_info")
miss <- setdiff(need, names(dt))
if (length(miss) > 0) {
  stop("Missing required columns: ", paste(miss, collapse = ", "))
}

if (!("dt_time" %in% names(dt)) || !inherits(dt$dt_time, "POSIXt")) {
  dt[, dt_time := parse_time_local(time, tz = LOCAL_TZ)]
}
dt <- dt[!is.na(dt_time)]

dt[, activity_norm := norm_ds(activity)]
dt[, raw_hr_num    := suppressWarnings(as.numeric(raw_hr))]
dt[, bl_hr_num     := suppressWarnings(as.numeric(bl_hr))]
dt[, weather_norm  := norm_label(weather_info)]

dt <- dt[is.finite(raw_hr_num)]

day_key <- make_day_key(dt)
if (is.null(day_key)) stop("Could not construct day key.")
dt[, day_key := day_key]

baseline_by_subj_day <- dt[
  is.finite(bl_hr_num) & !is.na(day_key),
  .(bl_hr_day = median(bl_hr_num, na.rm = TRUE)),
  by = .(p_id, day_key)
]
if (nrow(baseline_by_subj_day) == 0) {
  stop("baseline_by_subj_day is empty; check bl_hr.")
}

baseline_by_subj <- baseline_by_subj_day[
  , .(bl_hr_person = mean(bl_hr_day, na.rm = TRUE)),
  by = .(p_id)
]

dt <- merge(dt, baseline_by_subj, by = "p_id", all.x = TRUE)
dt <- dt[is.finite(bl_hr_person)]

dt_drive <- dt[activity_norm %in% DRIVING_LABELS]
if (nrow(dt_drive) == 0) stop("No DRIVING rows found.")

log_msg("Driving rows: ", nrow(dt_drive), " | subjects: ", uniqueN(dt_drive$p_id))

# ============================================================
# Build model frame
# ============================================================
preds_drive_use <- intersect(preds_drive, names(dt_drive))

trait_candidates_present <- intersect(TRAIT_VAR_CANDIDATES, names(dt_drive))

use_cols <- unique(c(
  "p_id",
  "raw_hr_num",
  "bl_hr_person",
  "weather_info",
  trait_candidates_present,
  preds_drive_use,
  "day_key"
))
use_cols <- intersect(use_cols, names(dt_drive))

df <- dt_drive[, ..use_cols] |> as.data.frame()
df$p_id <- as.factor(df$p_id)
df$raw_hr <- df$raw_hr_num
df$raw_hr_num <- NULL

trait_var <- find_trait_var(names(df), TRAIT_VAR_CANDIDATES)
if (is.na(trait_var)) {
  stop(
    "Could not find a trait variable in model frame. Tried: ",
    paste(TRAIT_VAR_CANDIDATES, collapse = ", "),
    ". Add the desired variable to predictor_list_DRIVING.csv and/or TRAIT_VAR_CANDIDATES."
  )
}
log_msg("Trait variable used: ", trait_var)

if (!("weather_info" %in% names(df))) {
  stop("weather_info is not available in the model frame. Ensure predictor_list_DRIVING.csv includes weather_info.")
}

# ============================================================
# Fit ENet on full DRIVING data
# ============================================================
rec <- recipe(raw_hr ~ ., data = df) %>%
  update_role(p_id, new_role = "id") %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors(), new_level = "Unknown") %>%
  step_novel(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

enet_spec <- linear_reg(
  penalty = penalty_val,
  mixture = mixture_val
) %>% set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(enet_spec)

log_msg("Fitting ENet on FULL DRIVING data ...")
fit_full <- fit(wf, data = df)

predict_nhr_hat <- function(fitted_wf, new_df) {
  nd <- new_df
  if (!("raw_hr" %in% names(nd))) nd$raw_hr <- NA_real_
  pr <- predict(fitted_wf, new_data = nd)
  pr$.pred - nd$bl_hr_person
}

scenarios_tbl <- tibble(
  scenario = names(SCENARIOS_HOURS_PER_DAY),
  hours_per_day = as.numeric(SCENARIOS_HOURS_PER_DAY)
) %>%
  mutate(H_year = hours_per_day * DAYS_PER_YEAR)

# ============================================================
# Panel A: weather contrast (observed driving rows only)
# ============================================================
weather_raw  <- as.character(df$weather_info)
weather_norm <- norm_label(weather_raw)

clouds_label_present  <- find_present_label(weather_raw, WEATHER_LABEL_CLOUDS_CANDIDATES)
adverse_label_present <- find_present_label(weather_raw, WEATHER_LABEL_ADVERSE_CANDIDATES)

if (is.na(clouds_label_present) || is.na(adverse_label_present)) {
  stop(
    "Could not find required DRIVING weather labels. Present normalized labels include: ",
    paste(sort(unique(weather_norm)), collapse = ", "),
    ". Expected clouds-like and other-like labels."
  )
}

df_w <- df %>%
  mutate(
    weather_norm = norm_label(weather_info),
    cond_weather = case_when(
      weather_norm == clouds_label_present  ~ WEATHER_LABEL_CLOUDS_FIG,
      weather_norm == adverse_label_present ~ WEATHER_LABEL_ADVERSE_FIG,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cond_weather))

if (nrow(df_w) == 0) {
  stop("Panel A weather subset is empty after selecting clouds and adverse weather.")
}

weather_audit <- df_w %>%
  group_by(cond_weather) %>%
  summarise(
    n_rows = n(),
    n_subj = n_distinct(p_id),
    .groups = "drop"
  )
write_csv(weather_audit, file.path(out_dir, "figure7_weather_split_audit.csv"))

if (any(weather_audit$n_rows < MIN_WEATHER_ROWS) || any(weather_audit$n_subj < MIN_WEATHER_SUBJ)) {
  stop(
    "Panel A weather contrast has too little support. Audit:\n",
    paste(capture.output(print(weather_audit)), collapse = "\n")
  )
}

write_csv(
  tibble(
    weather_label_clouds_in_data    = clouds_label_present,
    weather_label_adverse_in_data   = adverse_label_present,
    weather_label_clouds_in_figure  = WEATHER_LABEL_CLOUDS_FIG,
    weather_label_adverse_in_figure = WEATHER_LABEL_ADVERSE_FIG
  ),
  file.path(out_dir, "figure7_weather_labels_used.csv")
)

df_w$nhat <- predict_nhr_hat(fit_full, df_w)

subj_mu_weather <- df_w %>%
  group_by(p_id, cond_weather) %>%
  summarise(mu_i = mean(nhat, na.rm = TRUE), .groups = "drop")

mu_draws_clouds <- boot_mu_subjects(
  subj_mu_weather %>% filter(cond_weather == WEATHER_LABEL_CLOUDS_FIG),
  B = N_SIM
)
mu_draws_adverse <- boot_mu_subjects(
  subj_mu_weather %>% filter(cond_weather == WEATHER_LABEL_ADVERSE_FIG),
  B = N_SIM
)

sigma_clouds  <- sd(df_w$nhat[df_w$cond_weather == WEATHER_LABEL_CLOUDS_FIG], na.rm = TRUE)
sigma_adverse <- sd(df_w$nhat[df_w$cond_weather == WEATHER_LABEL_ADVERSE_FIG], na.rm = TRUE)

write_csv(
  tibble(sim = seq_len(N_SIM), mu_low = mu_draws_clouds, mu_high = mu_draws_adverse),
  file.path(out_dir, "figure7_panelA_weather_boot_mu_draws.csv")
)

sim_weather <- bind_rows(
  simulate_exposure(mu_draws_clouds, sigma_clouds, scenarios_tbl) %>%
    mutate(panel = "A", condition = WEATHER_LABEL_CLOUDS_FIG),
  simulate_exposure(mu_draws_adverse, sigma_adverse, scenarios_tbl) %>%
    mutate(panel = "A", condition = WEATHER_LABEL_ADVERSE_FIG)
)

sum_weather <- sim_weather %>%
  group_by(panel, scenario, condition) %>%
  summarise(
    mean = mean(exposure_nhr_hours),
    q05  = as.numeric(quantile(exposure_nhr_hours, 0.05)),
    q95  = as.numeric(quantile(exposure_nhr_hours, 0.95)),
    .groups = "drop"
  ) %>%
  mutate(scenario = factor(scenario, levels = names(SCENARIOS_HOURS_PER_DAY)))

write_csv(sum_weather, file.path(out_dir, "figure7_panelA_weather_sim_summary.csv"))

# ============================================================
# Panel B: trait anxiety extremes (matched typical driving profile)
# ============================================================
trait_vals <- suppressWarnings(as.numeric(df[[trait_var]]))
if (all(!is.finite(trait_vals))) {
  stop("Trait variable found but has no finite numeric values: ", trait_var)
}
df[[trait_var]] <- trait_vals

trait_by_subj <- df %>%
  filter(is.finite(.data[[trait_var]])) %>%
  group_by(p_id) %>%
  summarise(
    trait_value  = median(.data[[trait_var]], na.rm = TRUE),
    bl_hr_person = median(bl_hr_person, na.rm = TRUE),
    n_rows_drive = n(),
    n_days_drive = n_distinct(day_key),
    .groups = "drop"
  ) %>%
  arrange(trait_value)

if (nrow(trait_by_subj) < 2) {
  stop("Too few subjects with finite trait values for Panel B.")
}

pid_low_trait    <- trait_by_subj$p_id[1]
pid_high_trait   <- trait_by_subj$p_id[nrow(trait_by_subj)]
trait_low_value  <- trait_by_subj$trait_value[1]
trait_high_value <- trait_by_subj$trait_value[nrow(trait_by_subj)]
bl_low_trait     <- trait_by_subj$bl_hr_person[1]
bl_high_trait    <- trait_by_subj$bl_hr_person[nrow(trait_by_subj)]

typ_row <- make_typical_row(df)
typ_row$p_id <- factor("SYN", levels = c(levels(df$p_id), "SYN"))

typ_export <- typ_row
typ_export$p_id <- as.character(typ_export$p_id)
write_csv(as.data.frame(typ_export), file.path(out_dir, "figure7_typical_profile_used.csv"))

row_low_trait  <- typ_row
row_high_trait <- typ_row

row_low_trait[[trait_var]]  <- trait_low_value
row_high_trait[[trait_var]] <- trait_high_value

if (isTRUE(USE_COMMON_BASELINE_FOR_TRAIT_PANEL)) {
  common_bl <- median(df$bl_hr_person, na.rm = TRUE)
  row_low_trait$bl_hr_person  <- common_bl
  row_high_trait$bl_hr_person <- common_bl
} else {
  row_low_trait$bl_hr_person  <- bl_low_trait
  row_high_trait$bl_hr_person <- bl_high_trait
}

mu_low_B  <- as.numeric(predict_nhr_hat(fit_full, row_low_trait))
mu_high_B <- as.numeric(predict_nhr_hat(fit_full, row_high_trait))

nhat_all <- predict_nhr_hat(fit_full, df)
sigma_common <- sd(nhat_all, na.rm = TRUE)

panelB_params <- tibble(
  label         = c(TRAIT_LABEL_LOW, TRAIT_LABEL_HIGH),
  p_id          = c(as.character(pid_low_trait), as.character(pid_high_trait)),
  trait_var     = trait_var,
  trait_value   = c(trait_low_value, trait_high_value),
  baseline_mode = if (isTRUE(USE_COMMON_BASELINE_FOR_TRAIT_PANEL)) "common_median" else "subject_specific",
  bl_hr_person  = c(
    if (isTRUE(USE_COMMON_BASELINE_FOR_TRAIT_PANEL)) median(df$bl_hr_person, na.rm = TRUE) else bl_low_trait,
    if (isTRUE(USE_COMMON_BASELINE_FOR_TRAIT_PANEL)) median(df$bl_hr_person, na.rm = TRUE) else bl_high_trait
  ),
  typical_weather = if ("weather_info" %in% names(typ_row)) as.character(typ_row$weather_info) else NA_character_,
  mu_nhrhat     = c(mu_low_B, mu_high_B),
  sigma_used    = sigma_common
)
write_csv(panelB_params, file.path(out_dir, "figure7_panelB_trait_params.csv"))

mu_draws_lowB  <- rep(mu_low_B,  N_SIM)
mu_draws_highB <- rep(mu_high_B, N_SIM)

sim_trait <- bind_rows(
  simulate_exposure(mu_draws_lowB, sigma_common, scenarios_tbl) %>%
    mutate(panel = "B", condition = TRAIT_LABEL_LOW),
  simulate_exposure(mu_draws_highB, sigma_common, scenarios_tbl) %>%
    mutate(panel = "B", condition = TRAIT_LABEL_HIGH)
)

sum_trait <- sim_trait %>%
  group_by(panel, scenario, condition) %>%
  summarise(
    mean = mean(exposure_nhr_hours),
    q05  = as.numeric(quantile(exposure_nhr_hours, 0.05)),
    q95  = as.numeric(quantile(exposure_nhr_hours, 0.95)),
    .groups = "drop"
  ) %>%
  mutate(scenario = factor(scenario, levels = names(SCENARIOS_HOURS_PER_DAY)))

write_csv(sum_trait, file.path(out_dir, "figure7_panelB_trait_sim_summary.csv"))

# ============================================================
# Plot
# ============================================================
plot_panel <- function(sum_df, pal, title_text) {
  ggplot(sum_df, aes(x = scenario, y = mean, fill = condition)) +
    geom_col(
      position = position_dodge(width = 0.75),
      width = 0.68,
      color = "black",
      linewidth = 0.25
    ) +
    geom_errorbar(
      aes(ymin = q05, ymax = q95),
      position = position_dodge(width = 0.75),
      width = 0.18,
      linewidth = 0.7
    ) +
    scale_fill_manual(values = pal) +
    labs(
      title = title_text,
      x = NULL,
      y = "Annual cumulative NHR-hours [bpm·hours]",
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "plain", size = 13),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 12, hjust = 1)
    )
}

pA <- plot_panel(
  sum_weather %>% mutate(condition = factor(condition, levels = names(PAL_WEATHER))),
  PAL_WEATHER,
  "A. Driving weather: Clouds vs Adverse weather"
)

pB <- plot_panel(
  sum_trait %>% mutate(condition = factor(condition, levels = names(PAL_TRAIT))),
  PAL_TRAIT,
  "B. Trait anxiety: low vs high"
)

fig7 <- pA / pB +
  plot_annotation(
    title = NULL
  )

pdf_path <- file.path(fig_dir, "Figure7_ENetHorizon_Weather_and_TraitTypical.pdf")
png_path <- file.path(fig_dir, "Figure7_ENetHorizon_Weather_and_TraitTypical.png")

safe_save_pdf(fig7, pdf_path, w = PDF_W, h = PDF_H)
safe_save_png(fig7, png_path, w = PDF_W, h = PDF_H, dpi = PNG_DPI)

log_msg("Saved: ", pdf_path)
log_msg("Saved: ", png_path)
log_msg("DONE. Outputs in: ", out_dir)