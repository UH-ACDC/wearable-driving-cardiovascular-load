# npj Digital Medicine HR Prediction

This repository contains the analysis code and curated data products for the manuscript:

**From Instantaneous Heart Rate to Long-Horizon Cardiovascular Burden in Naturalistic Daily Life**

The project analyzes heart-rate dynamics in naturalistic daily life, with emphasis on driving and non-driving sedentary contexts. It uses curated participant-level wearable and contextual features to evaluate baseline-referenced cardiovascular load, machine-learning prediction of raw heart rate, context-level cardiovascular tax, and long-horizon cumulative exposure.

---

## Repository scope

This public repository starts from:

1. A final clean MASTER dataset.
2. Analysis scripts for generating curated machine-learning outputs used by the manuscript figures.
3. Figure-generation scripts for the manuscript and supplement.

It does **not** rebuild the full upstream processing pipeline from raw wearable, smartphone, vehicle, GPS, weather, or ground-truth streams.

It does **not** include generated results. The `Results/` directory is intentionally excluded from the repository. Users should run the scripts locally to regenerate model outputs, figures, diagnostics, and manuscript-ready plots.

The repository is intended for manuscript reproducibility from the curated final dataset, not for raw-data reconstruction.

---

## Repository structure

```text
.
├── Data/
│   └── NUBI_Data_60sec_Level_MASTER_CLEAN.csv
│
├── Scripts/
│   ├── 00_run_predictive_decomposition.R
│   ├── 01_figure1_participant_level_timeseries.R
│   ├── 02_figure2_baseline_and_tax.R
│   ├── 03_figure3_model_performance.R
│   ├── 04_figure4_context_tax.R
│   ├── 05_figure5_prediction_decomposition.R
│   ├── 06_figure6_enet_modulators.R
│   ├── 07_figure7_enet_horizon_simulation.R
│   ├── 08_figure8_horizon_scaling_rawhr.R
│   └── Fig_Supplement_Activity3_MultiRes.R
│
├── DATA_USE.md
├── LICENSE
├── README.md
├── .gitignore
└── .gitattributes
```

Generated outputs are written locally under:

```text
Results/
```

The `Results/` folder is not included in the repository.

The scripts listed above reflect the manuscript-ready public-release naming.

---

## Data

The primary public-repository dataset is:

```text
Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
```

Some scripts can also support 10-sec or 30-sec versions if matching files are available locally:

```text
Data/NUBI_Data_10sec_Level_MASTER_CLEAN.csv
Data/NUBI_Data_30sec_Level_MASTER_CLEAN.csv
Data/NUBI_Data_60sec_Level_MASTER_CLEAN.csv
```

The 60-sec dataset is the manuscript/public-repository default.

The dataset contains curated participant-level time-series features, including heart rate, baseline heart rate, activity labels, weather/context variables, and psychometric/workload variables.

Expected core columns include:

```text
p_id
time
day_num
activity
activity3
bl_hr
raw_hr
weather_info
trait_anxiety
age
gender
md
pd
td
p
e
f
```

Column names are canonicalized inside the scripts where needed, so minor naming variants are handled defensively.

Use of the curated dataset is governed by [`DATA_USE.md`](DATA_USE.md).

---

## Generated results

The repository intentionally does **not** include:

```text
Results/
```

All model outputs, figure outputs, diagnostics, and manuscript-ready plots are regenerated locally. The scripts create the required `Results/` subdirectories as needed.

The main generated locations are:

```text
Results/nubi_ml/
Results/paper_figs/
```

Machine-learning outputs required by later figure scripts are generated under:

```text
Results/nubi_ml/<ML_RUN_FOLDER>/
```

Figure outputs and diagnostics are generated under:

```text
Results/paper_figs/
```

---

## Reproducing the analysis

From the repository root, first run the predictive-decomposition script. This step is required before running Figures 6–8, because those scripts read the generated ML outputs:

```r
source("Scripts/00_run_predictive_decomposition.R", echo = TRUE)
```

This creates the curated machine-learning outputs under:

```text
Results/nubi_ml/<timestamp>_<RES>sec_.../
```

Then run the manuscript figure scripts:

```r
source("Scripts/01_figure1_participant_level_timeseries.R", echo = TRUE)
source("Scripts/02_figure2_baseline_and_tax.R", echo = TRUE)
source("Scripts/03_figure3_model_performance.R", echo = TRUE)
source("Scripts/04_figure4_context_tax.R", echo = TRUE)
source("Scripts/05_figure5_prediction_decomposition.R", echo = TRUE)
source("Scripts/06_figure6_enet_modulators.R", echo = TRUE)
source("Scripts/07_figure7_enet_horizon_simulation.R", echo = TRUE)
source("Scripts/08_figure8_horizon_scaling_rawhr.R", echo = TRUE)
source("Scripts/Fig_Supplement_Activity3_MultiRes.R", echo = TRUE)
```

Several scripts ask interactively which dataset resolution to use:

```text
10 sec
30 sec
60 sec
```

Pressing Enter uses the 60-sec manuscript/public-repository default where supported.

---

## Required generated ML outputs

Figures 6–8 require compatible generated files under:

```text
Results/nubi_ml/<ML_RUN_FOLDER>/
```

The scripts auto-detect the newest compatible run folder for the selected resolution by checking for required files, rather than relying on a fixed timestamped folder name. This allows users to run the scripts locally and use ML output folders with locally generated names.

### Figure 6 requires

```text
feature_importance_grouped_DRIVING.csv
feature_importance_grouped_NONDRIVING_SEDENTARY.csv
feature_importance_terms_DRIVING.csv
feature_importance_terms_NONDRIVING_SEDENTARY.csv
compare_metrics_rawhr_overall_by_stratum.csv
```

### Figure 7 requires

```text
predictor_list_DRIVING.csv
best_params_DRIVING.csv
```

### Figure 8 requires

```text
predictions_all_models_both_strata.csv
```

These files are generated locally by running the predictive-decomposition workflow before running Figures 6–8.

---

## R environment

The scripts were written for R and use common CRAN packages.

Install required packages with:

```r
install.packages(c(
  "data.table",
  "lubridate",
  "ggplot2",
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "patchwork",
  "scales",
  "tidymodels",
  "glmnet"
))
```

Some systems may also require additional tidymodels dependencies, which R will usually install automatically.

---

## Figure 6: ENet modulators

Script:

```text
Scripts/06_figure6_enet_modulators.R
```

Purpose:

Figure 6 visualizes non-physiological ENet modulators of heart rate beyond participant baseline and context-level cardiovascular tax.

Panels:

```text
A. Grouped ENet feature importance beyond baseline+tax
B. Signed standardized coefficients for interpretable predictors
C. Top continuous modulators shown as coefficient-implied effects
D. Incremental ENet gain beyond baseline+tax
```

Main outputs:

```text
Results/paper_figs/<timestamp>_<RES>sec_figure6_enet_modulators/
├── Figure6_ENet_Modulators.pdf
├── Figure6_ENet_Modulators.png
├── diagnostics_summary.txt
├── diag_counts_by_stratum.csv
├── diag_panelA_grouped_features.csv
├── diag_panelB_terms.csv
├── diag_panelC_selected_modulators.csv
├── diag_panelC_effect_curves.csv
└── diag_panelD_incremental_gain.csv
```

---

## Figure 7: ENet horizon simulation

Script:

```text
Scripts/07_figure7_enet_horizon_simulation.R
```

Purpose:

Figure 7 translates short-timescale ENet-predicted heart-rate burden into annual cumulative NHR-hours under repeated driving schedules.

The ENet model is refit on the full DRIVING stratum using saved predictors and selected hyperparameters. Predicted raw HR is converted to baseline-referenced load:

```text
NHR_hat = RAW_HR_hat - bl_hr_person
```

Panels:

```text
A. Driving weather contrast using observed DRIVING rows:
   Clouds versus Adverse weather

B. Trait-anxiety contrast using a matched typical driving profile:
   Lowest versus highest trait anxiety
```

Main outputs:

```text
Results/paper_figs/<timestamp>_<RES>sec_figure7_enet_horizon_weather_trait/
├── Figures/
│   ├── Figure7_ENetHorizon_Weather_and_TraitTypical.pdf
│   └── Figure7_ENetHorizon_Weather_and_TraitTypical.png
├── Figure7_enet_params_used.csv
├── figure7_weather_split_audit.csv
├── figure7_weather_labels_used.csv
├── figure7_panelA_weather_boot_mu_draws.csv
├── figure7_panelA_weather_sim_summary.csv
├── figure7_typical_profile_used.csv
├── figure7_panelB_trait_params.csv
├── figure7_panelB_trait_sim_summary.csv
└── run_log.txt
```

---

## Figure 8: Horizon scaling of RAW_HR prediction error

Script:

```text
Scripts/08_figure8_horizon_scaling_rawhr.R
```

Purpose:

Figure 8 evaluates how ENet RAW_HR prediction error changes when residuals are averaged over progressively longer temporal horizons.

Residual definition:

```text
residual = RAW_HR_observed - RAW_HR_predicted
```

Residuals are grouped into contiguous within-subject temporal blocks at multiple averaging horizons. The main figure shows normalized RMSE of block-mean residuals versus averaging horizon for DRIVING and NONDRIVING_SEDENTARY strata.

Main outputs:

```text
Results/paper_figs/<timestamp>_<RES>sec_figure8_horizon_scaling_rawhr/
├── Figure8_HorizonScaling_RAWHR.pdf
├── Figure8_HorizonScaling_RAWHR.png
├── Figure8_HorizonScaling_LogLogDiagnostic.pdf
├── Figure8_HorizonScaling_LogLogDiagnostic.png
├── Figure8_block_means_long.csv
├── Figure8_block_counts_by_horizon.csv
├── Figure8_horizon_metrics_long.csv
├── Figure8_loglog_slopes.csv
├── Figure8_reduction_summary.csv
├── Figure8_caption_numbers.csv
├── diagnostics_rowmap_DRIVING.csv
├── diagnostics_rowmap_NONDRIVING_SEDENTARY.csv
├── diagnostics_joined_prediction_sample.csv
├── diagnostics_segments.csv
├── diagnostics_segment_summary.csv
└── run_log.txt
```

---

## Supplementary activity3 figure

Script:

```text
Scripts/Fig_Supplement_Activity3_MultiRes.R
```

Purpose:

Generates a multipage supplementary figure showing participant-level heart-rate time series across the seven study-day slots.

Raw HR is colored by `activity3`:

```text
driving                         -> orange
non_driving_sedentary           -> black
non_driving_physical_activity   -> green
```

Baseline HR is shown in red.

The x-axis uses study-day placement over a fixed seven-slot layout:

```text
day1 = slot 1, 0-24 h
day2 = slot 2, 24-48 h
...
day7 = slot 7, 144-168 h
```

`day_num` is treated as the primary study-day key. Day1–Day7 are weekday-coded study-day labels and are not assumed to be consecutive calendar dates.

Main outputs:

```text
Results/paper_figs/<timestamp>_<RES>sec_supplementary_figure_activity3/
├── Figures/
│   └── Supplementary_Figure.pdf
├── Diagnostics/
│   ├── activity3_counts.csv
│   ├── subject_summary_before_fill.csv
│   ├── duplicate_pid_time_rows.csv
│   ├── subject_summary_study_day_slots.csv
│   ├── baseline_subject_study_day_level.csv
│   ├── baseline_subject_study_day_summary.csv
│   ├── baseline_study_day_overall_summary.csv
│   ├── baseline_subject_calendar_day_level.csv
│   ├── baseline_subject_calendar_day_summary.csv
│   ├── baseline_calendar_day_overall_summary.csv
│   └── missingness_after_fill.csv
└── run_log.txt
```

---

## Output policy

Generated figures, ML outputs, and diagnostics are written under:

```text
Results/
```

The `Results/` directory is intentionally ignored by Git and is not part of the public repository. This keeps the repository lightweight and ensures users regenerate outputs locally.

Recommended `.gitignore` entry:

```text
Results/
```

---

## Reproducibility notes

The scripts are designed to be robust to timestamped or renamed ML output folders. After running:

```r
source("Scripts/00_run_predictive_decomposition.R", echo = TRUE)
```

Figures 6–8 search under:

```text
Results/nubi_ml/
```

and select the newest folder for the chosen resolution that contains the required files.

If multiple compatible ML folders exist, the newest compatible folder is selected. To force a specific folder, edit the relevant script setting:

```r
RUN_DIR_NAME <- "your_exact_folder_name"
```

For public release, the recommended default is:

```r
RUN_DIR_NAME <- NULL
RUN_DIR_SUFFIX_REGEX <- ""
```

---

## Privacy and data-use constraints

This repository uses curated model inputs, locally generated model outputs, and timestamped features. It does not require direct GPS coordinates or raw location traces.

The curated dataset may still contain sensitive participant-level time-series information. Use of the dataset is governed by [`DATA_USE.md`](DATA_USE.md).

---

## License

The repository code is released under the MIT License. See [`LICENSE`](LICENSE).

Use of the curated dataset is governed separately by [`DATA_USE.md`](DATA_USE.md).

---

## Citation

Please cite the associated manuscript when using this repository:

```text
From Instantaneous Heart Rate to Long-Horizon Cardiovascular Burden in Naturalistic Daily Life
npj Digital Medicine
```

Full citation details should be updated after publication.
