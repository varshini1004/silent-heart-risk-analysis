# Live Demo link -

https://varshini1004.github.io/silent-heart-risk-analysis/


# Silent Heart Attack Risk Analysis

Analysis of "silent risk" heart disease patients — asymptomatic patients (no chest
pain) who nonetheless have confirmed heart disease — using the UCI Cleveland Heart
Disease dataset. Includes demographic/clinical profiling, a Random Forest
feature-importance model, k-means clustering, risk stratification, and an
auto-generated interactive HTML dashboard.

## Data source

[UCI Machine Learning Repository – Heart Disease dataset (Cleveland)](https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data)
The script downloads this automatically at runtime — no local data file needed.

## Project structure

```
.
├── silent_risk_analysis.R    # Main analysis: data prep, RF model, stats, plots, CSV
├── generate_dashboard.R      # Builds the interactive HTML patient dashboard
├── output/                   # Generated results (git-ignored, created on run)
├── .gitignore
└── README.md
```

## Requirements

- R (>= 4.1 recommended)
- Packages: `tidyverse`, `caret`, `ggplot2`, `corrplot`, `randomForest`, `glue`

Install them with:

```r
install.packages(c("tidyverse", "caret", "ggplot2", "corrplot", "randomForest", "glue"))
```

## Usage

Run from the project root (so relative paths resolve correctly):

```r
source("silent_risk_analysis.R")
source("generate_dashboard.R")
```

Or from the shell:

```bash
Rscript silent_risk_analysis.R
Rscript generate_dashboard.R
```

`generate_dashboard.R` depends on objects (`silent_risk_patients`, `top_factors`,
`patient_count`) created by `silent_risk_analysis.R`, so run them in order in the
same session, or `source()` the first script from the top of the second if you
want a single-command run.

## Outputs

All generated files land in `output/`:

- `silent_risk_focused_analysis.csv` — full per-patient data for the silent-risk cohort
- `random_forest_importance.png` — feature importance chart
- `silent_risk_age_distribution.png` — age histogram
- `silent_vs_symptomatic_comparison.png` — clinical comparison bar chart
- `silent_risk_dashboard.html` — interactive patient-card dashboard

## Notes

- This is a research/educational exploratory analysis of a public dataset, **not**
  a validated clinical tool, and shouldn't be used to inform real diagnostic or
  treatment decisions.
- `output/` is git-ignored by default since it's fully reproducible from the
  scripts; remove that line from `.gitignore` if you'd rather version the
  generated files (e.g., to render the dashboard via GitHub Pages).
