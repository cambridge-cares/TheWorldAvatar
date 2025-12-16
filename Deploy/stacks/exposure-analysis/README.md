# Exposure Analysis Toolkit

This folder contains three analysis scripts and shared configs. Data load attempts the SPARQL endpoint first and falls back to the CSV set in `config/data_paths.json`. SPARQL templates are mapped in `config/sparql_templates.json` and merged by LSOA code before analysis.

## Environment
- Python 3.10
- Install Python packages:
  ```bash
  python3 -m venv .venv
  source .venv/bin/activate
  pip install -r requirements.txt
  ```
- If you prefer system Python, ensure the packages in `requirements.txt` are available.

## Data and config
- Primary source: SPARQL endpoint configured in `config/sparql_config.json`. Templates are listed in `config/sparql_templates.json`; results are merged on LSOA code. If SPARQL is empty or unreachable, scripts fall back to the CSV in `config/data_paths.json`.
- `config/data_paths.json`: set `data` (absolute CSV path, shared) and `outdir` (defaults to each script’s `results/`). Keys `details`, `leaderboard`, and `external_config` are read only when the corresponding script needs them.

## Scripts (order: PAF → exposure-prevalence pattern → population-weighted partial correlation)

### 1) PAF combo search — `paf_experments.py`
- Purpose: estimate population-attributable fractions using Poisson GLMs with restricted cubic splines across strata and adjustment sets; select configurations that reduce negative PAFs and increase McFadden R².
- Inputs: prevalence outcomes (`prevalence-*`), population denominator (`Total`), exposure(s), IMD/air/healthcare covariates when available. Data source resolves via SPARQL first, then CSV.
- How to run:
  ```bash
  cd /path/to/TheWorldAvatar/Deploy/stacks/exposure-analysis
  python3 paf_experments.py
  ```

### 2) Exposure–prevalence patterns — `exposure_prevalence_pattern.py`
- Purpose: estimate exposure–response curves (relative risks) via Poisson GLMs with spline terms; quantify non-linearity and interquartile risk contrasts per exposure–disease pair.
- Inputs: exposure columns, prevalence outcomes, population denominator, controls for air quality, healthcare access, IMD when present. Data resolves via SPARQL then CSV. Plotting hyperparameters can be supplied via `external_config`.
- How to run:
  ```bash
  cd /path/to/TheWorldAvatar/Deploy/stacks/exposure-analysis
  python3 exposure_prevalence_pattern.py
  ```

### 3) Population-weighted partial correlation — `population_weigted_partial_corr.py`
- Purpose: quantify population-weighted partial correlations (Pearson or Spearman) between exposures and disease prevalences, controlling for age structure, urban status, healthcare access, IMD, and pollutants; provide partial regression lines.
- Inputs: exposure column(s), disease prevalence columns, population weights (`Total`), age shares, urban flag/category, GP/IMD/pollutant controls when present. Data resolves via SPARQL then CSV.
- How to run:
  ```bash
  cd /path/to/TheWorldAvatar/Deploy/stacks/exposure-analysis
  python3 population_weigted_partial_corr.py
  ```

## Common CLI overrides
- `--data`: override input CSV path.
- `--outdir`: override output directory.
- `--paths-config`: alternate `data_paths.json`.
- `--sparql-config` / `--sparql-template` / `--sparql-templates`: customize endpoint and templates.

## Repro checklist
1) Set `data_paths.json` `data` (and `outdir` if needed).
2) Configure SPARQL endpoint (`config/sparql_config.json`) and templates (`config/sparql_templates.json`) if using SPARQL.
3) Create and activate a virtualenv; install `requirements.txt`.
4) Run scripts as above.

