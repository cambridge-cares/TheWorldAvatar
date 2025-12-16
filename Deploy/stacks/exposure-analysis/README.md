# Exposure Analysis Toolkit

This folder contains three analysis scripts plus shared configs. Data are fetched from a SPARQL endpoint first and fall back to the local CSV defined in `config/data_paths.json`. The intended audience is researchers preparing publication-grade figures (e.g., Lancet-level) using reproducible, scriptable workflows.

## Environment
- Python 3.10 (tested)
- Install Python deps:
  ```bash
  python3 -m venv .venv
  source .venv/bin/activate
  pip install -r requirements.txt
  ```
- If you prefer system Python, ensure the packages in `requirements.txt` are available.

## Data and config
- Primary source: SPARQL endpoint configured in `config/sparql_config.json` (and query in `config/sparql_template.rq`). If the endpoint returns no data or is unreachable, scripts fall back to the local CSV.
- All scripts read paths from `config/data_paths.json`:
  - `data`: absolute path to the input CSV (shared by all scripts).
  - `outdir`: optional override for outputs (defaults to each script’s local `results/`).
  - `details`, `leaderboard`, `external_config`: optional; used where applicable.
- Outputs: each script writes to `results/` under this directory unless `--outdir` or `outdir` in the JSON is provided.

## Scripts (recommended reading order: PAF → exposure-prevalence pattern → population-weighted partial correlation)

### 1) PAF combo search — `paf_experments.py`
- Purpose: grid-search combinations of adjustment strategies and spline hyperparameters to maximize positive population attributable fractions (PAFs) and McFadden R².
- Key outputs: `combo_details.csv`, `combo_leaderboard.csv`, plus console summaries.
- Run:
  ```bash
  python3 paf_experments.py \
    --paths-config config/data_paths.json \
    --sparql-config config/sparql_config.json \
    --sparql-template config/sparql_template.rq
  ```
- Notes: Requires `data` in `data_paths.json` (or `--data`). Results go to `results/` by default.

### 2) Exposure–prevalence patterns — `exposure_prevalence_pattern.py`
- Purpose: fit spline-based exposure–prevalence curves (relative risks) with configurable baselines and per-exposure plotting settings.
- Key outputs: per-exposure CSVs and PNG panels; aggregated `dose_response_all.csv`.
- Run:
  ```bash
  python3 exposure_prevalence_pattern.py \
    --paths-config config/data_paths.json \
    --sparql-config config/sparql_config.json \
    --sparql-template config/sparql_template.rq
  ```
- Notes: Uses external plot config if `external_config` is set in `data_paths.json` (or `--config-path`).

### 3) Population-weighted partial correlation — `population_weigted_partial_corr.py`
- Purpose: compute (weighted) partial correlations and fitted lines between an environmental exposure and multiple disease prevalences, with optional covariate controls.
- Key outputs: PNG panels per exposure, colorbars/legends, console correlation summaries.
- Run:
  ```bash
  python3 population_weigted_partial_corr.py \
    --paths-config config/data_paths.json \
    --sparql-config config/sparql_config.json \
    --sparql-template config/sparql_template.rq
  ```

## Common CLI overrides
- `--data`: override input CSV path.
- `--outdir`: override output directory.
- `--paths-config`: point to an alternate `data_paths.json`.
- `--sparql-config` / `--sparql-template`: customize endpoint/query.

## Repro checklist
1) Set `data_paths.json` `data` to your CSV (and `outdir` if desired).
2) (Optional) Configure SPARQL endpoint/query.
3) Create and activate a virtualenv; install `requirements.txt`.
4) Run scripts as above; inspect outputs under `results/`.

