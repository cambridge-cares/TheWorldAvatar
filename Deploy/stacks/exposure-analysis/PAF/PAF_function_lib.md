# PAF Function Library Documentation

This document provides comprehensive documentation for all functions in `paf_experments-control-health.py`.

## Table of Contents

1. [Data Extraction Functions](#data-extraction-functions)
2. [Utility Functions](#utility-functions)
3. [Control Variable Construction Functions](#control-variable-construction-functions)
4. [Statistical Analysis Functions](#statistical-analysis-functions)
5. [Scoring and Selection Functions](#scoring-and-selection-functions)
6. [Main Function](#main-function)

---

## Data Extraction Functions

### `query_twa_stack_exposure_data(csv_fallback_path=None)`

**Purpose**: Query TWA Stack SPARQL endpoint for exposure and health data. If the query fails, fallback to reading local CSV file.

**Parameters**:
- `csv_fallback_path` (str, optional): Path to CSV file used as fallback when SPARQL query fails. Default: None.

**Returns**:
- `pd.DataFrame`: DataFrame containing all required columns including LSOA codes and disease prevalence data.

**Description**:
- Attempts to query the TWA Stack SPARQL endpoint at `http://localhost:5432/ontop/sparql`
- Currently queries disease prevalence data based on OBDA mapping (CHD, COPD, DM, HF, HYP, OB, STIA)
- Parses SPARQL JSON results and converts them to a pandas DataFrame
- Maps column names to expected format (e.g., `prevalence_CHD` → `prevalence-CHD`)
- If SPARQL query fails or returns insufficient data, falls back to reading the specified CSV file
- Returns DataFrame with at least LSOA codes and disease prevalence columns

**Exceptions**:
- Raises `FileNotFoundError` if fallback CSV path is provided but file doesn't exist
- Raises generic `Exception` if SPARQL query fails and no fallback CSV is provided

---

## Utility Functions

### `_parse_bool_or_none(x: str)`

**Purpose**: Parse a string value to boolean or None, supporting various boolean representations.

**Parameters**:
- `x` (str): String to parse.

**Returns**:
- `bool`, `None`, or raises `argparse.ArgumentTypeError`: Parsed boolean value or None.

**Description**:
- Accepts: "1", "true", "t", "yes", "y", "on" → True
- Accepts: "0", "false", "f", "no", "n", "off" → False
- Accepts: "none", "null", "nan", "" → None
- Case-insensitive matching
- Raises `argparse.ArgumentTypeError` if value cannot be parsed

### `_parse_bool(x: str, default: bool = False) -> bool`

**Purpose**: Parse a string value to boolean with a default fallback.

**Parameters**:
- `x` (str): String to parse.
- `default` (bool, optional): Default value if parsing fails. Default: False.

**Returns**:
- `bool`: Parsed boolean value or default.

**Description**:
- Similar to `_parse_bool_or_none` but returns a boolean default instead of None
- Returns the default value if the input is None or cannot be parsed

### `_safe_zscore(s: pd.Series) -> pd.Series`

**Purpose**: Compute z-scores (standardized values) for a pandas Series with robust error handling.

**Parameters**:
- `s` (pd.Series): Input series to standardize.

**Returns**:
- `pd.Series`: Z-scored series with mean=0 and std=1.

**Description**:
- Converts input to numeric, handling errors gracefully
- Computes mean and variance (ddof=0)
- Returns zero-filled series if variance is non-finite or <= 0
- Handles missing values by filling with 0.0
- Returns standardized values: (x - mean) / std

### `sanitize_inputs_for_glm(y: pd.Series, N: pd.Series, X: pd.DataFrame, drop_cols_if_allnan=True)`

**Purpose**: Clean and validate inputs for GLM fitting, removing invalid rows and columns.

**Parameters**:
- `y` (pd.Series): Outcome variable (counts).
- `N` (pd.Series): Population/exposure variable.
- `X` (pd.DataFrame): Design matrix (covariates).
- `drop_cols_if_allnan` (bool, optional): Whether to drop columns with all NaN values. Default: True.

**Returns**:
- `tuple`: (y_clean, N_clean, X_clean, row_mask) where:
  - `y_clean`: Cleaned outcome series
  - `N_clean`: Cleaned population series
  - `X_clean`: Cleaned design matrix
  - `row_mask`: Boolean mask indicating valid rows

**Description**:
- Removes infinite values from all inputs
- Drops columns with all NaN values if `drop_cols_if_allnan=True`
- Filters rows where: N > 0, y is not NaN, and all X columns are not NaN
- Requires at least 50 valid samples (raises `RuntimeError` if fewer)
- Returns cleaned data with only valid rows

**Exceptions**:
- Raises `RuntimeError` if fewer than 50 valid samples remain

### `_as_prop(series)`

**Purpose**: Convert a series to proportion format (values between 0 and 1).

**Parameters**:
- `series` (pd.Series): Input series (may be percentages or proportions).

**Returns**:
- `pd.Series`: Series with values normalized to [0, 1] range.

**Description**:
- Converts input to numeric
- If values are > 1.0, assumes they are percentages and divides by 100
- Returns proportions in [0, 1] range

---

## Control Variable Construction Functions

### `exposure_transform(raw, mode, logit_eps: float = 0.001)`

**Purpose**: Apply transformation to exposure variable for modeling.

**Parameters**:
- `raw` (pd.Series or array-like): Raw exposure values.
- `mode` (str): Transformation mode: "log1p", "logit_prop01", or "none".
- `logit_eps` (float, optional): Epsilon for logit transformation clipping. Default: 0.001.

**Returns**:
- `tuple`: (transformed_series, raw_series) where:
  - `transformed_series`: Transformed exposure values
  - `raw_series`: Original raw values (for reference)

**Description**:
- **log1p**: Applies log(1+x) transformation, adjusting for negative values by subtracting minimum
- **logit_prop01**: Applies logit transformation: log(p/(1-p)) after converting to proportion and clipping to [eps, 1-eps]
- **none**: Returns original values unchanged
- Returns both transformed and original values for downstream use

### `build_air_quality_vars(df)`

**Purpose**: Construct air quality control variables (NO2, SO2, PM10) from DataFrame.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing air quality columns.

**Returns**:
- `pd.DataFrame`: DataFrame with z-scored air quality variables.

**Description**:
- Looks for columns: 'ah4no2', 'ah4so2', 'ah4pm10'
- Standardizes each variable using `_safe_zscore`
- Returns DataFrame with columns prefixed with "air_" (e.g., "air_ah4no2")
- Returns empty DataFrame if no air quality columns are found

### `build_healthcare_access_vars(df)`

**Purpose**: Construct healthcare access control variable from GP accessibility data.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing 'ah4gp' column.

**Returns**:
- `pd.DataFrame`: DataFrame with z-scored healthcare access variable.

**Description**:
- Extracts 'ah4gp' column (GP accessibility/drive time)
- Applies log1p transformation: log(1 + gp_time)
- Standardizes using `_safe_zscore`
- Returns DataFrame with column "healthcare_access"
- Returns empty DataFrame if 'ah4gp' column is missing

### `build_imd_vars(df, exposure_name: str, exposure_series: pd.Series)`

**Purpose**: Construct IMD (Index of Multiple Deprivation) control variables with collinearity handling.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing IMD columns.
- `exposure_name` (str): Name of the exposure variable (used for collinearity checks).
- `exposure_series` (pd.Series): Exposure variable values (used for correlation checks).

**Returns**:
- `pd.DataFrame`: DataFrame with z-scored IMD variables (columns prefixed with "imd_").

**Description**:
- Uses full set of IMD sub-domain scores: Income, Education, Employment, Wider Barriers, Outdoors
- For socioeconomic exposures (e.g., fuel poverty), checks correlation with IMD controls
- Drops IMD columns with |correlation| >= 0.90 to reduce collinearity
- Standardizes remaining IMD columns using z-scores
- Returns empty DataFrame if no valid IMD columns remain

### `build_age_vars(df, mode='ai')`

**Purpose**: Construct age control variables using different modes.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing age proportion columns.
- `mode` (str, optional): Age control mode: 'ai' (ageing index) or '6band' (6-band proportions). Default: 'ai'.

**Returns**:
- `tuple`: (age_DataFrame, age_column_names) where:
  - `age_DataFrame`: DataFrame with age control variables
  - `age_column_names`: List of column names

**Description**:
- **ai mode**: Computes ageing index = (proportion_65+ / proportion_0-14) * 100
  - Handles division by zero and infinite values
  - Fills NaN values with median
  - Returns DataFrame with column "age_index"
- **6band mode**: Uses 6 age band proportions (0-14, 15-44, 45-64, 65-74, 75-84, 85+)
  - Drops reference category (prop_15_44 by default)
  - Normalizes proportions if they sum to > 1.0
  - Prefixes columns with "age_" and fills NaN with 0.0
  - Returns DataFrame with remaining age band columns

### `build_urban_control_vars(df, mode='none')`

**Purpose**: Construct urban/rural control variables using different encoding modes.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing urban/rural columns.
- `mode` (str, optional): Urban control mode: 'none', 'binary', or 'categorical'. Default: 'none'.

**Returns**:
- `pd.DataFrame`: DataFrame with urban/rural dummy variables.

**Description**:
- **none**: Returns empty DataFrame (no urban control)
- **binary**: Creates single dummy variable from 'Urban_rural_flag' column
  - Maps to categories: 'Rural', 'Urban'
  - Drops first category (Rural) as reference
  - Returns DataFrame with column "URB_Urban" (1=Urban, 0=Rural)
- **categorical**: Creates dummy variables from 'RUC21CD' column (6-category classification)
  - Categories: RLF1, RLN1, RSF1, RSN1, UF1, UN1
  - Drops 'UN1' as reference category
  - Returns DataFrame with 5 dummy columns (RUC_RLF1, RUC_RLN1, etc.)

### `create_strata(df, mode)`

**Purpose**: Create stratification series for subgroup analysis.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame.
- `mode` (str): Stratification mode: 'none', 'binary', 'ruc_6cat', or 'age_structure'.

**Returns**:
- `pd.Series` or None: Series with stratum labels, or None if mode is 'none'.

**Description**:
- **none**: Returns None (no stratification)
- **binary**: Returns 'Urban_rural_flag' column
- **ruc_6cat**: Returns 'RUC21CD' column (6-category rural-urban classification)
- **age_structure**: Creates binary stratification based on age:
  - 'Young': proportion_65+ < 0.19
  - 'Old': proportion_65+ >= 0.19
- Raises `ValueError` for unknown modes

---

## Statistical Analysis Functions

### `compute_paf(res_full, X_fit, N_fit, x_raw, design_info, spline_cols, transform_mode, direction, *, logit_eps: float = 0.001)`

**Purpose**: Compute Population Attributable Fraction (PAF) for multiple percentiles.

**Parameters**:
- `res_full`: Fitted GLM model result object.
- `X_fit` (pd.DataFrame): Design matrix used for model fitting.
- `N_fit` (pd.Series): Population/exposure counts used for fitting.
- `x_raw` (np.ndarray or pd.Series): Raw exposure values (native scale).
- `design_info`: Patsy design info object for spline basis reconstruction.
- `spline_cols` (list): List of spline column names.
- `transform_mode` (str): Exposure transformation mode ("log1p", "logit_prop01", or "none").
- `direction` (str): Direction of exposure effect: "lower_better" or "higher_better".
- `logit_eps` (float, optional): Epsilon for logit transformation. Default: 0.001.

**Returns**:
- `dict`: Dictionary containing:
  - `toP{p}`: PAF value for percentile p (signed, as proportion)
  - `tgt_p{p}`: Raw-scale threshold value for percentile p
  - For each p in PCT_LIST: [50, 40, 30, 20, 10]

**Description**:
- Computes PAF by comparing observed cases to counterfactual cases (if exposure set to target percentile)
- For "lower_better": target is p-th percentile (lower values)
- For "higher_better": target is (100-p)-th percentile (higher values)
- Applies same transformation used during model fitting
- Rebuilds spline basis for counterfactual exposure values
- Computes: PAF = 1 - (cases_counterfactual / cases_observed)
- Returns both PAF values and the raw-scale thresholds used

### `run_one_config(df_raw, strata_mode, age_mode, use_air, use_health, urban_ctrl_mode, exposure_cfg, *, sens_scenario: str = "base", spline_df: int = 4, logit_eps: float = 0.001)`

**Purpose**: Fit Poisson GLM and compute PAF for a single configuration combination.

**Parameters**:
- `df_raw` (pd.DataFrame): Raw input data.
- `strata_mode` (str): Stratification mode.
- `age_mode` (str): Age control mode ('ai' or '6band').
- `use_air` (bool): Whether to include air quality controls.
- `use_health` (bool): Whether to include healthcare access controls.
- `urban_ctrl_mode` (str): Urban control mode ('none', 'binary', 'categorical', or 'n/a').
- `exposure_cfg` (dict): Exposure configuration containing 'name', 'transform', 'direction'.
- `sens_scenario` (str, optional): Sensitivity scenario name. Default: "base".
- `spline_df` (int, optional): Degrees of freedom for restricted cubic splines. Default: 4.
- `logit_eps` (float, optional): Epsilon for logit transformation. Default: 0.001.

**Returns**:
- `pd.DataFrame`: DataFrame with results for all diseases and strata, containing:
  - Configuration parameters (scenario, spline_df, logit_eps, strata_mode, etc.)
  - Disease and stratum identifiers
  - Sample size (n)
  - McFadden R²
  - PAF values for each percentile (paf_p50, paf_p40, paf_p30, paf_p20, paf_p10)
  - Target thresholds for each percentile (tgt_p50, tgt_p40, etc.)

**Description**:
- Stratifies data if strata_mode is not 'none'
- For each stratum:
  - Transforms exposure variable according to exposure_cfg
  - Builds spline basis using Patsy with specified degrees of freedom
  - Constructs control variables (age, urban, air, health, IMD)
  - For each disease:
    - Fits Poisson GLM with offset = log(population)
    - Computes PAF for all percentiles in PCT_LIST
    - Computes McFadden R²
    - Stores results in output DataFrame
- Returns DataFrame with one row per (disease, stratum) combination
- Returns empty DataFrame if no valid results are produced

---

## Scoring and Selection Functions

### `negativity_count(row)`

**Purpose**: Count the number of negative PAF values in a result row.

**Parameters**:
- `row` (pd.Series or dict): Result row containing PAF columns.

**Returns**:
- `int`: Number of negative PAF values across all percentiles.

**Description**:
- Checks PAF columns: 'paf_p50', 'paf_p25', 'paf_p10' (or similar)
- Counts how many are finite and negative
- Used in scoring to penalize configurations with negative PAFs

### `score_block(df_block)`

**Purpose**: Score a block of results (same configuration, multiple diseases/strata).

**Parameters**:
- `df_block` (pd.DataFrame): DataFrame containing results for one configuration.

**Returns**:
- `tuple`: (negatives, mean_paf, mean_r2, score) where:
  - `negatives`: Total count of negative PAF values
  - `mean_paf`: Mean of positive PAF values (clipped at 0)
  - `mean_r2`: Mean McFadden R²
  - `score`: Combined score = mean_paf + 20.0 * mean_r2

**Description**:
- Counts total negative PAF values across all rows
- Computes mean of positive PAF values (negative values clipped to 0)
- Computes mean McFadden R²
- Scoring formula: score = mean_paf + 20.0 * mean_r2
- Lower negatives and higher score are better
- Returns default values (10^9, -inf, -inf, -inf) if block is empty

### `select_best_combo(df_all, exposure)`

**Purpose**: Select the best configuration for a given exposure based on scoring.

**Parameters**:
- `df_all` (pd.DataFrame): DataFrame containing all results.
- `exposure` (str): Exposure name to filter results.

**Returns**:
- `tuple`: (best_dict, best_rows) where:
  - `best_dict`: Dictionary with best configuration parameters and scores
  - `best_rows`: DataFrame containing all rows for the best configuration

**Description**:
- Filters results for the specified exposure
- Groups by configuration parameters: scenario, spline_df, logit_eps, strata_mode, urban_ctrl_mode, age_mode, use_air, use_health
- Scores each configuration block using `score_block`
- Selects configuration with:
  1. Fewest negative PAF values
  2. Highest score (if negatives are equal)
- Returns best configuration dictionary and corresponding result rows
- Returns (None, None) if no results found for exposure

---

## Main Function

### `main()`

**Purpose**: Main entry point for PAF grid search and best configuration selection.

**Description**:
- Parses command-line arguments:
  - `--force-health`: Force healthcare-access adjustment (True/False/None)
  - `--sensitivity`: Enable sensitivity analysis (vary spline_df and logit_eps)
  - `--fixed-config`: Run single fixed configuration instead of grid search
- Supports environment variable overrides (PAF_FORCE_HEALTH, PAF_SENSITIVITY)
- Loads data using `query_twa_stack_exposure_data()` with CSV fallback
- Merges KG data with CSV if both are available (KG takes precedence)
- Enumerates all configuration combinations:
  - Sensitivity scenarios (if enabled): base, sens_low, sens_high
  - Stratification modes
  - Age control modes
  - Urban control modes
  - Air quality controls (True/False)
  - Healthcare access controls (True/False)
- Runs `run_one_config()` for each combination
- Saves all results to `combo_details.csv`
- Selects best configuration for each exposure using `select_best_combo()`
- Saves best configurations to `combo_leaderboard.csv`
- Prints summary statistics including:
  - Number of negative PAF values
  - Mean positive PAF
  - Mean R²
  - Score
  - Per-disease PAF values and thresholds for all percentiles

**Output Files**:
- `combo_details.csv`: All configuration results
- `combo_leaderboard.csv`: Best configuration per exposure

---

## Global Constants

- `TWA_SPARQL_ENDPOINT`: SPARQL endpoint URL for TWA Stack queries
- `CSV_PATH`: Default path to input CSV file
- `BASE_OUT`: Base output directory
- `TOTAL_COL`: Column name for population totals
- `URBAN_FLAG_COL`: Column name for binary urban/rural flag
- `RUC_COL`: Column name for 6-category rural-urban classification
- `AGE_6`: List of 6 age band proportion column names
- `AGE_DROP`: Reference age category to drop in 6-band mode
- `PCT_LIST`: List of percentiles for PAF computation: [50, 40, 30, 20, 10]
- `EXPOSURES`: List of exposure configurations
- `DISEASES`: List of disease prevalence column names
- `USE_IMD_CONTROLS`: Boolean flag to enable IMD controls
- `IMD_FULL_COLS`: List of IMD sub-domain column names
- `FIXED_CONFIG`: Boolean flag for fixed-config mode
- `FIXED_PRESET`: Dictionary with fixed configuration parameters

