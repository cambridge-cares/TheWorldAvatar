# Dose-Response Function Library Documentation

This document provides comprehensive documentation for all functions in `Dose-response.py`.

## Table of Contents

1. [Data Extraction Functions](#data-extraction-functions)
2. [Utility Functions](#utility-functions)
3. [Control Variable Construction Functions](#control-variable-construction-functions)
4. [Statistical Analysis Functions](#statistical-analysis-functions)
5. [Visualization Functions](#visualization-functions)
6. [Configuration Functions](#configuration-functions)
7. [Scoring and Selection Functions](#scoring-and-selection-functions)
8. [Main Function](#main-function)

---

## Data Extraction Functions

### `query_twa_stack_exposure_data(csv_fallback_path=None)`

**Purpose**: Query TWA Stack SPARQL endpoint for exposure and health data. If query fails, fallback to reading local CSV file.

**Parameters**:
- `csv_fallback_path` (str, optional): Path to CSV file used as fallback when SPARQL query fails. Default: None.

**Returns**:
- `pd.DataFrame`: DataFrame containing all required columns including LSOA codes and disease prevalence data.

**Description**:
- Attempts to query the TWA Stack SPARQL endpoint at `http://localhost:5432/ontop/sparql`
- Queries disease prevalence data based on OBDA mapping (CHD, COPD, DM, HF, HYP, OB, STIA, PAD)
- Parses SPARQL JSON results and converts them to a pandas DataFrame
- Maps column names to expected format (e.g., `prevalence_CHD` → `prevalence-CHD`)
- If SPARQL query fails, falls back to reading the specified CSV file
- Returns DataFrame with at least LSOA codes and disease prevalence columns

**Exceptions**:
- Raises `FileNotFoundError` if fallback CSV path is provided but file doesn't exist
- Raises generic `Exception` if SPARQL query fails and no fallback CSV is provided

---

## Utility Functions

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

### `exposure_transform(raw, mode)`

**Purpose**: Apply transformation to exposure variable for modeling.

**Parameters**:
- `raw` (pd.Series or array-like): Raw exposure values.
- `mode` (str): Transformation mode: "log1p", "logit_prop01", or "none".

**Returns**:
- `tuple`: (transformed_series, raw_series) where:
  - `transformed_series`: Transformed exposure values
  - `raw_series`: Original raw values (for reference)

**Description**:
- **log1p**: Applies log(1+x) transformation, adjusting for negative values
- **logit_prop01**: Applies logit transformation: log(p/(1-p)) after converting to proportion and clipping to [0.001, 0.999]
- **none**: Returns original values unchanged
- Returns both transformed and original values

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

### `sanitize_inputs_for_glm(y, N, X)`

**Purpose**: Clean and validate inputs for GLM fitting, removing invalid rows and columns.

**Parameters**:
- `y` (pd.Series): Outcome variable (counts).
- `N` (pd.Series): Population/exposure variable.
- `X` (pd.DataFrame): Design matrix (covariates).

**Returns**:
- `tuple`: (y_clean, N_clean, X_clean, row_mask) where:
  - `y_clean`: Cleaned outcome series
  - `N_clean`: Cleaned population series
  - `X_clean`: Cleaned design matrix
  - `row_mask`: Boolean mask indicating valid rows

**Description**:
- Removes infinite values from all inputs
- Drops columns with all NaN values
- Filters rows where: N > 0, y is not NaN, and all X columns are not NaN
- Requires at least 50 valid samples (raises `RuntimeError` if fewer)
- Returns cleaned data with only valid rows

**Exceptions**:
- Raises `RuntimeError` if fewer than 50 valid samples remain

### `_make_ticks_from_limits(vmin, vmax, step)`

**Purpose**: Generate tick array from minimum, maximum, and step size.

**Parameters**:
- `vmin` (float): Minimum value.
- `vmax` (float): Maximum value.
- `step` (float): Step size between ticks.

**Returns**:
- `np.ndarray` or None: Array of tick values, or None if step is invalid.

**Description**:
- Computes starting tick as ceiling of vmin divided by step, multiplied by step
- Generates ticks from start to vmax with specified step
- Filters ticks to ensure they fall within [vmin, vmax] range
- Returns None if step is None or <= 0

---

## Control Variable Construction Functions

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
- Applies log1p transformation: log(1 + gp_time) where gp_time >= 0
- Standardizes using `_safe_zscore`
- Returns DataFrame with column "healthcare_access"
- Returns empty DataFrame if 'ah4gp' column is missing

### `build_imd_vars(df, imd_col_name: str)`

**Purpose**: Construct IMD (Index of Multiple Deprivation) control variable.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing IMD column.
- `imd_col_name` (str): Name of the IMD column to use.

**Returns**:
- `pd.DataFrame`: DataFrame with z-scored IMD variable (column "imd").

**Description**:
- Extracts specified IMD column
- Standardizes using `_safe_zscore`
- Returns empty DataFrame if column is missing or has fewer than 50 valid values
- Returns DataFrame with single column "imd"

### `build_age_vars(df, mode='ai')`

**Purpose**: Construct age control variables using different modes.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing age proportion columns.
- `mode` (str, optional): Age control mode: 'ai' (ageing index) or '6band' (6-band proportions). Default: 'ai'.

**Returns**:
- `pd.DataFrame`: DataFrame with age control variables.

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

### `build_controls_from_combo(df, combo)`

**Purpose**: Build all control covariates for a given configuration combination.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame.
- `combo` (dict): Configuration dictionary containing:
  - `age_mode`: Age control mode
  - `urban_ctrl_mode`: Urban control mode
  - `use_air`: Boolean flag for air quality controls
  - `use_health`: Boolean flag for healthcare access controls
  - `use_imd`: Boolean flag for IMD controls
  - `exposure`: Exposure name (for IMD column selection)

**Returns**:
- `pd.DataFrame`: Combined DataFrame with all control variables.

**Description**:
- Constructs age controls using `build_age_vars()`
- Constructs urban/rural controls if mode is not 'none' or 'n/a'
- Adds air quality controls if `use_air=True`
- Adds healthcare access controls if `use_health=True`
- Adds IMD controls if `use_imd=True` (selects IMD column based on exposure)
- Concatenates all control DataFrames
- Removes duplicate columns if present
- Returns empty DataFrame if no controls are constructed

### `create_strata_series(df, mode)`

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

### `fit_and_curve_one(df, exposure_cfg, combo, disease, baseline_quantile=BASELINE_QUANTILE, grid_points=GRID_POINTS)`

**Purpose**: Fit spline Poisson GLM and generate dose-response curve for a single exposure-disease combination.

**Parameters**:
- `df` (pd.DataFrame): Input data.
- `exposure_cfg` (dict): Exposure configuration with 'name', 'transform', 'direction', 'percentile_low', 'percentile_high'.
- `combo` (dict): Model configuration (strata_mode, urban_ctrl_mode, age_mode, use_air, use_health).
- `disease` (str): Disease prevalence column name.
- `baseline_quantile` (float, optional): Quantile for RR baseline. Default: BASELINE_QUANTILE (0.25).
- `grid_points` (int, optional): Number of points for curve grid. Default: GRID_POINTS (200).

**Returns**:
- `tuple` or None: (curve_DataFrame, x_hist, ref_raw, q_eff) if successful, None otherwise:
  - `curve_DataFrame`: DataFrame with curve data (x_raw, rr, rr_lo, rr_hi, etc.)
  - `x_hist`: Array of exposure values for histogram
  - `ref_raw`: Raw-scale baseline value
  - `q_eff`: Effective quantile used for baseline

**Description**:
- Transforms exposure variable according to exposure_cfg
- Builds restricted cubic spline basis (df=4) using Patsy
- Constructs control variables using `build_controls_from_combo()`
- Fits two models:
  - **Spline model**: Full spline GLM
  - **Linear model**: Linear exposure term (for nonlinearity test)
- Computes nonlinearity p-value using likelihood ratio test
- Determines baseline on raw scale (based on direction: lower_better vs higher_better)
- Creates grid of exposure values (uses CUSTOM_X_LIMITS if available, else percentile bounds)
- Computes relative risk (RR) curve:
  - RR = exp(eta_grid - eta_baseline)
  - Computes 95% confidence intervals using delta method
- Computes IQR-based RR summary (Q25 vs Q75 comparison)
- Returns DataFrame with columns:
  - Configuration parameters
  - Baseline values and quantiles
  - Nonlinearity p-value
  - IQR statistics
  - Curve data: x_raw, rr, rr_lo, rr_hi
  - Sample size and McFadden R²
- Returns None if insufficient data or fitting fails

---

## Visualization Functions

### `plot_panels(df_curve, x_hist, exposure_name, out_png, font_family=DEFAULT_FONT_FAMILY, label_size=DEFAULT_LABEL_SIZE, tick_size=DEFAULT_TICK_SIZE, hist_color=DEFAULT_HIST_COLOR, hist_alpha=DEFAULT_HIST_ALPHA, pnonlin_size=DEFAULT_PNONLIN_SIZE, xlabel_pad=DEFAULT_XLABEL_PAD, ylabel_pad=DEFAULT_YLABEL_PAD, xticks=DEFAULT_XTICKS, refline_color=DEFAULT_REFLINE_COLOR, refline_width=DEFAULT_REFLINE_WIDTH, refline_alpha=DEFAULT_REFLINE_ALPHA)`

**Purpose**: Generate multi-panel dose-response plot with up to 8 subplots (one per disease).

**Parameters**:
- `df_curve` (pd.DataFrame): DataFrame with curve data from `fit_and_curve_one()`.
- `x_hist` (np.ndarray): Exposure values for histogram overlay.
- `exposure_name` (str): Name of exposure variable.
- `out_png` (str): Output PNG file path.
- `font_family` (str, optional): Font family for text. Default: DEFAULT_FONT_FAMILY.
- `label_size` (int, optional): Font size for axis labels and titles. Default: DEFAULT_LABEL_SIZE.
- `tick_size` (int, optional): Font size for tick labels. Default: DEFAULT_TICK_SIZE.
- `hist_color` (str, optional): Color for histogram. Default: DEFAULT_HIST_COLOR.
- `hist_alpha` (float, optional): Transparency for histogram. Default: DEFAULT_HIST_ALPHA.
- `pnonlin_size` (int, optional): Font size for p-nonlinearity text. Default: DEFAULT_PNONLIN_SIZE.
- `xlabel_pad` (float, optional): Padding for x-axis label. Default: DEFAULT_XLABEL_PAD.
- `ylabel_pad` (float, optional): Padding for y-axis label. Default: DEFAULT_YLABEL_PAD.
- `xticks` (int, optional): Number of x-axis ticks. Default: DEFAULT_XTICKS.
- `refline_color` (str, optional): Color for reference line (RR=1.0). Default: DEFAULT_REFLINE_COLOR.
- `refline_width` (float, optional): Width for reference line. Default: DEFAULT_REFLINE_WIDTH.
- `refline_alpha` (float, optional): Transparency for reference line. Default: DEFAULT_REFLINE_ALPHA.

**Returns**:
- None (saves PNG file to disk).

**Description**:
- Determines number of diseases to plot from EXPOSURE_DISEASE_PANELS or DISEASES
- Creates subplot layout: 1x4 for 4 diseases, 2x4 for 8 diseases, or auto-layout
- For each disease panel:
  - Plots dose-response curve (RR vs exposure)
  - Adds reference line at RR=1.0
  - Optionally overlays histogram on right axis
  - Displays p-nonlinearity text in corner
  - Sets x-axis ticks based on EXPOSURE_X_TICKSTEP configuration
  - Sets y-axis range and ticks:
    - Uses Y_TICK_STEP if configured
    - Otherwise uses smart tick selection (small/medium/large sets or symmetric expansion)
    - Ensures RR=1.0 is always included
    - Centers display around RR=1.0 (BASE_TARGET parameter)
  - Adds y-axis grid lines
- Sets shared x-axis label (exposure name)
- Sets shared y-axis label ("Relative Risk (vs baseline)")
- Adds figure title with baseline quantile information
- Saves figure as PNG with 200 DPI

**Y-axis Tick Selection Logic**:
- Prefers fixed sets: small (0.90-1.10, step 0.05), medium (0.80-1.20, step 0.10), large (0.60-1.40, step 0.20)
- If none cover the range, uses symmetric expansion around 1.0
- Fallback: linear split with forced insertion of 1.0

---

## Configuration Functions

### `_load_external_config(path: str)`

**Purpose**: Load hyperparameters from external JSON configuration file.

**Parameters**:
- `path` (str): Path to JSON configuration file.

**Returns**:
- `dict`: Configuration dictionary.

**Description**:
- Reads JSON file and returns parsed dictionary
- Used to override default visualization and fitting parameters

### `_apply_external_config(cfg: dict)`

**Purpose**: Apply external configuration to global variables.

**Parameters**:
- `cfg` (dict): Configuration dictionary from `_load_external_config()`.

**Returns**:
- None (modifies global variables).

**Description**:
- Updates global variables based on configuration:
  - EXPOSURES: Exposure configurations with percentile bounds
  - EXPOSURE_X_TICKSTEP: X-axis tick step sizes per exposure
  - CUSTOM_X_LIMITS: Custom x-axis limits for plotting
  - Y_TICK_STEP: Y-axis tick step size
  - PEAK_PAD_FRAC, BASE_TARGET: Y-axis range parameters
  - Style parameters: CI_ALPHA, LINE_WIDTH, LINE_COLOR, CI_COLOR, etc.
  - IMD settings: USE_IMD, USE_IMD_BY_EXPO, IMD_COL_BY_EXPO

### `_maybe_load_external_config()`

**Purpose**: Conditionally load and apply external configuration if enabled.

**Parameters**:
- None (uses global USE_EXTERNAL_CONFIG and EXTERNAL_CONFIG_PATH).

**Returns**:
- None.

**Description**:
- Checks if USE_EXTERNAL_CONFIG is True
- Attempts to load configuration from EXTERNAL_CONFIG_PATH
- Silently falls back to defaults if file doesn't exist or loading fails
- Prints status messages

---

## Scoring and Selection Functions

### `negativity_count(row)`

**Purpose**: Count the number of negative PAF values in a result row.

**Parameters**:
- `row` (pd.Series or dict): Result row containing PAF columns.

**Returns**:
- `int`: Number of negative PAF values.

**Description**:
- Checks PAF columns: 'paf_p50', 'paf_p25', 'paf_p10'
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
- Returns default values (10^9, -inf, -inf, -inf) if block is empty

### `pick_best_from_details(df_details, exposure)`

**Purpose**: Select the best configuration for a given exposure from combo_details.csv.

**Parameters**:
- `df_details` (pd.DataFrame): DataFrame from combo_details.csv.
- `exposure` (str): Exposure name to filter results.

**Returns**:
- `dict` or None: Best configuration dictionary, or None if no results found.

**Description**:
- Filters results for the specified exposure
- Groups by configuration parameters: strata_mode, urban_ctrl_mode, age_mode, use_air, use_health
- Scores each configuration block using `score_block()`
- Selects configuration with:
  1. Fewest negative PAF values
  2. Highest score (if negatives are equal)
- Returns dictionary with best configuration parameters and scores
- Returns None if no results found for exposure

---

## Main Function

### `main()`

**Purpose**: Main entry point for dose-response curve generation.

**Description**:
- Parses command-line arguments:
  - `--details`: Path to combo_details.csv (for selecting best configs)
  - `--data`: Path to input CSV file
  - `--outdir`: Output directory for results
  - `--baseline-quantile`: Quantile for RR baseline (default: 0.25)
  - `--grid-points`: Number of curve grid points (default: 200)
  - `--fixed-config`: Use fixed configuration instead of selecting from details
  - `--use-config`: Enable external config loading
  - `--config-path`: Path to external JSON config file
- Loads external configuration if enabled
- Loads data using `query_twa_stack_exposure_data()` with CSV fallback
- Merges KG data with CSV if both are available (KG takes precedence)
- For each exposure:
  - Selects configuration:
    - External config mode: uses config from JSON
    - Fixed config mode: uses FIXED_PRESET
    - Normal mode: uses `pick_best_from_details()` to select from combo_details.csv
  - Creates strata if stratification is enabled
  - For each stratum:
    - Fits dose-response curves for all diseases using `fit_and_curve_one()`
    - Saves curve data to CSV (one file per exposure-stratum)
    - Generates multi-panel plot using `plot_panels()`
    - Saves plot as PNG (one file per exposure-stratum)
- Saves combined results to "dose_response_all.csv"

**Output Files**:
- `dose_response_{exposure}_stratum-{level}.csv`: Curve data per exposure-stratum
- `dose_response_{exposure}_stratum-{level}.png`: Plot per exposure-stratum
- `dose_response_all.csv`: Combined results for all exposures

---

## Global Constants

- `TWA_SPARQL_ENDPOINT`: SPARQL endpoint URL for TWA Stack queries
- `TOTAL_COL`: Column name for population totals
- `URBAN_FLAG_COL`: Column name for binary urban/rural flag
- `RUC_COL`: Column name for 6-category rural-urban classification
- `AGE_6`: List of 6 age band proportion column names
- `AGE_DROP`: Reference age category to drop in 6-band mode
- `EXPOSURES`: List of exposure configurations
- `DISEASES`: List of disease prevalence column names
- `GRID_POINTS`: Number of points for dose-response curve grid (default: 200)
- `BASELINE_QUANTILE`: Quantile for RR baseline (default: 0.25)
- `EXPOSURE_DISEASE_PANELS`: Mapping of exposures to disease panels for plotting
- `EXPOSURE_LABELS`: Friendly names for exposure variables
- `FIXED_CONFIG`: Boolean flag for fixed-config mode
- `USE_EXTERNAL_CONFIG`: Boolean flag for external config loading
- `EXTERNAL_CONFIG_PATH`: Path to external JSON configuration file

