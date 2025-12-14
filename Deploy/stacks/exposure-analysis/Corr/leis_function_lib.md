# Correlation Analysis Function Library Documentation

This document provides comprehensive documentation for all functions in `leis.py`.

## Table of Contents

1. [Data Preparation Functions](#data-preparation-functions)
2. [Age Control Functions](#age-control-functions)
3. [Urban-Rural Functions](#urban-rural-functions)
4. [Weighting and Ranking Functions](#weighting-and-ranking-functions)
5. [Correlation Functions](#correlation-functions)
6. [Plotting Functions](#plotting-functions)
7. [Utility Functions](#utility-functions)
8. [Main Program](#main-program)

---

## Data Preparation Functions

### `prevalence_to_pct(series: pd.Series) -> pd.Series`

**Purpose**: Convert prevalence values to percentage format.

**Parameters**:
- `series` (pd.Series): Input series with prevalence values (proportions or percentages).

**Returns**:
- `pd.Series`: Series with values in percentage format (0-100).

**Description**:
- Converts input to numeric
- If maximum value is <= 1.0, assumes proportions and multiplies by 100
- Otherwise, assumes values are already percentages and returns as-is
- Returns series with values in [0, 100] range

### `compute_age_share_pct(df: pd.DataFrame, age_key: str) -> pd.Series`

**Purpose**: Compute age group share as percentage.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing age proportion columns.
- `age_key` (str): Age group key (e.g., 'G_0_14', 'G_65_plus').

**Returns**:
- `pd.Series`: Series with age group share as percentage.

**Description**:
- Looks up age group definition from AGE_GROUP_DEFS dictionary
- Sums proportions for all columns in the age group
- Converts to percentage (multiplies by 100)
- Returns series with percentage values

### `compute_ageing_index(df: pd.DataFrame) -> pd.Series`

**Purpose**: Compute ageing index (ratio of 65+ to 0-14 age groups).

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing age proportion columns.

**Returns**:
- `pd.Series`: Series with ageing index values.

**Description**:
- Computes proportion of 65+ (G_65_plus) and 0-14 (G_0_14) age groups
- Calculates ageing index = (proportion_65+ / proportion_0-14) * 100
- Replaces infinite values with NaN
- Returns series with ageing index values (used for color coding in plots)

---

## Age Control Functions

### `build_age_controls(df: pd.DataFrame) -> pd.DataFrame`

**Purpose**: Build age control variables based on AGE_CTRL_MODE setting.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame containing age proportion columns.

**Returns**:
- `pd.DataFrame`: DataFrame with age control variables.

**Description**:
- **ai/ageing_index mode**: Returns DataFrame with single column "__AGEING_INDEX__"
- **single mode**: Returns DataFrame with single column "__AGE__" for specified age group
- **4band mode**: Returns DataFrame with 3 age band columns (drops reference category)
  - Columns: age_0_14, age_45_64, age_65_plus (drops age_15_44)
  - Values in percentage format
- **6band mode**: Returns DataFrame with 5 age band columns (drops reference category)
  - Columns: prop_0_14, prop_45_64, prop_65_74, prop_75_84, prop_85_plus (drops prop_15_44)
  - Values in percentage format
- All modes convert proportions to percentages (multiply by 100)

---

## Urban-Rural Functions

### `_normalize_binary_urban(ser: pd.Series) -> pd.Series`

**Purpose**: Normalize binary urban/rural series to standard categories.

**Parameters**:
- `ser` (pd.Series): Input series with urban/rural values.

**Returns**:
- `pd.Series`: Series with normalized categories ('Rural', 'Urban').

**Description**:
- Converts to string, strips whitespace, converts to lowercase
- Maps 'urban' → 'Urban', 'rural' → 'Rural'
- Creates categorical series with categories ['Rural', 'Urban']
- Returns normalized series

### `detect_urban_series(df: pd.DataFrame) -> Optional[pd.Series]`

**Purpose**: Detect and extract urban/rural series based on URBAN_MODE setting.

**Parameters**:
- `df` (pd.DataFrame): Input DataFrame.

**Returns**:
- `pd.Series` or None: Urban/rural series, or None if not found.

**Description**:
- If URBAN_MODE is 'binary': looks for URBAN_BINARY_COL and normalizes using `_normalize_binary_urban()`
- If URBAN_MODE is 'categorical': returns URBAN_CATEGORICAL_COL directly
- Returns None if required column is not found

### `prepare_urban(ser: Optional[pd.Series]) -> Optional[pd.Series]`

**Purpose**: Prepare urban/rural series for use in control matrix (convert to categorical if needed).

**Parameters**:
- `ser` (pd.Series, optional): Input urban/rural series.

**Returns**:
- `pd.Series` or None: Prepared series, or None if input is None.

**Description**:
- Returns None if input is None
- If already categorical, returns as-is
- If numeric with <= 100 unique values, converts to integer then categorical
- Otherwise converts to string then categorical
- Returns prepared series

---

## Weighting and Ranking Functions

### `_prep_weights(w: pd.Series) -> pd.Series`

**Purpose**: Prepare weights for weighted analysis with scaling options.

**Parameters**:
- `w` (pd.Series): Raw weight values.

**Returns**:
- `pd.Series`: Prepared weights.

**Description**:
- Converts to numeric and clips to >= 0
- Applies scaling based on WEIGHT_SCALING:
  - 'sqrt': square root transformation
  - 'log1p': log(1+x) transformation
  - 'identity': no transformation
- Removes infinite values
- Fills NaN with median (or 1.0 if median is invalid)
- Clips to minimum value WEIGHT_MIN
- Returns prepared weights

### `_is_binary_dummy(s: pd.Series) -> bool`

**Purpose**: Check if a series represents binary dummy variables (0/1).

**Parameters**:
- `s` (pd.Series): Input series.

**Returns**:
- `bool`: True if series is binary dummy, False otherwise.

**Description**:
- Converts to numeric
- Rounds to integers
- Checks if unique values are subset of {0, 1} and count <= 2
- Returns True if binary, False otherwise

### `weighted_rank(s: pd.Series, w: pd.Series) -> pd.Series`

**Purpose**: Compute weighted rank (RIDIT score) for a series.

**Parameters**:
- `s` (pd.Series): Values to rank.
- `w` (pd.Series): Weights for ranking.

**Returns**:
- `pd.Series`: Weighted ranks (RIDIT scores) in [0, 1] range.

**Description**:
- Groups values by unique value, summing weights within each group
- Computes cumulative sum of weights
- Calculates RIDIT: (cumsum_shifted + 0.5 * group_weight) / total_weight
- Returns RIDIT scores mapped back to original index
- Returns NaN-filled series if input is empty

### `weighted_pearson(a: np.ndarray, b: np.ndarray, w: np.ndarray) -> float`

**Purpose**: Compute weighted Pearson correlation coefficient.

**Parameters**:
- `a` (np.ndarray): First variable.
- `b` (np.ndarray): Second variable.
- `w` (np.ndarray): Weights (must sum to 1 after normalization).

**Returns**:
- `float`: Weighted Pearson correlation coefficient, or NaN if invalid.

**Description**:
- Normalizes weights to sum to 1
- Computes weighted means: sum(W * a), sum(W * b)
- Computes weighted covariance: sum(W * (a-mean_a) * (b-mean_b))
- Computes weighted standard deviations
- Returns correlation = covariance / (std_a * std_b)
- Returns NaN if denominator is zero or insufficient data

### `weighted_spearman(a: pd.Series, b: pd.Series, w: pd.Series) -> float`

**Purpose**: Compute weighted Spearman correlation coefficient.

**Parameters**:
- `a` (pd.Series): First variable.
- `b` (pd.Series): Second variable.
- `w` (pd.Series): Weights.

**Returns**:
- `float`: Weighted Spearman correlation coefficient, or NaN if invalid.

**Description**:
- Computes weighted ranks for both variables using `weighted_rank()`
- Computes weighted Pearson correlation on the ranks using `weighted_pearson()`
- Returns correlation coefficient

### `wls_beta(X: np.ndarray, y: np.ndarray, w: np.ndarray) -> np.ndarray`

**Purpose**: Compute weighted least squares (WLS) regression coefficients.

**Parameters**:
- `X` (np.ndarray): Design matrix (n x p).
- `y` (np.ndarray): Outcome vector (n,).
- `w` (np.ndarray): Weights vector (n,).

**Returns**:
- `np.ndarray`: Regression coefficients (p,).

**Description**:
- Computes sqrt(weights) for scaling
- Scales X and y by sqrt(weights)
- Solves weighted least squares: beta = (X'WX)^(-1)X'Wy
- Returns coefficient vector

### `wls_residual(y: np.ndarray, Z: np.ndarray, w: np.ndarray) -> np.ndarray`

**Purpose**: Compute weighted least squares residuals.

**Parameters**:
- `y` (np.ndarray): Outcome vector.
- `Z` (np.ndarray): Design matrix.
- `w` (np.ndarray): Weights vector.

**Returns**:
- `np.ndarray`: Residual vector (y - Z @ beta).

**Description**:
- Computes WLS coefficients using `wls_beta()`
- Computes predicted values: Z @ beta
- Returns residuals: y - predicted

### `pearson_corr_np(x: np.ndarray, y: np.ndarray) -> float`

**Purpose**: Compute unweighted Pearson correlation from numpy arrays.

**Parameters**:
- `x` (np.ndarray): First variable.
- `y` (np.ndarray): Second variable.

**Returns**:
- `float`: Pearson correlation coefficient, or NaN if invalid.

**Description**:
- Computes means and centers both variables
- Computes correlation: (x @ y) / sqrt(sum(x^2) * sum(y^2))
- Returns NaN if denominator is zero or insufficient data

---

## Correlation Functions

### `make_controls_df(age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame], use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool, rank: bool=False, weights: Optional[pd.Series]=None) -> pd.DataFrame`

**Purpose**: Construct control variables DataFrame for partial correlation analysis.

**Parameters**:
- `age_ctrl` (pd.Series or pd.DataFrame): Age control variable(s).
- `urban_series` (pd.Series, optional): Urban/rural series.
- `gp_series` (pd.Series, optional): GP accessibility series.
- `imd_series` (pd.Series, optional): IMD deprivation series.
- `extra_ctrl` (pd.DataFrame, optional): Extra control variables.
- `use_age` (bool): Whether to include age controls.
- `use_urban` (bool): Whether to include urban/rural controls.
- `use_gp` (bool): Whether to include GP controls.
- `use_imd` (bool): Whether to include IMD controls.
- `use_extra` (bool): Whether to include extra controls.
- `rank` (bool, optional): Whether to rank-transform non-binary variables. Default: False.
- `weights` (pd.Series, optional): Weights for weighted ranking. Default: None.

**Returns**:
- `pd.DataFrame`: Combined DataFrame with all control variables.

**Description**:
- Constructs age controls (as DataFrame or single column)
- Constructs urban/rural dummy variables if categorical
- Adds GP, IMD, and extra controls if enabled
- Optionally rank-transforms non-binary, non-dummy variables
- Prefixes columns with "__AGE__", "URB_", "__GP__", "__IMD__", "__EXTRA_" for identification
- Returns concatenated DataFrame

### `build_design_matrix(x: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame], use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool)`

**Purpose**: Build design matrix for linear regression with intercept and controls.

**Parameters**:
- `x` (pd.Series): Exposure variable (will be included as '__X__').
- `age_ctrl` (AgeLike): Age control variable(s).
- `urban_series` (pd.Series, optional): Urban/rural series.
- `gp_series` (pd.Series, optional): GP accessibility series.
- `imd_series` (pd.Series, optional): IMD deprivation series.
- `extra_ctrl` (pd.DataFrame, optional): Extra control variables.
- `use_age` (bool): Whether to include age controls.
- `use_urban` (bool): Whether to include urban/rural controls.
- `use_gp` (bool): Whether to include GP controls.
- `use_imd` (bool): Whether to include IMD controls.
- `use_extra` (bool): Whether to include extra controls.

**Returns**:
- `tuple`: (dfX, X_mat, colnames, ctrl_means) where:
  - `dfX`: DataFrame with x and controls (for reference)
  - `X_mat`: Numpy array design matrix with intercept column
  - `colnames`: List of column names including 'Intercept'
  - `ctrl_means`: Dictionary of control variable means (for line fitting)

**Description**:
- Builds control DataFrame using `make_controls_df()` (without ranking)
- Concatenates x (as '__X__') with controls
- Removes rows with NaN or infinite values
- Constructs numpy design matrix with intercept (column of ones)
- Computes mean values of control variables (for holding constant in fitted lines)
- Returns design matrix and metadata

### `fit_partial_line(x: pd.Series, y: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame], use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool, ngrid: int = 200, weights: Optional[pd.Series] = None, weight_policy: str = 'full')`

**Purpose**: Fit partial regression line (y ~ x | controls) for plotting.

**Parameters**:
- `x` (pd.Series): Exposure variable.
- `y` (pd.Series): Outcome variable.
- `age_ctrl` (AgeLike): Age control variable(s).
- `urban_series` (pd.Series, optional): Urban/rural series.
- `gp_series` (pd.Series, optional): GP accessibility series.
- `imd_series` (pd.Series, optional): IMD deprivation series.
- `extra_ctrl` (pd.DataFrame, optional): Extra control variables.
- `use_age` (bool): Whether to include age controls.
- `use_urban` (bool): Whether to include urban/rural controls.
- `use_gp` (bool): Whether to include GP controls.
- `use_imd` (bool): Whether to include IMD controls.
- `use_extra` (bool): Whether to include extra controls.
- `ngrid` (int, optional): Number of grid points for line. Default: 200.
- `weights` (pd.Series, optional): Weights for WLS. Default: None.
- `weight_policy` (str, optional): Weighting policy: 'full', 'y_only', or 'none'. Default: 'full'.

**Returns**:
- `tuple` or None: (xgrid, ygrid) arrays for plotting, or None if insufficient data.

**Description**:
- Builds design matrix using `build_design_matrix()`
- Fits regression: y ~ intercept + controls + x
- Uses WLS if weights provided and weight_policy in ('full', 'y_only')
- Creates grid of x values spanning the data range
- Predicts y values with controls held at their means
- Returns x and y arrays for line plotting
- Returns None if fewer than 10 valid observations

### `corr_pearson(x: pd.Series, y: pd.Series) -> float`

**Purpose**: Compute unweighted Pearson correlation coefficient.

**Parameters**:
- `x` (pd.Series): First variable.
- `y` (pd.Series): Second variable.

**Returns**:
- `float`: Pearson correlation coefficient, or NaN if invalid.

**Description**:
- Converts inputs to numeric
- Drops rows with NaN in either variable
- Computes correlation using numpy.corrcoef
- Returns correlation coefficient or NaN if insufficient data

### `corr_spearman(x: pd.Series, y: pd.Series) -> float`

**Purpose**: Compute unweighted Spearman rank correlation coefficient.

**Parameters**:
- `x` (pd.Series): First variable.
- `y` (pd.Series): Second variable.

**Returns**:
- `float`: Spearman correlation coefficient.

**Description**:
- Ranks both variables using pandas rank()
- Computes Pearson correlation on the ranks
- Returns Spearman correlation

### `partial_correlation(x: pd.Series, y: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame], rank: bool, use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool, weights: Optional[pd.Series] = None, weight_policy: str = 'full') -> float`

**Purpose**: Compute partial correlation coefficient controlling for specified variables.

**Parameters**:
- `x` (pd.Series): First variable.
- `y` (pd.Series): Second variable.
- `age_ctrl` (AgeLike): Age control variable(s).
- `urban_series` (pd.Series, optional): Urban/rural series.
- `gp_series` (pd.Series, optional): GP accessibility series.
- `imd_series` (pd.Series, optional): IMD deprivation series.
- `extra_ctrl` (pd.DataFrame, optional): Extra control variables.
- `rank` (bool): Whether to use rank-based (Spearman) correlation.
- `use_age` (bool): Whether to control for age.
- `use_urban` (bool): Whether to control for urban/rural.
- `use_gp` (bool): Whether to control for GP.
- `use_imd` (bool): Whether to control for IMD.
- `use_extra` (bool): Whether to control for extra variables.
- `weights` (pd.Series, optional): Weights for weighted analysis. Default: None.
- `weight_policy` (str, optional): Weighting policy. Default: 'full'.

**Returns**:
- `float`: Partial correlation coefficient, or NaN if invalid.

**Description**:
- Optionally rank-transforms x and y if rank=True
- Constructs control matrix using `make_controls_df()`
- If no controls: returns simple (weighted) correlation
- If controls present:
  - Regresses x and y on controls separately
  - Computes residuals: rx = x - x_hat, ry = y - y_hat
  - Computes correlation of residuals
  - Uses WLS if weights provided
- Returns partial correlation coefficient
- Returns NaN if fewer than 3 valid observations

### `trim_by_percentile(xx: pd.Series, yy: pd.Series, x_frac: Tuple[float, float], y_frac: Tuple[float, float]) -> pd.Index`

**Purpose**: Filter data to keep only values within specified percentile ranges.

**Parameters**:
- `xx` (pd.Series): First variable.
- `yy` (pd.Series): Second variable.
- `x_frac` (Tuple[float, float]): Percentile range for x (e.g., (0.001, 0.999)).
- `y_frac` (Tuple[float, float]): Percentile range for y (e.g., (0.001, 0.999)).

**Returns**:
- `pd.Index`: Index of rows that pass both filters.

**Description**:
- Computes percentile thresholds for both variables
- Filters to keep rows where x and y are within their respective ranges
- Uses actual min/max if percentiles are invalid
- Returns index of passing rows

### `partial_median_line(x: pd.Series, y: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame], use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool, bins: int = 20)`

**Purpose**: Compute partial residual median line for nonparametric smoothing.

**Parameters**:
- `x` (pd.Series): Exposure variable.
- `y` (pd.Series): Outcome variable.
- `age_ctrl` (AgeLike): Age control variable(s).
- `urban_series` (pd.Series, optional): Urban/rural series.
- `gp_series` (pd.Series, optional): GP accessibility series.
- `imd_series` (pd.Series, optional): IMD deprivation series.
- `extra_ctrl` (pd.DataFrame, optional): Extra control variables.
- `use_age` (bool): Whether to control for age.
- `use_urban` (bool): Whether to control for urban/rural.
- `use_gp` (bool): Whether to control for GP.
- `use_imd` (bool): Whether to control for IMD.
- `use_extra` (bool): Whether to control for extra variables.
- `bins` (int, optional): Number of bins for smoothing. Default: 20.

**Returns**:
- `tuple` or None: (x_bins, y_medians) arrays, or None if insufficient data.

**Description**:
- Computes partial residuals for x and y (controlling for specified variables)
- Bins x residuals into specified number of bins
- Computes median y residual within each bin
- Returns bin centers and median values
- Returns None if fewer than 20 observations or fewer than 3 valid bins

---

## Plotting Functions

### `make_color_norm(series: pd.Series)`

**Purpose**: Create color normalization object for scatter plot coloring.

**Parameters**:
- `series` (pd.Series): Values to use for color mapping (typically ageing index).

**Returns**:
- `Normalize`, `PowerNorm`, or `LogNorm`: Matplotlib normalization object.

**Description**:
- Computes percentile range based on COLOR_PCTLS setting
- Creates normalization based on COLOR_SCALE:
  - 'linear': Linear normalization
  - 'power': Power-law normalization with COLOR_GAMMA
  - 'log': Logarithmic normalization
- Returns normalization object for use with colormap

### `fit_raw_ols_line(x: pd.Series, y: pd.Series, ngrid: int = 200)`

**Purpose**: Fit simple OLS line (y ~ x) without controls.

**Parameters**:
- `x` (pd.Series): Exposure variable.
- `y` (pd.Series): Outcome variable.
- `ngrid` (int, optional): Number of grid points. Default: 200.

**Returns**:
- `tuple` or None: (xgrid, ygrid) arrays, or None if insufficient data.

**Description**:
- Fits simple linear regression: y = a*x + b
- Creates grid spanning x range
- Predicts y values on grid
- Returns x and y arrays for plotting
- Returns None if fewer than 2 observations

### `plot_env_across_diseases(df: pd.DataFrame, env: str, diseases_all: List[str], outprefix: str)`

**Purpose**: Generate multi-panel scatter plots showing correlation between exposure and multiple diseases.

**Parameters**:
- `df` (pd.DataFrame): Input data.
- `env` (str): Exposure variable name.
- `diseases_all` (List[str]): List of disease prevalence column names.
- `outprefix` (str): Output file prefix (full path will be constructed).

**Returns**:
- None (saves PNG files to disk).

**Description**:
- Creates subplot grid (PANEL_ROWS x PANEL_COLS) per figure
- For each disease panel:
  - Creates scatter plot (x=exposure, y=disease prevalence)
  - Colors points by ageing index
  - Optionally splits markers by urban/rural status
  - Fits and plots partial regression line (controlling for specified variables)
  - Optionally plots raw OLS line
  - Optionally plots partial median smoothing line
  - Adds reference line at fitted line intercept (RR=1 equivalent)
  - Optionally fills area between reference and fitted line
  - Computes and displays partial correlation coefficient and p-value
  - Adds statistical annotation box
- Sets shared x-axis if SHARE_X=True
- Saves separate colorbar and legend images (transparent PNG)
- Saves main figure as PNG with specified DPI
- Handles pagination if number of diseases exceeds panel capacity

**Features**:
- Supports weighted analysis (population weighting)
- Supports rank-based (Spearman) partial correlation
- Uses historical rho values if available (hidden feature)
- Adjusts fitted line slope to match historical correlation if specified
- Applies percentile trimming to remove outliers

---

## Utility Functions

### `fisher_p_from_r(r: float, n_eff: float, k_ctrl: int = 0) -> float`

**Purpose**: Compute p-value for correlation coefficient using Fisher's z-transformation.

**Parameters**:
- `r` (float): Correlation coefficient.
- `n_eff` (float): Effective sample size.
- `k_ctrl` (int, optional): Number of control variables. Default: 0.

**Returns**:
- `float`: Two-tailed p-value, or NaN if invalid.

**Description**:
- Computes degrees of freedom: n_eff - k_ctrl - 3
- Applies Fisher's z-transformation: z = 0.5 * log((1+r)/(1-r))
- Computes standard error: se = 1/sqrt(df)
- Computes test statistic: |z| / se
- Returns p-value using complementary error function
- Returns NaN if correlation is non-finite or df <= 0

### `_format_p(p: float) -> str`

**Purpose**: Format p-value as string for display.

**Parameters**:
- `p` (float): P-value.

**Returns**:
- `str`: Formatted p-value string.

**Description**:
- Returns "NA" if p-value is non-finite
- Returns "< 1 × 10⁻⁵" (with superscript) if p <= 1e-5
- Returns formatted string: "{p:.3f}" if p < 0.01
- Returns formatted string: "{p:.2f}" otherwise

### `make_safe_filename_tag(tag: str, reserve: int = 12, max_basename: int = 200) -> str`

**Purpose**: Generate safe filename from descriptive tag string.

**Parameters**:
- `tag` (str): Descriptive tag string.
- `reserve` (int, optional): Reserved characters for extension. Default: 12.
- `max_basename` (int, optional): Maximum filename length. Default: 200.

**Returns**:
- `str`: Safe filename string with hash suffix.

**Description**:
- Computes MD5 hash of tag (first 8 characters)
- Removes non-alphanumeric characters (except ., _, -)
- Truncates to fit within max_basename limit
- Appends hash: "{tag}__{hash8}"
- Ensures minimum length of 32 characters

### `_format_y_tick(val, pos)`

**Purpose**: Custom formatter for y-axis tick labels.

**Parameters**:
- `val` (float): Tick value.
- `pos` (int): Tick position (unused).

**Returns**:
- `str`: Formatted tick label.

**Description**:
- Formats values with appropriate precision
- Handles very small and very large values
- Returns integer format for values >= 10
- Returns decimal format for values in [1, 10)
- Returns high-precision format for values < 1

### `_format_x_tick(val, pos)`

**Purpose**: Custom formatter for x-axis tick labels.

**Parameters**:
- `val` (float): Tick value.
- `pos` (int): Tick position (unused).

**Returns**:
- `str`: Formatted tick label.

**Description**:
- Uses general format ("{val:g}") for automatic precision
- Handles zero and non-finite values

### `_legend_text_for_line()`

**Purpose**: Generate legend text describing which variables are controlled in fitted line.

**Parameters**:
- None (uses global control flags).

**Returns**:
- `str`: Legend text string.

**Description**:
- Checks which control variables are enabled (age, urban, GP, IMD, extra)
- Constructs descriptive text: "Population-weighted fit, controlling for ..."
- Returns text for use in legend

### `save_separate_colorbar_once(color_norm)`

**Purpose**: Save colorbar as separate transparent PNG file (only once).

**Parameters**:
- `color_norm`: Matplotlib normalization object.

**Returns**:
- None (saves PNG file).

**Description**:
- Creates separate figure with colorbar
- Uses specified colormap (COLOR_CMAP) and normalization
- Saves as transparent PNG to base_out directory
- Only saves once (tracks via global flag)

### `save_separate_colorbar_horizontal_once(color_norm)`

**Purpose**: Save horizontal colorbar as separate transparent PNG file (only once).

**Parameters**:
- `color_norm`: Matplotlib normalization object.

**Returns**:
- None (saves PNG file).

**Description**:
- Creates separate figure with horizontal colorbar
- Adds custom tick labels for better readability
- Saves as transparent PNG to base_out directory
- Only saves once (tracks via global flag)

### `save_separate_legend_once()`

**Purpose**: Save urban/rural marker legend as separate transparent PNG file (only once).

**Parameters**:
- None.

**Returns**:
- None (saves PNG file).

**Description**:
- Creates legend showing urban (circle) and rural (triangle) markers
- Saves as transparent PNG to base_out directory
- Only saves once (tracks via global flag)

### `save_separate_line_legend_once()`

**Purpose**: Save line style legend as separate transparent PNG file (only once).

**Parameters**:
- None.

**Returns**:
- None (saves PNG file).

**Description**:
- Creates legend showing partial regression line and reference line styles
- Uses `_legend_text_for_line()` for description
- Saves as transparent PNG to base_out directory
- Only saves once (tracks via global flag)

### `abbr_of(dis: str) -> str`

**Purpose**: Get abbreviation for disease name.

**Parameters**:
- `dis` (str): Disease prevalence column name.

**Returns**:
- `str`: Disease abbreviation.

**Description**:
- Looks up abbreviation in DISEASE_ABBR dictionary
- Returns abbreviation or original name if not found

### `chunk_list(lst: List[str], n: int) -> List[List[str]]`

**Purpose**: Split list into chunks of specified size.

**Parameters**:
- `lst` (List[str]): Input list.
- `n` (int): Chunk size.

**Returns**:
- `List[List[str]]`: List of chunks.

**Description**:
- Splits list into sublists of size n
- Last chunk may be smaller
- Returns list of chunks

---

## Main Program

The main program (executed when script is run directly):

1. Loads data from CSV file
2. Normalizes age proportions to sum to 1.0 if needed
3. For each exposure variable in ENV_VARS:
   - Constructs safe filename tag based on configuration
   - Calls `plot_env_across_diseases()` to generate plots
   - Saves plots with descriptive filenames

**Output Files**:
- Main plots: `{safe_tag}.png` (or `{safe_tag}_part{N}.png` for multiple pages)
- Separate assets: `colorbar_ageing_index.png`, `colorbar_ageing_index_horizontal.png`, `legend_urban_rural.png`, `legend_lines.png`

---

## Global Constants

- `csv_path`: Path to input CSV file
- `base_out`: Base output directory for plots
- `PANEL_ROWS`, `PANEL_COLS`: Subplot grid dimensions
- `PANEL_W_INCH`, `PANEL_H_INCH`: Panel size in inches
- `ENV_VARS`: List of exposure variables to plot
- `DISEASE_VARS`: List of disease prevalence columns
- `AGE_CTRL_MODE`: Age control mode ('single', '4band', '6band', 'ai')
- `URBAN_MODE`: Urban/rural mode ('binary', 'categorical')
- `CORR_TYPE`: Correlation type ('pearson', 'spearman', 'partial_pearson', 'partial_spearman')
- `USE_POP_WEIGHT`: Whether to use population weighting
- `WEIGHT_POLICY`: Weighting policy ('full', 'y_only', 'none')
- `COLOR_CMAP`: Colormap for scatter plot coloring
- `COLOR_SCALE`: Color scale type ('linear', 'power', 'log')
- `TRIM_X_FRAC`, `TRIM_Y_FRAC`: Percentile ranges for data trimming

