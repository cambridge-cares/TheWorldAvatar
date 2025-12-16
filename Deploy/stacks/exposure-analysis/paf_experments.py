# -*- coding: utf-8 -*-
"""
PAF_FORCE_HEALTH=true python3 ./paf_experments-control-health.py
PAF combo grid search (fully automated selection, robust cleaning)
Goal: find settings where PAFs are as positive as possible and McFadden R² is as large as possible
Outputs: combo_details.csv (all runs) and combo_leaderboard.csv (best combo per exposure)

This version:
- Use fixed percentiles: P50 / P40 / P30 / P20 / P10
- compute_paf also returns raw-scale thresholds (tgt_pXX)
- Scoring / negative-count / printing covers all 5 percentiles
"""

import os, warnings, argparse, json
from typing import Any, Dict, Optional
import numpy as np, pandas as pd, statsmodels.api as sm
from patsy import dmatrix, build_design_matrices
from statsmodels.genmod.families import Poisson
import requests

warnings.filterwarnings("ignore")

# NOTE: This integrated file contains IMD columns and fuel poverty exposure.
RESULT_DIR = os.path.join(os.path.dirname(__file__), "results")
CSV_PATH  = None
BASE_OUT  = RESULT_DIR
DETAILS_CSV = None
LEADER_CSV  = None

CONFIG_DIR = os.path.join(os.path.dirname(__file__), "config")
DEFAULT_PATHS_CONFIG_PATH = os.path.join(CONFIG_DIR, "data_paths.json")
DEFAULT_SPARQL_CONFIG_PATH = os.path.join(CONFIG_DIR, "sparql_config.json")
DEFAULT_SPARQL_TEMPLATE_PATH = os.path.join(CONFIG_DIR, "sparql_template.rq")
DEFAULT_SPARQL_TEMPLATES_PATH = os.path.join(CONFIG_DIR, "sparql_templates.json")

DEFAULT_PATHS_CONFIG: Dict[str, Any] = {
    "data": None,
    "outdir": None,
    "details": None,
    "leaderboard": None,
    "external_config": None
}

DEFAULT_SPARQL_CONFIG: Dict[str, Any] = {
    "endpoint": "http://localhost:3838/sparql/ui"
}

DEFAULT_SPARQL_TEMPLATES: Dict[str, str] = {
    "demographics": os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL", "demographics_core.sparql"),
    "exposures": os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL", "exposures_all.sparql"),
    "diseases": os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL", "disease_all.sparql"),
    "imd": os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL", "imd_all.sparql"),
    "crime_fuel": os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL", "crime_fuel_all.sparql"),
}

SPARQL_COLUMN_RENAMES: Dict[str, str] = {
    "prevalence_CHD": "prevalence-CHD",
    "prevalence_COPD": "prevalence-COPD",
    "prevalence_DM": "prevalence-DM",
    "prevalence_HF": "prevalence-HF",
    "prevalence_HYP": "prevalence-HYP",
    "prevalence_OB": "prevalence-OB",
    "prevalence_PAD": "prevalence-PAD",
    "prevalence_STIA": "prevalence-STIA",
}

TOTAL_COL = "Total"
URBAN_FLAG_COL = "Urban_rural_flag"  # Rural/Urban
RUC_COL = "RUC21CD"                  # RLF1/RLN1/RSF1/RSN1/UF1/UN1
AGE_6 = ['prop_0_14','prop_15_44','prop_45_64','prop_65_74','prop_75_84','prop_85_plus']
AGE_DROP = 'prop_15_44'

PCT_LIST = [50, 40, 30, 20, 10]

# ========== Debug printing (default off) ==========
# If True, print Poisson full/null fit summaries and key diagnostics for each (exposure, disease, stratum, scenario).
DEBUG_PRINT = True

# ========== IMD controls (mirrors leis.py "full IMD controls") ==========
# Full set of IMD sub-domain scores used as covariates.
USE_IMD_CONTROLS = True
IMD_FULL_COLS = [
    "Income Score (rate)",
    "Education, Skills and Training Score",
    "Employment Score (rate)",
    "Wider Barriers Sub-domain score",
    "Outdoors Sub-domain score",
]

# ========== IMD collinearity guard (only for selected socioeconomic exposures) ==========
# If an exposure is listed here, we will check correlation between that exposure and each IMD control
# and drop IMD columns with |corr| above the threshold (to reduce collinearity).
SOCIOECONOMIC_IMD_COLLINEARITY_EXPOSURES = [
    "Proportion of households fuel poor (%)",
]
IMD_COLLINEARITY_METHOD = "spearman"   # "pearson" | "spearman"
IMD_COLLINEARITY_ABS_CORR_THRESHOLD = 0.5
IMD_COLLINEARITY_MIN_N = 200

# ========== Manual fixed configuration (edit here if you want) ==========
# If set to True, the script will skip the full combo search and run ONE fixed configuration only.
# This is a pure code-level switch (no CLI needed).
FIXED_CONFIG = True

# Preset used when FIXED_CONFIG=True (or when CLI --fixed-config is passed)
FIXED_PRESET = {
    "strata_mode": "none",         # no stratification
    "age_mode": "6band",           # 6-band age controls
    "urban_ctrl_mode": "categorical",
    "use_air": False,
    "use_health": False,
    # Default single scenario to use when fixed mode is on AND sensitivity is off.
    # If sensitivity is enabled (CLI --sensitivity or env PAF_SENSITIVITY=true),
    # the script will run the sensitivity scenario list instead.
    "sens_scenario": {"name": "base", "spline_df": 4, "logit_eps": 0.001},
}

# ========== Sensitivity analysis switch (default off) ==========
# How to enable:
#   1) CLI flag: --sensitivity
#   2) env var:  PAF_SENSITIVITY=true/false
SENS_DEFAULT = False


EXPOSURES = [
    # {"name": "ah4leis",  "transform": "log1p",        "direction": "lower_better"},   # shorter is healthier
    # {"name": "ah4gpas",  "transform": "logit_prop01", "direction": "higher_better"},  # higher NDVI is healthier
    # {"name": "ah4ffood", "transform": "log1p",        "direction": "higher_better"},  # longer is healthier
    {"name": "Proportion of households fuel poor (%)", "transform": "logit_prop01", "direction": "lower_better"},  # lower is healthier
]

DISEASES = [
    'prevalence-CHD','prevalence-COPD','prevalence-DM','prevalence-HF',
    'prevalence-HYP','prevalence-OB','prevalence-STIA'
]

# ========== Search space (defaults) ==========
STRATA_CHOICES = ['none', 'binary', 'ruc_6cat', 'age_structure']  # stratification
AGE_CHOICES    = ['ai','6band']                                   # age controls
AIR_CHOICES    = [True, False]                                    # adjust for air quality
HEALTH_CHOICES = [True, False]                                    # adjust for healthcare access
URBAN_CTRL_CHOICES = ['none', 'binary', 'categorical']            # urban/rural controls (only if strata in {'none','age_structure'})

# ========== Optionally force healthcare-access control (CLI/env) ==========
FORCE_HEALTH_CONTROL = None

def _parse_bool_or_none(x: str):
    if x is None: return None
    s = x.strip().lower()
    if s in ("1","true","t","yes","y","on"): return True
    if s in ("0","false","f","no","n","off"): return False
    if s in ("none","null","nan",""): return None
    raise argparse.ArgumentTypeError("must be true/false/none")

def _parse_bool(x: str, default: bool = False) -> bool:
    if x is None:
        return default
    s = str(x).strip().lower()
    if s in ("1","true","t","yes","y","on"): return True
    if s in ("0","false","f","no","n","off"): return False
    return default

# ========== Config helpers ==========
def _deep_update(base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
    for k, v in override.items():
        if isinstance(v, dict) and isinstance(base.get(k), dict):
            base[k] = _deep_update(dict(base[k]), v)
        else:
            base[k] = v
    return base

def load_json_config(path: Optional[str], defaults: Dict[str, Any]) -> Dict[str, Any]:
    cfg = json.loads(json.dumps(defaults))
    if path and os.path.exists(path):
        try:
            with open(path, "r") as f:
                user_cfg = json.load(f)
            cfg = _deep_update(cfg, user_cfg)
        except Exception as exc:
            print(f"[warning] Failed to load config {path}: {exc}")
    return cfg

def fetch_dataframe_via_sparql(endpoint: Optional[str], template_path: Optional[str], templates_map: Optional[Dict[str, str]]) -> Optional[pd.DataFrame]:
    """
    Try provided template, then mapped templates, then all .rq/.sparql under OBDA/SPARQL; join on LSOA code.
    Returns merged DataFrame; if nothing usable, return empty so caller can fall back to CSV.
    """
    if not endpoint:
        return None

    def _run_query(qpath: str) -> Optional[pd.DataFrame]:
        try:
            with open(qpath, "r") as f:
                query = f.read()
            resp = requests.post(
                endpoint,
                data={"query": query},
                headers={"Accept": "application/sparql-results+json"},
                timeout=30,
            )
            resp.raise_for_status()
            payload = resp.json()
            bindings = payload.get("results", {}).get("bindings", [])
            if not bindings:
                return pd.DataFrame()
            rows = [{k: v.get("value") for k, v in b.items()} for b in bindings]
            return pd.DataFrame(rows)
        except Exception as exc:
            print(f"[warning] SPARQL fetch failed for {qpath}: {exc}")
            return None

    def _pick_key(df: pd.DataFrame) -> Optional[str]:
        for k in ("lsoaCode", "LSOA_CODE", "lsoa_code", "LSOA_code"):
            if k in df.columns:
                return k
        return None

    def _merge_frames(frames):
        merged = frames[0]
        key = _pick_key(merged)
        if not key:
            return pd.DataFrame()
        for df in frames[1:]:
            k = _pick_key(df)
            if not k:
                continue
            merged = merged.merge(df, left_on=key, right_on=k, how="outer")
            if k != key and k in merged.columns:
                merged = merged.drop(columns=[k])
            dup_bases = [c[:-2] for c in merged.columns if c.endswith("_x") and (c[:-2] + "_y") in merged.columns]
            for base in dup_bases:
                merged[base] = merged[base + "_x"].combine_first(merged[base + "_y"])
                merged = merged.drop(columns=[base + "_x", base + "_y"])
        return merged

    candidates = []
    if template_path and os.path.exists(template_path):
        candidates.append(template_path)
    if templates_map:
        for _, path in templates_map.items():
            if path and os.path.exists(path) and path not in candidates:
                candidates.append(path)
    obda_dir = os.path.join(os.path.dirname(__file__), "OBDA", "SPARQL")
    if os.path.isdir(obda_dir):
        for fname in sorted(os.listdir(obda_dir)):
            if fname.lower().endswith((".rq", ".sparql")):
                fpath = os.path.join(obda_dir, fname)
                if fpath not in candidates:
                    candidates.append(fpath)

    dfs = []
    for qpath in candidates:
        df = _run_query(qpath)
        if df is not None and not df.empty:
            dfs.append(df)

    if not dfs:
        return pd.DataFrame()
    merged = _merge_frames(dfs)
    return merged if not merged.empty else pd.DataFrame()

def load_input_dataframe(data_path: str, sparql_cfg: Dict[str, Any], template_path: str, templates_map: Dict[str, str]) -> pd.DataFrame:
    df = fetch_dataframe_via_sparql(sparql_cfg.get("endpoint"), template_path, templates_map)
    if df is None or df.empty:
        print("[info] SPARQL source missing or empty; falling back to configured file path.")
        return pd.read_csv(data_path)
    rename_map = {k: v for k, v in SPARQL_COLUMN_RENAMES.items() if k in df.columns}
    if rename_map:
        df = df.rename(columns=rename_map)
    return df

# ========== Utilities: robust cleaning ==========
def _safe_zscore(s: pd.Series) -> pd.Series:
    s = pd.to_numeric(s, errors="coerce")
    m = s.mean(skipna=True)
    v = s.var(ddof=0, skipna=True)
    if not np.isfinite(v) or v <= 0:
        return pd.Series(0.0, index=s.index)
    z = (s - m) / np.sqrt(v)
    return z.fillna(0.0)

# Fixed thresholds for reproducibility
MIN_SAMPLES_FOR_GLM = 50        # minimum sample size required to fit a GLM
MIN_SAMPLES_FOR_STRATUM = 100   # minimum sample size required per stratum
MIN_VALID_EXPOSURE = 50         # minimum non-missing exposure count

def sanitize_inputs_for_glm(y: pd.Series, N: pd.Series, X: pd.DataFrame, drop_cols_if_allnan=True):
    y = pd.to_numeric(y, errors="coerce").replace([np.inf, -np.inf], np.nan)
    N = pd.to_numeric(N, errors="coerce").replace([np.inf, -np.inf], np.nan)
    X = X.replace([np.inf, -np.inf], np.nan).copy()
    if drop_cols_if_allnan:
        all_nan_cols = [c for c in X.columns if X[c].isna().all()]
        if all_nan_cols:
            X = X.drop(columns=all_nan_cols)
    row_ok = (N > 0) & y.notna() & X.notna().all(axis=1)
    if row_ok.sum() < MIN_SAMPLES_FOR_GLM:
        raise RuntimeError(f"Insufficient valid samples: {int(row_ok.sum())} < {MIN_SAMPLES_FOR_GLM}")
    return y.loc[row_ok], N.loc[row_ok], X.loc[row_ok], row_ok

# ========== Other helpers ==========
def _as_prop(series):
    x = pd.to_numeric(series, errors='coerce').astype(float)
    result = np.where(x <= 1.0, x, x / 100.0)
    return pd.Series(result, index=series.index)

def exposure_transform(raw, mode, logit_eps: float = 0.001):
    raw = pd.to_numeric(raw, errors='coerce').astype(float)
    if mode == "log1p":
        mn = np.nanmin(raw)
        adj = raw - mn if (mn < 0) else raw
        return pd.Series(np.log1p(adj), index=raw.index), raw
    elif mode == "logit_prop01":
        x = _as_prop(raw)
        eps = float(logit_eps) if np.isfinite(logit_eps) else 0.001
        eps = min(max(eps, 1e-9), 0.49)
        x = np.clip(x, eps, 1.0 - eps)
        return pd.Series(np.log(x / (1 - x)), index=raw.index), raw
    else:
        return pd.Series(raw, index=raw.index), raw

def build_air_quality_vars(df):
    cols = [c for c in ['ah4no2', 'ah4so2', 'ah4pm10'] if c in df.columns]
    if not cols: return pd.DataFrame(index=df.index)
    out = {}
    for c in cols:
        out[f"air_{c}"] = _safe_zscore(pd.to_numeric(df[c], errors='coerce'))
    return pd.DataFrame(out, index=df.index)

def build_healthcare_access_vars(df):
    if 'ah4gp' not in df.columns:
        return pd.DataFrame(index=df.index)
    gp = pd.to_numeric(df['ah4gp'], errors='coerce')
    gp = gp.where(gp >= 0)
    gp_log = np.log1p(gp)
    gp_z = _safe_zscore(gp_log)
    return pd.DataFrame({'healthcare_access': gp_z}, index=df.index)

def _safe_corr(a: pd.Series, b: pd.Series, method: str = "pearson") -> float:
    a = pd.to_numeric(a, errors="coerce").astype(float)
    b = pd.to_numeric(b, errors="coerce").astype(float)
    df = pd.concat([a.rename("a"), b.rename("b")], axis=1).replace([np.inf, -np.inf], np.nan).dropna()
    if len(df) < 3:
        return float("nan")
    if method == "spearman":
        aa = df["a"].rank(method="average", na_option="keep")
        bb = df["b"].rank(method="average", na_option="keep")
        return float(np.corrcoef(aa, bb)[0, 1])
    # pearson
    return float(np.corrcoef(df["a"], df["b"])[0, 1])

def _select_imd_cols_for_exposure(df: pd.DataFrame, exposure_name: str, exposure_series: pd.Series):
    """
    Returns (kept_cols, dropped_info) where dropped_info is list of (col, corr).
    Only applies the collinearity rule for exposures listed in SOCIOECONOMIC_IMD_COLLINEARITY_EXPOSURES.
    """
    base_cols = [c for c in IMD_FULL_COLS if c in df.columns]
    if (not base_cols) or (exposure_name not in SOCIOECONOMIC_IMD_COLLINEARITY_EXPOSURES):
        return base_cols, []

    kept = []
    dropped = []
    for c in base_cols:
        r = _safe_corr(exposure_series, df[c], method=IMD_COLLINEARITY_METHOD)
        if np.isfinite(r) and (abs(r) >= float(IMD_COLLINEARITY_ABS_CORR_THRESHOLD)):
            dropped.append((c, float(r)))
        else:
            kept.append(c)

    # Don't drop anything if sample is too small (avoid noisy decisions)
    exp_num = pd.to_numeric(exposure_series, errors="coerce").astype(float)
    imd_num = df[base_cols].apply(pd.to_numeric, errors="coerce").astype(float)
    n_eff = pd.concat([exp_num.rename("__EXPO__"), imd_num], axis=1).dropna().shape[0]
    if n_eff < int(IMD_COLLINEARITY_MIN_N):
        return base_cols, []

    return kept, dropped

def build_imd_vars(df, exposure_name: str, exposure_series: pd.Series):
    """
    Full IMD controls, matching the set used in leis.py.
    If a column is missing, it is silently skipped.
    """
    cols, dropped = _select_imd_cols_for_exposure(df, exposure_name, exposure_series)
    if not cols:
        return pd.DataFrame(index=df.index)
    out = {}
    for c in cols:
        out[f"imd_{c}"] = pd.to_numeric(df[c], errors="coerce").astype(float)
    if dropped and DEBUG_PRINT:
        dropped_str = ", ".join([f"{c} (r={r:+.2f})" for c, r in dropped])
        print(f"[IMD collinearity] exposure='{exposure_name}': dropped IMD cols due to |r| >= {IMD_COLLINEARITY_ABS_CORR_THRESHOLD}: {dropped_str}")
    return pd.DataFrame(out, index=df.index)

def build_age_vars(df, mode='ai'):
    if mode == 'ai':
        p65 = pd.to_numeric(df.get('prop_65_74', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_75_84', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_85_plus', np.nan), errors='coerce')
        p014 = pd.to_numeric(df.get('prop_0_14', np.nan), errors='coerce')
        ai = (p65 / p014) * 100.0
        ai.replace([np.inf, -np.inf], np.nan, inplace=True)
        return pd.DataFrame({'age_index': ai.fillna(ai.median())}, index=df.index), ['age_index']
    # 6band
    age_data = pd.DataFrame({c: pd.to_numeric(df[c], errors='coerce').astype(float)
                             for c in AGE_6 if c in df.columns})
    if age_data.empty:
        return pd.DataFrame(index=df.index), []
    if np.nanmax(age_data.to_numpy()) > 1.0:
        age_data = age_data / 100.0
    use_cols = [c for c in age_data.columns if c != AGE_DROP]
    A = age_data[use_cols].add_prefix("age_")
    return A.fillna(0.0), list(A.columns)

def build_urban_control_vars(df, mode='none'):
    """
    Urban/rural control variables. Only included when strata in {'none','age_structure'}.
    mode:
      - 'none'         : no adjustment
      - 'binary'       : use Urban_rural_flag, create 0/1 dummy URB_Urban
      - 'categorical'  : use 6-category RUC21CD, drop 'UN1' as reference
    """
    if mode == 'none':
        return pd.DataFrame(index=df.index)
    if mode == 'binary':
        if URBAN_FLAG_COL not in df.columns:
            return pd.DataFrame(index=df.index)
        s = (df[URBAN_FLAG_COL].astype('string').str.strip().str.lower()
             .map({'urban':'Urban','rural':'Rural'}))
        d = pd.get_dummies(pd.Categorical(s, categories=['Rural','Urban']),
                           drop_first=True, prefix='URB', dtype=float)  # → URB_Urban
        return d
    if mode == 'categorical':
        if RUC_COL not in df.columns:
            return pd.DataFrame(index=df.index)
        r = df[RUC_COL].astype('string').str.strip().str.upper()
        cats = ['RLF1','RLN1','RSF1','RSN1','UF1','UN1']
        d = pd.get_dummies(pd.Categorical(r, categories=cats),
                           drop_first=False, prefix='RUC', dtype=float)
        # Reference category: UN1
        if 'RUC_UN1' in d.columns:
            d = d.drop('RUC_UN1', axis=1)
        return d
    # Any other value: treat as 'none'
    return pd.DataFrame(index=df.index)

def create_strata(df, mode):
    if mode == 'none':
        return None
    if mode == 'binary':
        return df[URBAN_FLAG_COL]
    if mode == 'ruc_6cat':
        return df[RUC_COL]
    if mode == 'age_structure':
        age_65plus = df['prop_65_74'] + df['prop_75_84'] + df['prop_85_plus']
        return np.where(age_65plus < 0.19, 'Young', 'Old')
    raise ValueError(f"Unknown stratification mode: {mode}")

# ---------- PAF + thresholds for each percentile in PCT_LIST ----------
def compute_paf(res_full, X_fit, N_fit, x_raw, design_info, spline_cols, transform_mode, direction, *,
                logit_eps: float = 0.001):
    """
    Returns a dict with:
      - toP{p}: PAF value (signed), and
      - tgt_p{p}: raw-scale threshold,
    for each p in PCT_LIST.
    """
    mu_obs = res_full.predict(X_fit, offset=np.log(N_fit)) / N_fit
    cases_obs = float(np.sum(mu_obs * N_fit))
    out = {}

    for p in PCT_LIST:
        # Target threshold on raw scale
        if direction == "lower_better":
            tgt = float(np.nanpercentile(x_raw, p))
            x_cf_raw = np.minimum(x_raw, tgt)
        else:
            tgt = float(np.nanpercentile(x_raw, 100 - p))
            x_cf_raw = np.maximum(x_raw, tgt)
        out[f"tgt_p{p}"] = tgt

        # Apply the same transform used during model fitting
        if transform_mode == "log1p":
            mn = np.nanmin(x_raw)
            adj = x_cf_raw - mn if (mn < 0) else x_cf_raw
            x_cf_tr = np.log1p(adj)
        elif transform_mode == "logit_prop01":
            x_cf_prop = _as_prop(x_cf_raw)
            eps = float(logit_eps) if np.isfinite(logit_eps) else 0.001
            eps = min(max(eps, 1e-9), 0.49)
            x_cf_prop = np.clip(x_cf_prop, eps, 1.0 - eps)
            x_cf_tr = np.log(x_cf_prop / (1 - x_cf_prop))
        else:
            x_cf_tr = x_cf_raw

        # Rebuild spline basis and replace exposure spline columns
        Xs_cf_mat = build_design_matrices([design_info], {"z": np.asarray(x_cf_tr)})[0]
        Xs_cf = pd.DataFrame(np.asarray(Xs_cf_mat), index=X_fit.index, columns=spline_cols)
        X_cf = X_fit.copy(deep=True)
        for c in spline_cols:
            X_cf[c] = np.asarray(Xs_cf[c], dtype=float)

        # Compute PAF
        mu_cf = res_full.predict(X_cf, offset=np.log(N_fit)) / N_fit
        cases_cf = float(np.sum(mu_cf * N_fit))
        paf_raw = (1.0 - (cases_cf / cases_obs)) if cases_obs > 0 else np.nan
        out[f"toP{p}"] = float(paf_raw) if np.isfinite(paf_raw) else np.nan

    return out

# ========== Fit a single configuration ==========
def run_one_config(df_raw, strata_mode, age_mode, use_air, use_health, urban_ctrl_mode, exposure_cfg,
                   *, sens_scenario: str = "base", spline_df: int = 4, logit_eps: float = 0.001):
    """
    urban_ctrl_mode is only included in the design matrix when strata_mode in {'none','age_structure'}.
    Otherwise it is ignored and stored as 'n/a'.
    """
    exposure_name = exposure_cfg['name']
    transform_mode = exposure_cfg['transform']
    direction = exposure_cfg['direction']

    # Stratification
    strata = create_strata(df_raw, strata_mode)
    strata_levels = ['All'] if strata is None else [lvl for lvl in pd.Series(strata).dropna().unique()]
    strata_data = {'All': df_raw} if strata is None else {lvl: df_raw[strata == lvl] for lvl in strata_levels}

    # Whether urban/rural adjustment is applicable
    urban_ctrl_effective = strata_mode in {'none', 'age_structure'}
    effective_urban_mode = urban_ctrl_mode if urban_ctrl_effective else 'n/a'

    rows = []
    for level in strata_levels:
        df = strata_data[level]
        if len(df) < MIN_SAMPLES_FOR_STRATUM:
            print(f"      [skip] {level}: sample size {len(df)} < {MIN_SAMPLES_FOR_STRATUM}")
            continue

        # x_raw_scale is the variable that always contains the *original/native* scale of the exposure,
        # regardless of whether any transformation (like log1p or logit) is applied for modeling.
        # This distinction is necessary: often the model uses a transformed x (for e.g. splines, linearity, etc),
        # but the reporting, interpretation, and PAF write-up refer to the exposure's original, intuitive, or natural scale.
        # Technically, in most use cases, x_raw and x_raw_scale will be identical and refer to the original values.
        # Here, we standardize by always extracting/returning this "raw"/untransformed series as x_raw_scale,
        # so downstream code can always refer to it for reporting and write-up, even if the modeling uses a transformation.

        x_raw_all = pd.to_numeric(df[exposure_name], errors='coerce')  # always the unprocessed (native scale) exposure variable
        x_tr_all, x_raw_scale = exposure_transform(x_raw_all, transform_mode, logit_eps=logit_eps)
        # x_tr_all: exposure values on transformed scale (if specified by transform_mode, e.g. "log1p", "logit_prop01")
        # x_raw_scale: exposure values on original/native scale (used for write-up, interpretation, PAF boundaries, etc)

        valid_mask = x_tr_all.notna()
        if valid_mask.sum() < MIN_VALID_EXPOSURE:
            print(f"      [skip] {level}: valid exposure count {valid_mask.sum()} < {MIN_VALID_EXPOSURE}")
            continue

        dfc = df.loc[valid_mask].copy()
        x_tr = x_tr_all[valid_mask]        # Transformed exposure values for modeling
        x_raw = x_raw_scale[valid_mask]    # Always original/native exposure scale (alias for x_raw_scale; for write-up)

        # This block generates spline basis functions for the exposure variable to allow for flexible,
        # generally nonlinear modeling of exposure-outcome relationships.
        #
        # 1. We choose the number and nature of the basis functions by setting `spline_df` (degrees of freedom):
        #    higher values allow the spline to be more flexible and capture more curvature. If not specified,
        #    the default is 4. This `df` parameter directly controls the number of underlying basis functions.
        # 2. We ensure that these are restricted cubic splines (also known as natural cubic splines)
        #    by using Patsy's "cr" function in the dmatrix formula: `cr(z, df=...)`.
        #    The "cr" formula produces cubic splines that are constrained to be linear outside the boundary knots,
        #    which is the key property of restricted cubic splines.
        # 3. We store the `design_info` with details of the basis construction, enabling us to reconstruct
        #    or apply the same spline transformation to other datasets when needed.
        # 4. For clarity, we explicitly rename the resulting columns as "EXPO_spline_1", ..., so each basis function
        #    can be referenced in an interpretable way and traced to the exposure's effect.
        # 5. We reset the index on the spline DataFrame to ensure alignment with the design matrix for model fitting.
        spline_df = int(spline_df) if spline_df is not None else 4
        Xs = dmatrix(f"cr(z, df={spline_df})", {"z": x_tr.values}, return_type="dataframe")
        # The "cr" function here ensures the basis is a restricted (natural) cubic spline.
        design_info = Xs.design_info  # Stores basis construction for future prediction/PAF calculations
        spline_cols = [f"EXPO_spline_{i+1}" for i in range(Xs.shape[1])]
        Xs.columns = spline_cols
        Xs.index = dfc.index

        # Age controls
        age_df, _ = build_age_vars(dfc, age_mode)

        # Other controls
        ctrl = [age_df]
        if USE_IMD_CONTROLS:
            ctrl.append(build_imd_vars(dfc, exposure_name=exposure_name, exposure_series=x_raw))
        if use_air:
            ctrl.append(build_air_quality_vars(dfc))
        if use_health:
            ctrl.append(build_healthcare_access_vars(dfc))
        # Urban/rural controls (only when applicable)
        if urban_ctrl_effective:
            ctrl.append(build_urban_control_vars(dfc, mode=urban_ctrl_mode))

        X = pd.concat(ctrl + [Xs], axis=1)
        X = sm.add_constant(X, has_constant='add')

        for disease in DISEASES:
            if disease not in dfc.columns:
                print(f"      [skip] {disease}: column not found")
                continue
            try:
                prevalence = _as_prop(dfc[disease])
                population = pd.to_numeric(dfc[TOTAL_COL], errors='coerce').astype(float)
                y = prevalence * population

                y_fit, N_fit, X_fit, row_ok = sanitize_inputs_for_glm(y, population, X)
                x_fit = x_raw.loc[row_ok]

                res_full = sm.GLM(y_fit, X_fit, family=Poisson(), offset=np.log(N_fit)).fit()
                pafs = compute_paf(
                    res_full, X_fit, N_fit, x_fit, design_info, spline_cols, transform_mode, direction,
                    logit_eps=logit_eps
                )

                # McFadden R²
                X_null = sm.add_constant(pd.DataFrame(index=y_fit.index), has_constant='add')
                res_null = sm.GLM(y_fit, X_null, family=Poisson(), offset=np.log(N_fit)).fit()
                r2 = 1 - (res_full.llf / res_null.llf) if res_null.llf != 0 else np.nan

                if DEBUG_PRINT:
                    mu_hat = res_full.predict(X_fit, offset=np.log(N_fit))
                    pearson_ratio = (res_full.pearson_chi2 / res_full.df_resid) if res_full.df_resid else np.nan
                    print("\n=== DEBUG: Poisson GLM fit ===")
                    print(f"scenario={sens_scenario} spline_df={int(spline_df)} logit_eps={float(logit_eps)}")
                    print(f"stratum={level} exposure={exposure_name} disease={disease} n={int(len(y_fit))}")
                    print(f"offset log(N): min={float(np.nanmin(N_fit)):.4g} max={float(np.nanmax(N_fit)):.4g}")
                    print(f"y (cases): mean={float(np.nanmean(y_fit)):.4g} var={float(np.nanvar(y_fit)):.4g}")
                    print(f"mu_hat:   mean={float(np.nanmean(mu_hat)):.4g} var={float(np.nanvar(mu_hat)):.4g}")
                    print(f"llf_full={float(res_full.llf):.6g} llf_null={float(res_null.llf):.6g} mcfadden_r2={float(r2):.6g}")
                    print(f"pearson_chi2/df_resid={float(pearson_ratio):.6g}  (>>1 suggests overdispersion)")
                    print("\n--- FULL model summary ---")
                    print(res_full.summary())
                    print("\n--- NULL model summary ---")
                    print(res_null.summary())

                # Build output row (all 5 percentiles)
                row = {
                    'sens_scenario': sens_scenario,
                    'spline_df': int(spline_df),
                    'logit_eps': float(logit_eps),
                    'strata_mode': strata_mode,
                    'stratum': level,
                    'urban_ctrl_mode': effective_urban_mode,
                    'age_mode': age_mode,
                    'use_air': use_air,
                    'use_health': use_health,
                    'exposure': exposure_name,
                    'disease': disease,
                    'n': int(len(y_fit)),
                    'mcfadden_r2': r2
                }
                for p in PCT_LIST:
                    row[f'paf_p{p}'] = pafs.get(f'toP{p}', np.nan) * 100
                    row[f'tgt_p{p}'] = pafs.get(f'tgt_p{p}', np.nan)

                rows.append(row)
            except Exception as e:
                print(f"      [skip] {disease}: {type(e).__name__}: {str(e)}")
                continue

    if rows:
        return pd.DataFrame(rows)

    # Return an empty DataFrame with all expected columns (prevents concat errors)
    base_cols = ['sens_scenario','spline_df','logit_eps',
                 'strata_mode','stratum','urban_ctrl_mode','age_mode','use_air','use_health',
                 'exposure','disease','n','mcfadden_r2']
    extra = [f'paf_p{p}' for p in PCT_LIST] + [f'tgt_p{p}' for p in PCT_LIST]
    return pd.DataFrame(columns=base_cols + extra)

# ========== Scoring + selection ==========
def negativity_count(row):
    cnt = 0
    for p in PCT_LIST:
        v = row.get(f'paf_p{p}', np.nan)
        if np.isfinite(v) and v < 0:
            cnt += 1
    return cnt

def score_block(df_block):
    if df_block.empty:
        return 10**9, -np.inf, -np.inf, -np.inf
    negs = df_block.apply(negativity_count, axis=1).sum()

    paf_pos_cols = [f'paf_p{p}' for p in PCT_LIST]
    paf_pos = df_block[paf_pos_cols].clip(lower=0)  # clip negatives to 0 for scoring only
    mean_paf = float(paf_pos.stack().mean(skipna=True))

    mean_r2  = float(df_block['mcfadden_r2'].mean(skipna=True))
    score = mean_paf + 20.0*mean_r2
    return int(negs), mean_paf, mean_r2, score

def select_best_combo(df_all, exposure):
    df_exp = df_all[df_all['exposure'] == exposure].copy()
    if df_exp.empty:
        return None, None
    # Select best combo per sensitivity scenario (easier to compare)
    keys = ['sens_scenario','spline_df','logit_eps',
            'strata_mode','urban_ctrl_mode','age_mode','use_air','use_health']
    best = None
    best_row = None
    for combo, df_block in df_exp.groupby(keys):
        neg_total, mean_paf, mean_r2, score = score_block(df_block)
        rec = {
            'exposure': exposure,
            'sens_scenario': combo[0],
            'spline_df': combo[1],
            'logit_eps': combo[2],
            'strata_mode': combo[3],
            'urban_ctrl_mode': combo[4],
            'age_mode': combo[5],
            'use_air': combo[6],
            'use_health': combo[7],
            'negatives': neg_total,
            'mean_paf_pos': mean_paf,
            'mean_r2': mean_r2,
            'score': score
        }
        if best is None or \
           (rec['negatives'] < best['negatives']) or \
           (rec['negatives'] == best['negatives'] and rec['score'] > best['score']):
            best = rec
            best_row = df_block
    return best, best_row

# ========== Main ==========
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--force-health", type=_parse_bool_or_none, default=None,
                        help="Force healthcare-access adjustment: True/False/None (None tries both)")
    parser.add_argument("--sensitivity", action="store_true",
                        help="Enable sensitivity analysis (vary spline_df / logit_eps). Default: off")
    parser.add_argument("--fixed-config", action="store_true",
                        help="Disable combo search and run ONE fixed configuration only (see code for the preset).")
    parser.add_argument("--data", default=None, help="Override data CSV path (overrides paths config)")
    parser.add_argument("--outdir", default=None, help="Override output directory (overrides paths config)")
    parser.add_argument("--details-csv", dest="details_csv", default=None, help="Override combo details CSV path")
    parser.add_argument("--leader-csv", dest="leader_csv", default=None, help="Override leaderboard CSV path")
    parser.add_argument("--paths-config", dest="paths_config", default=DEFAULT_PATHS_CONFIG_PATH,
                        help="JSON file containing data/output paths")
    parser.add_argument("--sparql-config", dest="sparql_config", default=DEFAULT_SPARQL_CONFIG_PATH,
                        help="JSON file containing SPARQL endpoint")
    parser.add_argument("--sparql-template", dest="sparql_template", default=DEFAULT_SPARQL_TEMPLATE_PATH,
                        help="SPARQL query template for primary data source")
    parser.add_argument("--sparql-templates", dest="sparql_templates", default=DEFAULT_SPARQL_TEMPLATES_PATH,
                        help="JSON mapping from data category to SPARQL template paths (demographics/exposures/diseases/imd/crime_fuel)")
    args = parser.parse_args()

    paths_cfg = load_json_config(args.paths_config, DEFAULT_PATHS_CONFIG)
    sparql_cfg = load_json_config(args.sparql_config, DEFAULT_SPARQL_CONFIG)
    sparql_templates_cfg = load_json_config(args.sparql_templates, DEFAULT_SPARQL_TEMPLATES)

    csv_path = args.data or paths_cfg.get("data") or CSV_PATH
    base_out = args.outdir or paths_cfg.get("outdir") or BASE_OUT
    details_csv = args.details_csv or paths_cfg.get("details", DETAILS_CSV)
    leader_csv = args.leader_csv or paths_cfg.get("leaderboard", LEADER_CSV)
    details_csv = details_csv or os.path.join(base_out, "combo_details.csv")
    leader_csv = leader_csv or os.path.join(base_out, "combo_leaderboard.csv")

    if not csv_path:
        raise ValueError("Data path is required (set in data_paths.json or --data).")

    os.makedirs(base_out, exist_ok=True)

    # Env var override (if provided)
    env_force = os.environ.get("PAF_FORCE_HEALTH", None)
    env_force = _parse_bool_or_none(env_force) if env_force is not None else None

    # Final healthcare-access options
    force_health = args.force_health if env_force is None else env_force
    health_options = [force_health] if force_health is not None else HEALTH_CHOICES

    # Sensitivity switch: CLI flag first, then env var
    env_sens = _parse_bool(os.environ.get("PAF_SENSITIVITY", None), default=SENS_DEFAULT)
    sens_on = bool(args.sensitivity) or bool(env_sens)

    print("Loading data…")
    df_raw = load_input_dataframe(csv_path, sparql_cfg, args.sparql_template, sparql_templates_cfg)
    print(f"Loaded rows: {len(df_raw)}")
    print(f"Healthcare-access adjustment options: {health_options}  (CLI --force-health or env PAF_FORCE_HEALTH)")
    print(f"Sensitivity analysis: {sens_on}  (CLI --sensitivity or env PAF_SENSITIVITY=true/false)")
    fixed_on = bool(FIXED_CONFIG) or bool(args.fixed_config)
    print(f"Fixed config only: {fixed_on}  (set FIXED_CONFIG=True in code or use CLI --fixed-config)")
    if USE_IMD_CONTROLS:
        present = [c for c in IMD_FULL_COLS if c in df_raw.columns]
        missing = [c for c in IMD_FULL_COLS if c not in df_raw.columns]
        print(f"IMD controls: ON ({len(present)}/{len(IMD_FULL_COLS)} columns present)")
        if missing:
            print(f"  [warn] Missing IMD columns (will be skipped): {missing}")
    else:
        print("IMD controls: OFF")

    # Constrain 6 age proportions to sum to ~1 (if present)
    if set(AGE_6).issubset(df_raw.columns):
        age_df = df_raw[AGE_6].apply(pd.to_numeric, errors='coerce').astype(float)
        s = age_df.sum(axis=1)
        bad = (s > 0) & (np.abs(s - 1.0) > 1e-4)
        if bad.any():
            df_raw.loc[bad, AGE_6] = age_df[bad].div(s[bad], axis=0).values

    details = []

    # Sensitivity scenarios: default is only 'base'
    # - spline_df: degrees of freedom for restricted cubic splines (patsy cr)
    # - logit_eps: epsilon for clipping before logit (only affects logit_prop01)
    scenarios = [{"name": "base", "spline_df": 4, "logit_eps": 0.001}]
    if sens_on:
        scenarios = [
            {"name": "base", "spline_df": 4, "logit_eps": 0.001},
            {"name": "sens_low", "spline_df": 3, "logit_eps": 0.0005},
            {"name": "sens_high", "spline_df": 5, "logit_eps": 0.005},
        ]

    # If fixed config is requested (via code toggle or CLI), override the entire search space.
    if fixed_on:
        STRATA_MODES = [str(FIXED_PRESET["strata_mode"])]
        URBAN_MODES = [str(FIXED_PRESET["urban_ctrl_mode"])]
        AGE_MODES = [str(FIXED_PRESET["age_mode"])]
        AIR_MODES = [bool(FIXED_PRESET["use_air"])]
        HEALTH_MODES = [bool(FIXED_PRESET["use_health"])]
        # Important: fixed config does NOT disable sensitivity analysis.
        # - If sens_on=True, keep the sensitivity scenario list computed above.
        # - Otherwise, use the single default scenario from FIXED_PRESET.
        if not sens_on:
            scenarios = [dict(FIXED_PRESET["sens_scenario"])]
    else:
        STRATA_MODES = STRATA_CHOICES
        URBAN_MODES = URBAN_CTRL_CHOICES
        AGE_MODES = AGE_CHOICES
        AIR_MODES = AIR_CHOICES
        HEALTH_MODES = health_options

    for exposure_cfg in EXPOSURES:
        print(f"\n=== Enumerating combos: {exposure_cfg['name']} ===")
        for sc in scenarios:
            print(f"  [scenario] {sc['name']} (spline_df={sc['spline_df']}, logit_eps={sc['logit_eps']})")
            for strata_mode in STRATA_MODES:
                # Only enumerate urban_ctrl when strata in {'none','age_structure'}, otherwise fixed to 'n/a'
                if fixed_on:
                    urban_options = URBAN_MODES
                else:
                    urban_options = URBAN_MODES if strata_mode in {'none','age_structure'} else ['n/a']
                for urban_ctrl_mode in urban_options:
                    for age_mode in AGE_MODES:
                        for use_air in AIR_MODES:
                            for use_health in HEALTH_MODES:
                                print(f"  -> Combo: scenario={sc['name']}, strata={strata_mode}, urban_ctrl={urban_ctrl_mode}, age={age_mode}, air={use_air}, health={use_health}")
                                try:
                                    df_res = run_one_config(
                                        df_raw, strata_mode, age_mode, use_air, use_health, urban_ctrl_mode, exposure_cfg,
                                        sens_scenario=sc["name"], spline_df=sc["spline_df"], logit_eps=sc["logit_eps"]
                                    )
                                    if not df_res.empty:
                                        details.append(df_res)
                                except Exception as e:
                                    print(f"     skipped: {e}")

    if not details:
        print("No results produced. Please check the input data and column names.")
        return

    df_all = pd.concat(details, ignore_index=True)
    df_all.to_csv(details_csv, index=False)
    print(f"\nWrote details: {details_csv}")

    leaders = []
    for exp in [e['name'] for e in EXPOSURES]:
        best, best_rows = select_best_combo(df_all, exp)
        if best is not None:
            leaders.append(best)
            neg_msg = "no negatives" if best['negatives']==0 else f"{best['negatives']} negatives"
            print(f"\n⭐ {exp} best combo: scenario={best.get('sens_scenario','base')}, spline_df={best.get('spline_df','')}, logit_eps={best.get('logit_eps','')}, "
                  f"strata={best['strata_mode']}, urban_ctrl={best['urban_ctrl_mode']}, "
                  f"age={best['age_mode']}, air={best['use_air']}, health={best['use_health']}  | {neg_msg} | "
                  f"meanPAF+={best['mean_paf_pos']:.2f}%  meanR2={best['mean_r2']:.3f}  score={best['score']:.2f}")

            # Per-disease signed PAF and thresholds (P50/P40/P30/P20/P10)
            if best_rows is not None and not best_rows.empty:
                print("   Details (signed PAF, P50/P40/P30/P20/P10):")
                def fmt_pct(v): return "nan" if not np.isfinite(v) else f"{v:+.2f}%"
                def fmt_num(v): return "nan" if not np.isfinite(v) else f"{v:.4g}"
                for _, r in best_rows.sort_values('disease').iterrows():
                    parts = []
                    for p in PCT_LIST:
                        parts.append(f"P{p}={fmt_pct(r.get(f'paf_p{p}', np.nan))} @tgt={fmt_num(r.get(f'tgt_p{p}', np.nan))}")
                    print(f"   {r['disease']}: " + "   ".join(parts))

    if leaders:
        df_lead = pd.DataFrame(leaders)
        df_lead.sort_values(['negatives','score'], ascending=[True, False], inplace=True)
        df_lead.to_csv(leader_csv, index=False)
        print(f"\nWrote leaderboard: {leader_csv}")
    else:
        print("No best combo selected.")

if __name__ == "__main__":
    main()
