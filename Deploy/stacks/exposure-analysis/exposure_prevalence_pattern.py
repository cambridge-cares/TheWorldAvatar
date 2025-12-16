# -*- coding: utf-8 -*-
"""
Purpose: 
- APR
-RR
-Response curves
"""

import os, argparse, warnings, math, json
from typing import Any, Dict, Optional
import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
from patsy import dmatrix, build_design_matrices
from statsmodels.genmod.families import Poisson
from scipy.stats import chi2
from matplotlib.ticker import MaxNLocator
import requests

warnings.filterwarnings("ignore")

# ---------------- Tunable defaults ----------------
TOTAL_COL = "Total"
URBAN_FLAG_COL = "Urban_rural_flag"   # Rural/Urban
RUC_COL = "RUC21CD"                   # RLF1/RLN1/RSF1/RSN1/UF1/UN1
AGE_6 = ['prop_0_14','prop_15_44','prop_45_64','prop_65_74','prop_75_84','prop_85_plus']
AGE_DROP = 'prop_15_44'

EXPOSURES = [
    # Each exposure can set its own percentile range (percentile_low to percentile_high)
    # {"name": "ah4leis",  "transform": "log1p",        "direction": "lower_better",  "percentile_low": 5,  "percentile_high": 98},  # Shorter = healthier
    {"name": "ah4gpas",  "transform": "logit_prop01", "direction": "higher_better", "percentile_low": 5,  "percentile_high": 95},  # NDVI higher = healthier
    # {"name": "ah4ffood", "transform": "log1p",        "direction": "higher_better", "percentile_low": 5,  "percentile_high": 95},  # Longer to fast-food = healthier
]

DISEASES = [
    'prevalence-CHD','prevalence-COPD','prevalence-DM','prevalence-HF',
    'prevalence-HYP','prevalence-OB','prevalence-PAD','prevalence-STIA'
]

GRID_POINTS = 200           # Number of curve grid points
BASELINE_QUANTILE = 0.25    # RR reference quantile (use 0.10/0.25 to avoid P50)
HIST_BINS = 30              # Histogram bins

# ---------------- Plot defaults ----------------
DEFAULT_FONT_FAMILY = "DejaVu Sans"   # Close to Arial on Linux
DEFAULT_LABEL_SIZE  = 18              # Axis/panel title size
DEFAULT_TICK_SIZE   = 14              # Tick label size
DEFAULT_PNONLIN_SIZE = 11            # P-nonlinearity text size
DEFAULT_HIST_COLOR  = "#B0BEC5"       # Match check-baseline-1103.py
DEFAULT_HIST_ALPHA  = 0.45            # Match check-baseline-1103.py (more visible)
DEFAULT_XLABEL_PAD  = 0.005           # X label padding
DEFAULT_YLABEL_PAD  = 0.005           # Y label padding
DEFAULT_XTICKS      = 5               # Number of X ticks
DEFAULT_REFLINE_COLOR = "#6C757D"    # Match check-baseline-1103.py
DEFAULT_REFLINE_WIDTH = 1.0          # Reference line width
DEFAULT_REFLINE_ALPHA = 1.0           # Reference line alpha

# ---------------- Y-axis range parameters (aligned with check-baseline-1103.py) ----------------
PEAK_PAD_FRAC = 0.20      # Expand beyond peak by 20% to avoid touching the top
BASE_TARGET = 0.40         # Place RR=1.0 around 40% of the y-span to keep symmetry

# ---------------- Visual style parameters (aligned with check-baseline-1103.py) ----------------
SHOW_CI = False
CI_ALPHA = 0.75           # CI alpha
LINE_WIDTH = 2.2          # Curve line width
LINE_COLOR = '#1565C0'    # Curve color
CI_COLOR = '#64B5F6'      # CI color
QUIET_CONFIG_LOGS = True  # Suppress verbose config prints
SHOW_RUN_SUMMARY = True   # Print brief per-run summary (n, q25/q75, IQR RR)

# ---------------- Terminal logging ----------------
# log_level:
#   0 = silent
#   1 = high-signal progress logs (recommended)
#   2 = extra diagnostics (still no covariates/coefficients)
LOG_LEVEL_DEFAULT = 1
LOG_LEVEL = LOG_LEVEL_DEFAULT

def log(msg: str, level: int = 1):
    if int(LOG_LEVEL) >= int(level):
        print(msg, flush=True)

# ---------------- Config helpers ----------------
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
            log(f"[warning] Failed to load config {path}: {exc}", level=1)
    return cfg

def fetch_dataframe_via_sparql(endpoint: Optional[str], template_path: Optional[str]) -> Optional[pd.DataFrame]:
    if not endpoint or not template_path or not os.path.exists(template_path):
        return None
    try:
        with open(template_path, "r") as f:
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
        rows = []
        for b in bindings:
            rows.append({k: v.get("value") for k, v in b.items()})
        return pd.DataFrame(rows)
    except Exception as exc:
        log(f"[warning] SPARQL fetch failed: {exc}", level=1)
        return None

def load_input_dataframe(data_path: str, sparql_cfg: Dict[str, Any], template_path: str) -> pd.DataFrame:
    endpoint = sparql_cfg.get("endpoint")
    df = fetch_dataframe_via_sparql(endpoint, template_path)
    if df is None or df.empty:
        log("[info] SPARQL source missing or empty; falling back to file paths config.", level=1)
        return pd.read_csv(data_path)
    rename_map = {k: v for k, v in SPARQL_COLUMN_RENAMES.items() if k in df.columns}
    if rename_map:
        df = df.rename(columns=rename_map)
    return df

# ---------------- X/Y tick config (based on check-baseline-1103.py) ----------------
# X ticks: per-exposure step
EXPOSURE_X_TICKSTEP = {
    'ah4gpas': 0.1,
    'ah4ffood': 2.0,
    'ah4leis': 2.0,
}

# Y ticks: default step if not overridden
Y_TICK_STEP = 0.1

# ---------------- Friendly X labels (shared) ----------------
EXPOSURE_LABELS = {
    "ah4leis":  "Drive time to the nearest leisure centre (min)",
    "ah4ffood": "Drive time to the nearest fast food retailer (min)",
    "ah4gpas":  "NDVI value indicating the green space (-)",
}

# ---------------- Diseases to plot per exposure (left-to-right) ----------------
# Edit here to change which diseases appear and their order
EXPOSURE_DISEASE_PANELS = {
    "ah4gpas": [  # NDVI
        'prevalence-DM',    # Diabetes
        'prevalence-OB',    # Obesity
        'prevalence-CHD',   # CHD
        'prevalence-COPD',  # COPD
    ],
    "ah4ffood": [  # Fast food drive time
        'prevalence-DM',    # Diabetes
        'prevalence-OB',    # Obesity
        'prevalence-CHD',   # CHD
        'prevalence-HF',    # HF
    ],
    "ah4leis": [  # Leisure centre drive time
        'prevalence-DM',    # Diabetes
        'prevalence-OB',    # Obesity
        'prevalence-HYP',   # Hypertension
        'prevalence-CHD',   # CHD
    ],
}

# ---------------- Fixed Config settings ----------------
# When enabled, skip combo_details.csv and use the preset below
FIXED_CONFIG = True        # Set True to enable fixed-config mode
# To use external config (aligned with check-baseline-1103.py), keep USE_EXTERNAL_CONFIG=True
USE_EXTERNAL_CONFIG = True
EXTERNAL_CONFIG_PATH = None
CUSTOM_X_LIMITS = {}  # Overridden by external config if provided
USE_IMD = False
USE_IMD_BY_EXPO = {}
IMD_COL_BY_EXPO = {}

# ---------- Default local paths for quick runs (`python3 Dose-response-copy.py`) ----------
RESULT_DIR          = os.path.join(os.path.dirname(__file__), "results")
DEFAULT_DATA_PATH   = None
DEFAULT_OUTDIR      = RESULT_DIR
DEFAULT_DETAILS_CSV = None  # Fill path if selecting best combos from combo_details

# IMD column mapping (aligned with check-baseline-1103.py)
IMD_COL_DEFAULT = 'Income Decile (where 1 is most deprived 10% of LSOAs)'
IMD_COL_MAPPING = {
    'ah4gpas': 'Geographical Barriers Sub-domain',
    'ah4leis': 'Income Score (rate)',
    'ah4ffood': 'Wider Barriers Sub-domain Score'
}
CUSTOM_X_LIMITS = {}
USE_IMD_BY_EXPO = {}
IMD_COL_BY_EXPO = {}
IMD_COL_DEFAULT = "Income Decile (where 1 is most deprived 10% of LSOAs)"
# Optional overrides; falls back to default names if external config lacks them
IMD_COL_MAPPING_FALLBACK = {
    "ah4gpas": "Geographical Barriers Sub-domain",
    "ah4leis": "Income Score (rate)",
    "ah4ffood": "Wider Barriers Sub-domain Score",
}

# ---------------- Config paths ----------------
CONFIG_DIR = os.path.join(os.path.dirname(__file__), "config")
DEFAULT_PATHS_CONFIG_PATH = os.path.join(CONFIG_DIR, "data_paths.json")
DEFAULT_SPARQL_CONFIG_PATH = os.path.join(CONFIG_DIR, "sparql_config.json")
DEFAULT_SPARQL_TEMPLATE_PATH = os.path.join(CONFIG_DIR, "sparql_template.rq")

DEFAULT_PATHS_CONFIG: Dict[str, Any] = {
    "data": None,
    "outdir": None,
    "details": None,
    "external_config": None
}

DEFAULT_SPARQL_CONFIG: Dict[str, Any] = {
    "endpoint": "http://localhost:3838/sparql/ui"
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

# Fixed-config preset (used when FIXED_CONFIG=True)
FIXED_PRESET = {
    "strata_mode": "none",         # Stratification: "none", "binary", "ruc_6cat", "age_structure"
    "age_mode": "6band",           # Age control: "ai", "6band"
    "urban_ctrl_mode": "categorical",  # Urban control: "none", "binary", "categorical"
    "use_air": False,              # Use air-quality controls?
    "use_health": False,            # Use healthcare access controls?
}

# ---------------- Utility functions ----------------
def _as_prop(series):
    x = pd.to_numeric(series, errors='coerce').astype(float)
    return pd.Series(np.where(x <= 1.0, x, x/100.0), index=series.index)

def exposure_transform(raw, mode):
    raw = pd.to_numeric(raw, errors='coerce').astype(float)
    if mode == "log1p":
        mn = np.nanmin(raw)
        adj = raw - mn if (mn < 0) else raw
        return pd.Series(np.log1p(adj), index=raw.index), raw
    elif mode == "logit_prop01":
        x = _as_prop(raw)
        x = np.clip(x, 0.001, 0.999)
        return pd.Series(np.log(x/(1-x)), index=raw.index), raw
    return pd.Series(raw, index=raw.index), raw

def _safe_zscore(s: pd.Series) -> pd.Series:
    s = pd.to_numeric(s, errors="coerce")
    m = s.mean(skipna=True)
    v = s.var(ddof=0, skipna=True)
    if not np.isfinite(v) or v <= 0:
        return pd.Series(0.0, index=s.index)
    return ((s - m) / np.sqrt(v)).fillna(0.0)

def build_air_quality_vars(df):
    cols = [c for c in ['ah4no2', 'ah4so2', 'ah4pm10'] if c in df.columns]
    if not cols: return pd.DataFrame(index=df.index)
    out = {f"air_{c}": _safe_zscore(pd.to_numeric(df[c], errors='coerce')) for c in cols}
    return pd.DataFrame(out, index=df.index)

def build_healthcare_access_vars(df):
    if 'ah4gp' not in df.columns:
        return pd.DataFrame(index=df.index)
    gp = pd.to_numeric(df['ah4gp'], errors='coerce').where(lambda s: s>=0)
    gp_log = np.log1p(gp)
    return pd.DataFrame({'healthcare_access': _safe_zscore(gp_log)}, index=df.index)

def build_imd_vars(df, imd_col_name: str):
    """
    IMD control variable: z-score the specified IMD column; if missing or too few valid rows, return empty DataFrame.
    """
    if not imd_col_name or imd_col_name not in df.columns:
        return pd.DataFrame(index=df.index)
    imd = pd.to_numeric(df[imd_col_name], errors='coerce').astype(float)
    if imd.notna().sum() < 50:
        return pd.DataFrame(index=df.index)
    return pd.DataFrame({"imd": _safe_zscore(imd)}, index=df.index)

def build_age_vars(df, mode='ai'):
    if mode == 'ai':
        p65 = pd.to_numeric(df.get('prop_65_74', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_75_84', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_85_plus', np.nan), errors='coerce')
        p014 = pd.to_numeric(df.get('prop_0_14', np.nan), errors='coerce')
        ai = (p65 / p014) * 100.0
        ai.replace([np.inf, -np.inf], np.nan, inplace=True)
        return pd.DataFrame({'age_index': ai.fillna(ai.median())}, index=df.index)
    # 6band
    age_data = pd.DataFrame({c: pd.to_numeric(df[c], errors='coerce').astype(float)
                             for c in AGE_6 if c in df.columns})
    if age_data.empty: return pd.DataFrame(index=df.index)
    if np.nanmax(age_data.to_numpy()) > 1.0:
        age_data = age_data / 100.0
    use_cols = [c for c in age_data.columns if c != AGE_DROP]
    return age_data[use_cols].add_prefix("age_").fillna(0.0)

def build_urban_control_vars(df, mode='none'):
    if mode == 'none': return pd.DataFrame(index=df.index)
    if mode == 'binary':
        if URBAN_FLAG_COL not in df.columns: return pd.DataFrame(index=df.index)
        s = (df[URBAN_FLAG_COL].astype('string').str.strip().str.lower()
             .map({'urban':'Urban','rural':'Rural'}))
        d = pd.get_dummies(pd.Categorical(s, categories=['Rural','Urban']),
                           drop_first=True, prefix='URB', dtype=float)
        return d
    if mode == 'categorical':
        if RUC_COL not in df.columns: return pd.DataFrame(index=df.index)
        r = df[RUC_COL].astype('string').str.strip().str.upper()
        cats = ['RLF1','RLN1','RSF1','RSN1','UF1','UN1']
        d = pd.get_dummies(pd.Categorical(r, categories=cats),
                           drop_first=False, prefix='RUC', dtype=float)
        if 'RUC_UN1' in d.columns: d = d.drop('RUC_UN1', axis=1)
        return d
    return pd.DataFrame(index=df.index)

def build_controls_from_combo(df, combo):
    """
    Build control covariates per combo (age/air/health/urban/IMD).
    Combo fields: age_mode, urban_ctrl_mode, use_air, use_health, use_imd
    """
    parts = []
    # 年龄
    parts.append(build_age_vars(df, combo.get('age_mode', '6band')))
    # 城乡
    urban_mode = combo.get('urban_ctrl_mode', 'none')
    if urban_mode not in ['n/a', None, 'none']:
        parts.append(build_urban_control_vars(df, urban_mode))
    # 空气
    if combo.get('use_air', False):
        parts.append(build_air_quality_vars(df))
    # 健康
    if combo.get('use_health', False):
        parts.append(build_healthcare_access_vars(df))
    # IMD
    if combo.get('use_imd', False):
        expo_name = combo.get('exposure')
        imd_col = IMD_COL_BY_EXPO.get(expo_name, IMD_COL_MAPPING.get(expo_name, IMD_COL_DEFAULT))
        parts.append(build_imd_vars(df, imd_col))
    # 合并
    parts = [p for p in parts if p is not None and not p.empty]
    if not parts:
        return pd.DataFrame(index=df.index)
    Xc = pd.concat(parts, axis=1)
    if Xc.columns.duplicated().any():
        Xc = Xc.loc[:, ~Xc.columns.duplicated()]
    return Xc

# IMD 变量
def build_imd_vars(df, imd_col_name: str):
    if not imd_col_name or imd_col_name not in df.columns:
        return pd.DataFrame(index=df.index)
    imd = pd.to_numeric(df[imd_col_name], errors='coerce').astype(float)
    if imd.notna().sum() < 50:
        return pd.DataFrame(index=df.index)
    return pd.DataFrame({"imd": _safe_zscore(imd)}, index=df.index)

def create_strata_series(df, mode):
    if mode == 'none': return None
    if mode == 'binary': return df[URBAN_FLAG_COL]
    if mode == 'ruc_6cat': return df[RUC_COL]
    if mode == 'age_structure':
        age_65plus = df['prop_65_74'] + df['prop_75_84'] + df['prop_85_plus']
        return np.where(age_65plus < 0.19, 'Young', 'Old')
    raise ValueError(f"Unknown stratification mode: {mode}")

def sanitize_inputs_for_glm(y, N, X):
    y = pd.to_numeric(y, errors="coerce").replace([np.inf, -np.inf], np.nan)
    N = pd.to_numeric(N, errors="coerce").replace([np.inf, -np.inf], np.nan)
    X = X.replace([np.inf, -np.inf], np.nan).copy()
    all_nan_cols = [c for c in X.columns if X[c].isna().all()]
    if all_nan_cols:
        X = X.drop(columns=all_nan_cols)
    row_ok = (N > 0) & y.notna() & X.notna().all(axis=1)
    if row_ok.sum() < 50:
        raise RuntimeError(f"Insufficient valid samples: {int(row_ok.sum())}")
    return y.loc[row_ok], N.loc[row_ok], X.loc[row_ok], row_ok

# ---------------- Tick helpers (based on check-baseline-1103.py) ----------------
def _make_ticks_from_limits(vmin, vmax, step):
    """
    Generate tick array from min/max and step.
    Returns numpy array or None if step is invalid.
    """
    if step is None or step <= 0:
        return None
    start = math.ceil(vmin / step) * step
    ticks = np.arange(start, vmax + 1e-9, step)
    ticks = ticks[(ticks >= vmin - 1e-12) & (ticks <= vmax + 1e-12)]
    return ticks

# ---------------- External config loader (aligned with check-baseline-1103.py) ----------------
def _load_external_config(path: str):
    """
    从 JSON 配置文件加载超参数，覆盖默认的可视化/截取/控制设置。
    仅在 USE_EXTERNAL_CONFIG=True 时调用。
    """
    import json
    with open(path, "r") as f:
        cfg = json.load(f)
    return cfg

def _apply_external_config(cfg: dict):
    """
    将外部配置写回全局变量（影响绘图和部分拟合配置）。
    仅在 USE_EXTERNAL_CONFIG=True 时调用。
    """
    global EXPOSURES, EXPOSURE_X_TICKSTEP, CUSTOM_X_LIMITS, Y_TICK_STEP
    global PEAK_PAD_FRAC, BASE_TARGET
    global CI_ALPHA, LINE_WIDTH, LINE_COLOR, CI_COLOR
    global DEFAULT_HIST_COLOR, DEFAULT_HIST_ALPHA
    global DEFAULT_REFLINE_COLOR, DEFAULT_REFLINE_WIDTH, DEFAULT_REFLINE_ALPHA
    global INDEPENDENT_Y, USE_IMD_BY_EXPO, IMD_COL_BY_EXPO, USE_IMD

    # Override exposure config (including percentile bounds)
    if "exposures" in cfg:
        EXPOSURES = cfg["exposures"]
        USE_IMD_BY_EXPO = {}
        IMD_COL_BY_EXPO = {}
        for e in EXPOSURES:
            name = e.get("name")
            if not name:
                continue
            USE_IMD_BY_EXPO[name] = bool(e.get("use_imd", False))
            if "imd_col" in e:
                IMD_COL_BY_EXPO[name] = e["imd_col"]

    # X 轴步长
    if "x_tickstep" in cfg:
        EXPOSURE_X_TICKSTEP = cfg["x_tickstep"]

    # 自定义 X 范围
    if "custom_x_limits" in cfg:
        # Applies only to plot/grid truncation (fit still uses percentile_low/high from exposure cfg)
        global CUSTOM_X_LIMITS
        CUSTOM_X_LIMITS = cfg["custom_x_limits"]

    # Y 轴刻度
    if "y_tick_step" in cfg:
        Y_TICK_STEP = cfg["y_tick_step"]

    # Y 轴范围形状参数
    if "peak_pad_frac" in cfg:
        PEAK_PAD_FRAC = cfg["peak_pad_frac"]
    if "base_target" in cfg:
        BASE_TARGET = cfg["base_target"]

    # 样式
    style = cfg.get("style", {})
    CI_ALPHA = style.get("ci_alpha", CI_ALPHA)
    LINE_WIDTH = style.get("line_width", LINE_WIDTH)
    LINE_COLOR = style.get("line_color", LINE_COLOR)
    CI_COLOR = style.get("ci_color", CI_COLOR)
    DEFAULT_HIST_COLOR = style.get("hist_color", DEFAULT_HIST_COLOR)
    DEFAULT_HIST_ALPHA = style.get("hist_alpha", DEFAULT_HIST_ALPHA)
    DEFAULT_REFLINE_COLOR = style.get("baseline_color", DEFAULT_REFLINE_COLOR)
    DEFAULT_REFLINE_WIDTH = style.get("baseline_lw", DEFAULT_REFLINE_WIDTH)
    DEFAULT_REFLINE_ALPHA = style.get("baseline_alpha", DEFAULT_REFLINE_ALPHA)

    # Y 轴独立/共享
    if "independent_y" in cfg:
        INDEPENDENT_Y = bool(cfg["independent_y"])

    # IMD 总开关
    if "use_imd" in cfg:
        USE_IMD = bool(cfg["use_imd"])

    # IQR 参考线开关（若后续需要可扩展）
    # show_iqr = cfg.get("show_iqr", None)  # IQR reference toggle (unused)

def _maybe_load_external_config():
    """
    如果 USE_EXTERNAL_CONFIG=True，则加载并应用外部配置。
    如果文件不存在或加载失败，将静默回退为默认配置。
    """
    if not USE_EXTERNAL_CONFIG:
        return
    try:
        cfg = _load_external_config(EXTERNAL_CONFIG_PATH)
        _apply_external_config(cfg)
        if not QUIET_CONFIG_LOGS:
            print("[config] Using external config file.")
    except Exception as e:
        print(f"[warning] Failed to load external config, fallback to defaults. Reason: {e}")

# Scoring: same as detail stage (fewer negatives, score=mean(PAF+)+20*R2 maximized)
def negativity_count(row):
    cnt = 0
    for k in ['paf_p50','paf_p25','paf_p10']:
        v = row.get(k, np.nan)
        if np.isfinite(v) and v < 0: cnt += 1
    return cnt

def score_block(df_block):
    if df_block.empty:
        return 10**9, -np.inf, -np.inf, -np.inf
    negs = df_block.apply(negativity_count, axis=1).sum()
    paf_pos = []
    paf_cols = ['paf_p50','paf_p25','paf_p10']
    available_paf_cols = [k for k in paf_cols if k in df_block.columns]
    if not available_paf_cols:
        paf_like_cols = [c for c in df_block.columns if 'paf' in c.lower()]
        if paf_like_cols:
            available_paf_cols = paf_like_cols[:3]  # take the first three candidates
        else:
            mean_paf = 0.0
            mean_r2 = float(df_block.get('mcfadden_r2', pd.Series([0.0])).mean(skipna=True)) if 'mcfadden_r2' in df_block.columns else 0.0
            score = mean_paf + 20.0*mean_r2
            return int(negs), mean_paf, mean_r2, score
    
    for k in available_paf_cols:
        if k in df_block.columns:
            paf_pos.append(np.clip(df_block[k], a_min=0, a_max=None))
    
    if not paf_pos:
        mean_paf = 0.0
    else:
        paf_pos = pd.concat(paf_pos, axis=0)
        mean_paf = float(paf_pos.mean(skipna=True))
    
    mean_r2 = float(df_block.get('mcfadden_r2', pd.Series([0.0])).mean(skipna=True)) if 'mcfadden_r2' in df_block.columns else 0.0
    score = mean_paf + 20.0*mean_r2
    return int(negs), mean_paf, mean_r2, score

def pick_best_from_details(df_details, exposure):
    df_exp = df_details[df_details['exposure']==exposure].copy()
    if df_exp.empty: return None
    keys = ['strata_mode','urban_ctrl_mode','age_mode','use_air','use_health']
    best = None
    for combo, df_block in df_exp.groupby(keys):
        negs, mean_paf, mean_r2, score = score_block(df_block)
        rec = dict(exposure=exposure,
                   strata_mode=combo[0], urban_ctrl_mode=combo[1], age_mode=combo[2],
                   use_air=combo[3], use_health=combo[4],
                   negatives=negs, mean_paf_pos=mean_paf, mean_r2=mean_r2, score=score)
        if (best is None) or (rec['negatives'] < best['negatives']) or \
           (rec['negatives']==best['negatives'] and rec['score']>best['score']):
            best = rec
    return best

# Fit + curve (single exposure, single disease, single stratum)
def fit_and_curve_one(df, exposure_cfg, combo, disease,
                      baseline_quantile=BASELINE_QUANTILE, grid_points=GRID_POINTS):
    expo = exposure_cfg['name']
    transform_mode = exposure_cfg['transform']
    direction = exposure_cfg.get('direction', 'lower_better')
    percentile_low = exposure_cfg.get('percentile_low', 5)
    percentile_high = exposure_cfg.get('percentile_high', 95)

    # Exposure
    x_raw_all = pd.to_numeric(df[expo], errors='coerce')
    x_tr_all, x_raw_scale = exposure_transform(x_raw_all, transform_mode)
    valid_mask = x_tr_all.notna()
    n_valid_expo = int(valid_mask.sum())
    if n_valid_expo < 50:
        log(f"      [skip] {expo} {disease}: valid exposure {n_valid_expo} < 50", level=2)
        return None
    df = df.loc[valid_mask].copy()
    x_tr = x_tr_all.loc[valid_mask]
    x_raw = x_raw_scale.loc[valid_mask]

    # Spline basis
    Xs = dmatrix("cr(z, df=4)", {"z": x_tr.values}, return_type="dataframe")
    design_info = Xs.design_info
    spline_cols = [f"EXPO_spline_{i+1}" for i in range(Xs.shape[1])]
    Xs.columns = spline_cols
    Xs.index = df.index

    # Linear term (for nonlinearity test)
    X_lin = pd.DataFrame({'EXPO_linear': x_tr.values}, index=df.index)

    # Controls per combo
    ctrl = [build_controls_from_combo(df, {**combo, "exposure": expo})]

    # Spline design matrix
    X_spline = pd.concat(ctrl + [Xs], axis=1)
    X_spline = sm.add_constant(X_spline, has_constant='add')

    # Linear design matrix (same controls + one linear term)
    X_linear = pd.concat(ctrl + [X_lin], axis=1)
    X_linear = sm.add_constant(X_linear, has_constant='add')

    if disease not in df.columns:
        log(f"      [skip] {expo} {disease}: column not found", level=2)
        return None

    prevalence = _as_prop(df[disease])
    population = pd.to_numeric(df[TOTAL_COL], errors='coerce').astype(float)
    y = prevalence * population

    try:
        y_fit, N_fit, X_fit_spline, row_ok = sanitize_inputs_for_glm(y, population, X_spline)
    except Exception as e:
        log(f"      [skip] {expo} {disease}: sanitize failed ({type(e).__name__}: {e})", level=2)
        return None
    X_fit_linear = X_linear.loc[row_ok]
    x_fit = x_raw.loc[row_ok]

    # 拟合：样条模型与线性模型
    try:
        res_spline = sm.GLM(y_fit, X_fit_spline, family=Poisson(), offset=np.log(N_fit)).fit(cov_type='HC1')
    except Exception:
        res_spline = sm.GLM(y_fit, X_fit_spline, family=Poisson(), offset=np.log(N_fit)).fit()
    try:
        res_linear = sm.GLM(y_fit, X_fit_linear, family=Poisson(), offset=np.log(N_fit)).fit(cov_type='HC1')
    except Exception:
        res_linear = sm.GLM(y_fit, X_fit_linear, family=Poisson(), offset=np.log(N_fit)).fit()

    # Null for R^2
    X_null = sm.add_constant(pd.DataFrame(index=y_fit.index), has_constant='add')
    res_null = sm.GLM(y_fit, X_null, family=Poisson(), offset=np.log(N_fit)).fit()

    # Nonlinearity test (LRT): spline vs linear
    df_diff = Xs.shape[1] - 1  # spline basis includes the linear term
    lr_stat = 2.0 * (res_spline.llf - res_linear.llf)
    p_nonlinear = 1.0 - chi2.cdf(max(lr_stat, 0.0), df=max(df_diff, 1))

    # Baseline on raw scale: effective quantile depends on direction
    q_eff = baseline_quantile if direction == 'lower_better' else (1.0 - baseline_quantile)
    ref_raw = float(np.nanpercentile(x_fit, q_eff * 100.0))
    if not np.isfinite(ref_raw):
        ref_raw = float(np.nanmedian(x_fit))

    # Grid on raw scale: prefer CUSTOM_X_LIMITS else percentile_low/high
    if expo in CUSTOM_X_LIMITS:
        p_lo, p_hi = CUSTOM_X_LIMITS[expo]
        x_min = float(np.nanpercentile(x_fit, p_lo))
        x_max = float(np.nanpercentile(x_fit, p_hi))
    else:
        x_min = float(np.nanpercentile(x_fit, percentile_low))
        x_max = float(np.nanpercentile(x_fit, percentile_high))
    if (not np.isfinite(x_min)) or (not np.isfinite(x_max)) or (x_min >= x_max):
        x_min = float(np.nanmin(x_fit)); x_max = float(np.nanmax(x_fit))
    if (not np.isfinite(x_min)) or (not np.isfinite(x_max)) or (x_min >= x_max):
        return None
    grid_raw = np.linspace(x_min, x_max, grid_points)

    def to_tr(v_raw):
        if transform_mode == "log1p":
            mn = np.nanmin(x_raw)  # shift by sample minimum for consistency
            return np.log1p(v_raw - mn if mn < 0 else v_raw)
        elif transform_mode == "logit_prop01":
            p = np.clip(np.where(v_raw<=1.0, v_raw, v_raw/100.0), 0.001, 0.999)
            return np.log(p/(1-p))
        return v_raw

    grid_tr = to_tr(grid_raw)
    ref_tr  = to_tr(np.array([ref_raw]))[0]

    # Use training design_info to build spline matrices (avoid single-point failure)
    Xs_grid = build_design_matrices([design_info], {"z": np.asarray(grid_tr)})[0]
    Xs_grid = pd.DataFrame(np.asarray(Xs_grid), columns=spline_cols, index=None)

    Xs_ref  = build_design_matrices([design_info], {"z": np.asarray([ref_tr])})[0]
    Xs_ref  = pd.DataFrame(np.asarray(Xs_ref), columns=spline_cols, index=None)

    # Build mean control profile from training X_fit_spline (common reference profile)
    ctrl_means = X_fit_spline.drop(columns=spline_cols+['const']).mean().to_dict()

    # Assemble grid design matrix
    grid_rows = []
    for i in range(len(grid_raw)):
        row = {**ctrl_means, **{c: float(Xs_grid.iloc[i][c]) for c in spline_cols}}
        row['const'] = 1.0
        grid_rows.append(row)
    X_grid = pd.DataFrame(grid_rows, columns=X_fit_spline.columns).astype(float)

    # Reference row
    ref_row = {**ctrl_means, **{c: float(Xs_ref.iloc[0][c]) for c in spline_cols}}
    ref_row['const'] = 1.0
    X_ref = pd.DataFrame([ref_row], columns=X_fit_spline.columns).astype(float)

    # Linear predictors and relative risk (log link)
    beta = res_spline.params.values
    V = res_spline.cov_params().values

    eta_grid = X_grid.values @ beta
    eta_ref  = X_ref.values  @ beta
    rr = np.exp(eta_grid - float(eta_ref))

    # CI via delta method: Var(η_grid - η_ref) = (Xg - Xr) V (Xg - Xr)^T diagonal
    D = X_grid.values - X_ref.values  # n x p
    var = np.einsum('ij,jk,ik->i', D, V, D)  
    se  = np.sqrt(np.maximum(var, 0))
    rr_lo = np.exp((eta_grid - float(eta_ref)) - 1.96*se)
    rr_hi = np.exp((eta_grid - float(eta_ref)) + 1.96*se)

    q25_raw = float(np.nanpercentile(x_fit, 25.0))
    q75_raw = float(np.nanpercentile(x_fit, 75.0))
    iqr_raw = float(q75_raw - q25_raw) if (np.isfinite(q25_raw) and np.isfinite(q75_raw)) else np.nan

    rr_iqr = rr_iqr_lo = rr_iqr_hi = np.nan
    rr_iqr_direction = ""
    if np.isfinite(q25_raw) and np.isfinite(q75_raw) and (q75_raw != q25_raw):
        zA = np.full(len(x_fit), q75_raw if exposure_cfg.get("direction") != "higher_better" else q25_raw)
        zB = np.full(len(x_fit), q25_raw if exposure_cfg.get("direction") != "higher_better" else q75_raw)
        zA_tr = to_tr(zA)
        zB_tr = to_tr(zB)
        Xs_A = build_design_matrices([design_info], {"z": np.asarray(zA_tr)})[0]
        Xs_B = build_design_matrices([design_info], {"z": np.asarray(zB_tr)})[0]
        Xs_A = pd.DataFrame(np.asarray(Xs_A), columns=spline_cols, index=X_fit_spline.index)
        Xs_B = pd.DataFrame(np.asarray(Xs_B), columns=spline_cols, index=X_fit_spline.index)
        X_A = X_fit_spline.copy()
        X_B = X_fit_spline.copy()
        for c in spline_cols:
            X_A[c] = Xs_A[c].values
            X_B[c] = Xs_B[c].values
            
        # Predict the mean outcome (mu_A) for each instance, using the first scenario (A), corresponding to exposure at one IQR extreme (typically Q75 or Q25).
        mu_A = res_spline.predict(X_A, offset=np.log(N_fit))
        # Likewise, predict the mean outcome for the second scenario (B), corresponding to exposure at the opposite IQR extreme (typically Q25 or Q75).
        mu_B = res_spline.predict(X_B, offset=np.log(N_fit))

        # Sum the predicted means to get the expected total outcome in each scenario.
        total_A = float(mu_A.sum())
        total_B = float(mu_B.sum())

        # Check that these totals are finite and positive to avoid invalid calculations.
        if np.isfinite(total_A) and np.isfinite(total_B) and total_B > 0 and total_A > 0:
            # Calculate the difference in log-totals, which is the log-relative-risk (prevalence ratio) between scenario A and scenario B.
            eta_diff = np.log(total_A) - np.log(total_B)

            # To estimate the uncertainty (variance) of eta_diff, we use the delta method.
            # Compute the gradient for each scenario: sum over rows of (predicted means * design matrix values).
            G_A = (mu_A.values[:, None] * X_A.values).sum(axis=0)
            G_B = (mu_B.values[:, None] * X_B.values).sum(axis=0)
            # Gradient of the log sum for each set, and take their difference (A - B).
            grad = G_A / total_A - G_B / total_B

            # Compute the variance of eta_diff (log-prevalence ratio) using the covariance matrix V from the fitted model.
            var_iqr = float(grad @ V @ grad)
            # Standard error is the square root of the variance.
            se_iqr = float(np.sqrt(max(var_iqr, 0.0)))

            # Calculate the relative risk (prevalence ratio) between A and B by exponentiating eta_diff.
            rr_iqr = float(np.exp(eta_diff))
            # Calculate lower and upper 95% confidence interval bounds for rr_iqr using +/- 1.96 standard errors.
            rr_iqr_lo = float(np.exp(eta_diff - 1.96 * se_iqr))
            rr_iqr_hi = float(np.exp(eta_diff + 1.96 * se_iqr))

            # Specify the direction of the relative risk depending on the exposure configuration.
            if exposure_cfg.get("direction") == "higher_better":
                rr_iqr_direction = "RR_IQR = total(Q25)/total(Q75)"  # Lower/higher direction (see exposure logic)
            else:
                rr_iqr_direction = "RR_IQR = total(Q75)/total(Q25)"  # Higher/lower direction

        # HOW IS THE PREVALENCE RATIO CALCULATED?
        # The prevalence ratio (or "relative risk" over the IQR) is the ratio of the predicted sum of cases 
        # (from the fitted Poisson-GLM) for two exposure scenarios: one at the upper IQR bound (Q75) and one at the lower (Q25). 
        # This is calculated as:
        #     RR_IQR = PredictedTotalCases(Q75) / PredictedTotalCases(Q25)
        # All covariates except the exposure of interest are held at their mean values, isolating the effect of shifting 
        # the exposure from Q25 -> Q75 (or vice versa, depending on direction). Uncertainty is estimated using the delta method
        # applied to the model's parameter covariance matrix.

    out = pd.DataFrame({
        "exposure": expo,
        "disease": disease,
        "direction": direction,
        "strata_mode": combo['strata_mode'],
        "stratum": "All",  # For strata-specific values, upper layer sets this
        "urban_ctrl_mode": combo['urban_ctrl_mode'],
        "age_mode": combo['age_mode'],
        "use_air": combo['use_air'],
        "use_health": combo['use_health'],
        "arg_baseline_quantile": baseline_quantile,
        "effective_q": q_eff,
        "baseline_value": ref_raw,
        "p_nonlinear": float(p_nonlinear),
        # IQR-based RR summary (constant across rows)
        "q25_value": q25_raw,
        "q75_value": q75_raw,
        "iqr_value": iqr_raw,
        "rr_iqr": rr_iqr,
        "rr_iqr_lo": rr_iqr_lo,
        "rr_iqr_hi": rr_iqr_hi,
        "rr_iqr_definition": rr_iqr_direction,
        "x_raw": grid_raw,
        "rr": rr,
        "rr_lo": rr_lo,
        "rr_hi": rr_hi,
        "n": int(len(y_fit)),
        "mcfadden_r2": float(1 - (res_spline.llf / res_null.llf) if res_null.llf != 0 else np.nan),
    })

    # Diagnostics for terminal logs (no covariate names / no coefficient tables)
    pearson_ratio = np.nan
    try:
        if getattr(res_spline, "df_resid", 0):
            pearson_ratio = float(res_spline.pearson_chi2 / res_spline.df_resid)
    except Exception:
        pearson_ratio = np.nan
    diag = {
        "n_stratum": int(len(df)),
        "n_valid_exposure": int(n_valid_expo),
        "n_fit": int(len(y_fit)),
        "baseline_value": float(ref_raw) if np.isfinite(ref_raw) else np.nan,
        "effective_q": float(q_eff) if np.isfinite(q_eff) else np.nan,
        "p_nonlinear": float(p_nonlinear) if np.isfinite(p_nonlinear) else np.nan,
        "mcfadden_r2": float(1 - (res_spline.llf / res_null.llf) if res_null.llf != 0 else np.nan),
        "pearson_ratio": pearson_ratio,
        "llf_full": float(getattr(res_spline, "llf", np.nan)),
        "llf_null": float(getattr(res_null, "llf", np.nan)),
    }
    return out, x_fit.values, ref_raw, q_eff, diag  # diag only for logging

def plot_panels(df_curve, x_hist, exposure_name, out_png,
                font_family=DEFAULT_FONT_FAMILY, label_size=DEFAULT_LABEL_SIZE,
                tick_size=DEFAULT_TICK_SIZE, hist_color=DEFAULT_HIST_COLOR,
                hist_alpha=DEFAULT_HIST_ALPHA, pnonlin_size=DEFAULT_PNONLIN_SIZE,
                xlabel_pad=DEFAULT_XLABEL_PAD, ylabel_pad=DEFAULT_YLABEL_PAD,
                xticks=DEFAULT_XTICKS,
                refline_color=DEFAULT_REFLINE_COLOR, refline_width=DEFAULT_REFLINE_WIDTH,
                refline_alpha=DEFAULT_REFLINE_ALPHA):
    """
    Plot up to 2x4 dose-response panels.
    Only presentation-level limits/ticks are changed; data are not clipped/altered.
    """
    # === Tick chooser: try small/medium/large sets, then symmetric expansion, else fallback ===
    def pick_rr_ticks(ymin, ymax):
        """
        Input: must cover true range [ymin, ymax]
        Output: (yticks_array, (ylo, yhi))
        Rules:
          - Prefer fixed three sets:
            small: 0.90, 0.95, 1.00, 1.05, 1.10 (step 0.05)
            medium: 0.80, 0.90, 1.00, 1.10, 1.20 (step 0.10)
            large: 0.60, 0.80, 1.00, 1.20, 1.40 (step 0.20)
          - If none cover: symmetric expansion around 1 (start step 0.20, +0.10 each loop) to 5 ticks;
          - Fallback: linear split into 5 points, force-insert 1.0; ensure ≥4 ticks.
        """
        small  = np.array([0.90, 0.95, 1.00, 1.05, 1.10])
        medium = np.array([0.80, 0.90, 1.00, 1.10, 1.20])
        large  = np.array([0.60, 0.80, 1.00, 1.20, 1.40])

        def covers(arr, lo, hi):
            return (arr[0] <= lo + 1e-12) and (arr[-1] >= hi - 1e-12)

        # 优先三档
        if covers(small, ymin, ymax):
            return small, (small[0], small[-1])
        if covers(medium, ymin, ymax):
            return medium, (medium[0], medium[-1])
        if covers(large, ymin, ymax):
            return large, (large[0], large[-1])

        # 对称扩展（以 1 为中心）
        step = 0.20
        for _ in range(15):  # expand at most 15 times
            arr = np.array([1 - 2*step, 1 - step, 1.0, 1 + step, 1 + 2*step])
            if covers(arr, ymin, ymax):
                return arr, (arr[0], arr[-1])
            step += 0.10

        # 兜底：线性等分 + 插入 1.0
        lo = float(ymin); hi = float(ymax)
        if not np.isfinite(lo) or not np.isfinite(hi) or lo >= hi:
            return medium, (medium[0], medium[-1])
        arr = np.linspace(lo, hi, num=5)
        if np.all(np.abs(arr - 1.0) > 1e-8):
            arr = np.sort(np.append(arr, 1.0))
            if arr.size > 6:
                # 去掉距离 1.0 最近的一个（保留 1.0）
                d = np.abs(arr - 1.0)
                idx = np.argsort(d)
                remove = idx[1]
                arr = np.delete(arr, remove)
        while arr.size < 4:
            arr = np.sort(np.append(arr, (arr[0] + arr[-1]) / 2))
        return arr, (arr[0], arr[-1])

    # Global font
    plt.rcParams["font.family"] = font_family

    # Shared axis labels
    friendly_label = EXPOSURE_LABELS.get(exposure_name, exposure_name)
    shared_ylabel  = "Relative Risk (vs baseline)"

    # Diseases to plot for this exposure (fallback to DISEASES)
    diseases_to_plot = EXPOSURE_DISEASE_PANELS.get(exposure_name, DISEASES)
    n_diseases = len(diseases_to_plot)

    # Layout adapts to number of diseases (max 4 per row)
    if n_diseases == 4:
        nrows, ncols = 1, 4
        figsize = (16, 4)  # 一行四个子图：宽度16，高度4
    elif n_diseases == 8:
        nrows, ncols = 2, 4
        figsize = (16, 8)  # 两行四列：宽度16，高度8
    else:
        # 自动计算合适的布局（尽量保持一行，如果超过4个则换行）
        ncols = min(4, n_diseases)  # cap at 4 columns per row
        nrows = int(np.ceil(n_diseases / ncols))
        figsize = (4 * ncols, 4 * nrows)  # 4x4 inches per subplot cell

    # Figure/subplots (each panel has its own Y ticks)
    fig, axes = plt.subplots(nrows, ncols, figsize=figsize, sharey=False)
    if n_diseases == 1:
        axes = [axes]
    else:
        axes = axes.ravel()

    try:
        effq = float(df_curve["effective_q"].iloc[0])
    except Exception:
        effq = BASELINE_QUANTILE

    xmin = float(df_curve["x_raw"].min())
    xmax = float(df_curve["x_raw"].max())
    x_hist_clip = x_hist[(x_hist >= xmin) & (x_hist <= xmax)] if x_hist is not None else None

    for i, dis in enumerate(diseases_to_plot):
        ax = axes[i]
        d = df_curve[df_curve['disease'] == dis]

        # Reference line and styling
        ax.axhline(1.0, ls='--', lw=refline_width, color=refline_color, alpha=refline_alpha)
        ax.tick_params(labelsize=tick_size)
        
        # Disease title
        ax.set_title(dis, fontsize=label_size, fontfamily=font_family)

        if d.empty:
            # Empty panel: show mid-range window for readability (display-only)
            yticks = np.array([0.80, 0.90, 1.00, 1.10, 1.20])
            ax.set_ylim(0.80, 1.20)
            ax.set_yticks(yticks)
            # If disease absent, hide this axis
            if dis not in df_curve['disease'].values:
                ax.axis('off')
            continue

        # --- Curve and CI (no data clipping) ---
        if SHOW_CI:
            ax.fill_between(d['x_raw'], d['rr_lo'], d['rr_hi'],
                           color=CI_COLOR, alpha=CI_ALPHA, linewidth=0)
        # 主曲线：使用统一的线宽和颜色
        ax.plot(d['x_raw'], d['rr'], lw=LINE_WIDTH, color=LINE_COLOR)

        # Histogram on right axis
        if x_hist_clip is not None and len(x_hist_clip) > 0:
            try:
                ax_hist = ax.twinx()
                ax_hist.hist(x_hist_clip, bins=HIST_BINS, alpha=hist_alpha, color=hist_color)
                ax_hist.set_yticks([])
            except Exception:
                pass

        # P-nonlinearity text
        try:
            pval = float(d["p_nonlinear"].iloc[0])
            pval_text = "P-nonlinearity < 0.0001" if pval < 1e-4 else f"P-nonlinearity = {pval:.3g}"
            if exposure_name == "ah4leis":
                x_pos, y_pos, va_align = 0.98, 0.02, "bottom"
            else:
                x_pos, y_pos, va_align = 0.98, 0.98, "top"
            ax.text(x_pos, y_pos, pval_text,
                    transform=ax.transAxes, ha="right", va=va_align,
                    fontsize=pnonlin_size, fontfamily=font_family,
                    bbox=dict(facecolor="white", edgecolor="none", alpha=0.7, pad=2.0))
        except Exception:
            pass

        # X axis: unified step per exposure
        ax.set_xlim(xmin, xmax)
        x_step = EXPOSURE_X_TICKSTEP.get(exposure_name, None)
        if x_step is not None:
            xticks_all = _make_ticks_from_limits(xmin, xmax, x_step)
            if xticks_all is not None and len(xticks_all) > 0:
                ax.set_xticks(xticks_all)
        else:
            # Fallback to the default MaxNLocator when no step is configured
            ax.xaxis.set_major_locator(MaxNLocator(nbins=xticks, prune=None))

        # Y axis: center around RR=1.0 for symmetric display
        y_min_data = float(np.nanmin([d['rr_lo'].min(), d['rr'].min()]))
        y_max_data = float(np.nanmax([d['rr_hi'].max(), d['rr'].max()]))

        # Same logic as check-baseline-1103.py:
        # 1) pad peak by PEAK_PAD_FRAC, 2) add inner padding, 3) center around 1.0 (BASE_TARGET=0.40 means 1.0 at 40% height)
        peak_pad = PEAK_PAD_FRAC * max(1e-6, y_max_data)
        local_max = y_max_data + peak_pad
        span_raw = max(1e-6, local_max - y_min_data)
        pad = max(0.02, 0.03 * span_raw)  # padding: at least 0.02 or 3% of span
        span = span_raw + 2 * pad
        y_min = 1.0 - BASE_TARGET * span  # center on 1.0 with 40% position
        y_max = y_min + span

        # Generate Y ticks with configured step
        yticks = _make_ticks_from_limits(y_min, y_max, Y_TICK_STEP)
        
        # If ticks are empty/too few, fallback using older logic
        if yticks is None or len(yticks) < 3:
            # choose ticks (small/medium/large → symmetric → fallback)
            yticks_old, (ylo, yhi) = pick_rr_ticks(y_min, y_max)
            # Ensure display range covers data (only expand)
            ylo = min(ylo, y_min_data)
            yhi = max(yhi, y_max_data)
            yticks = np.asarray(yticks_old, dtype=float)
            if yticks.size < 4:
                extra = np.linspace(ylo, yhi, num=4)
                yticks = np.unique(np.round(np.sort(np.append(yticks, extra)), 6))
            if not np.any(np.isclose(yticks, 1.0, atol=1e-8)):
                yticks = np.sort(np.append(yticks, 1.0))
            ax.set_ylim(ylo, yhi)
        else:
            # Ensure 1.0 included
            if not np.any(np.isclose(yticks, 1.0, atol=1e-8)):
                yticks = np.sort(np.append(yticks, 1.0))
            # Set y-range from computed bounds (already centered on 1.0)
            ax.set_ylim(y_min, y_max)

        # Set y-ticks
        ax.set_yticks(yticks)
        
        # Add y-grid lines for readability
        ax.grid(axis='y', linestyle='--', alpha=0.25)

    # Hide extra subplots if fewer diseases than slots
    for i in range(n_diseases, len(axes)):
        axes[i].axis('off')

    # Figure title + shared labels
    fig.suptitle(f"Dose-Response: {friendly_label} (baseline effective q={effq:.2f})",
                 y=0.98, fontsize=label_size+2, fontfamily=font_family)
    try:
        fig.supxlabel(friendly_label, fontsize=label_size, fontfamily=font_family)
        fig.supylabel(shared_ylabel, fontsize=label_size, fontfamily=font_family)
        plt.tight_layout(rect=[ylabel_pad, xlabel_pad, 1.00, 0.95])
    except Exception:
        fig.text(0.5, xlabel_pad/2, friendly_label, ha="center", va="center",
                 fontsize=label_size, fontfamily=font_family)
        fig.text(ylabel_pad/2, 0.5, shared_ylabel, ha="center", va="center", rotation="vertical",
                 fontsize=label_size, fontfamily=font_family)
        plt.tight_layout(rect=[ylabel_pad, xlabel_pad, 1.00, 0.95])

    plt.savefig(out_png, dpi=200)
    plt.close(fig)

# ---------------- 主流程 ----------------
def main():
    # Note: declare globals before first use (argparse defaults included)
    global USE_EXTERNAL_CONFIG, EXTERNAL_CONFIG_PATH

    ap = argparse.ArgumentParser()
    ap.add_argument("--details", required=False, default=None,
                    help="Path to combo_details.csv (overrides paths config)")
    ap.add_argument("--data",    required=False, default=None,
                    help="Raw data CSV path (overrides paths config)")
    ap.add_argument("--outdir",  required=False, default=None,
                    help="Output directory (overrides paths config)")
    ap.add_argument("--baseline-quantile", type=float, default=BASELINE_QUANTILE,
                    help="RR reference quantile (default 0.25; use 0.10 to avoid P50)")
    ap.add_argument("--grid-points", type=int, default=GRID_POINTS, help="Number of curve grid points")
    ap.add_argument("--fixed-config", action="store_true", 
                    help="Enable fixed-config mode using FIXED_PRESET; ignore combo_details.csv")
    ap.add_argument("--use-config", dest="use_config", action="store_true", default=USE_EXTERNAL_CONFIG,
                    help="Use external config (hyperparameters aligned with check-baseline-1103)")
    ap.add_argument("--config-path", dest="config_path", default=None,
                    help="External config path (overrides paths config)")
    ap.add_argument("--paths-config", dest="paths_config", default=DEFAULT_PATHS_CONFIG_PATH,
                    help="JSON config file containing data/output paths")
    ap.add_argument("--sparql-config", dest="sparql_config", default=DEFAULT_SPARQL_CONFIG_PATH,
                    help="JSON config file containing SPARQL endpoint settings")
    ap.add_argument("--sparql-template", dest="sparql_template", default=DEFAULT_SPARQL_TEMPLATE_PATH,
                    help="SPARQL query template path used to fetch input data")
    ap.add_argument("--log-level", type=int, default=LOG_LEVEL_DEFAULT, choices=[0,1,2],
                    help="Terminal verbosity: 0=silent, 1=progress, 2=extra diagnostics (no covariates)")
    args = ap.parse_args()

    paths_cfg = load_json_config(args.paths_config, DEFAULT_PATHS_CONFIG)
    sparql_cfg = load_json_config(args.sparql_config, DEFAULT_SPARQL_CONFIG)

    data_path = args.data or paths_cfg.get("data") or DEFAULT_DATA_PATH
    outdir = args.outdir or paths_cfg.get("outdir") or DEFAULT_OUTDIR
    details_path = args.details if args.details is not None else paths_cfg.get("details", DEFAULT_DETAILS_CSV)
    config_path = args.config_path or paths_cfg.get("external_config") or EXTERNAL_CONFIG_PATH

    if not data_path:
        raise ValueError("Data path is required (set in data_paths.json or --data).")

    # Load external config (plot + some fit hyperparams), enabled by default
    USE_EXTERNAL_CONFIG = args.use_config
    EXTERNAL_CONFIG_PATH = config_path
    global LOG_LEVEL
    LOG_LEVEL = int(args.log_level)
    _maybe_load_external_config()

    os.makedirs(outdir, exist_ok=True)

    # Decide whether to use fixed config
    use_fixed_config = args.fixed_config or FIXED_CONFIG
    # External config overrides fixed/details
    if USE_EXTERNAL_CONFIG:
        use_fixed_config = False
    
    # Read details and pick best combo (only when not fixed/external)
    if not use_fixed_config and not USE_EXTERNAL_CONFIG:
        if details_path is None:
            raise ValueError("--details required when not using fixed/external config")
        df_details = pd.read_csv(details_path)
    else:
        df_details = None
        if not QUIET_CONFIG_LOGS:
            print("[fixed-config] Using FIXED_PRESET from code; ignoring combo_details.csv")
            print(f"[fixed-config] Settings: {FIXED_PRESET}")
    
    # Read raw data
    df_raw = load_input_dataframe(data_path, sparql_cfg, args.sparql_template)
    log("=== Dose-response run ===", level=1)
    log(f"[args] data={data_path}", level=1)
    log(f"[args] outdir={outdir}", level=1)
    log(f"[args] baseline_quantile={float(args.baseline_quantile):.3g} grid_points={int(args.grid_points)} log_level={int(LOG_LEVEL)}", level=1)
    log(f"[data] rows={int(len(df_raw))} cols={int(df_raw.shape[1])}", level=1)
    log(f"[mode] external_config={bool(USE_EXTERNAL_CONFIG)} fixed_config={bool(use_fixed_config)}", level=1)

    # Normalize six-band age proportions if given as shares
    if set(AGE_6).issubset(df_raw.columns):
        age_df = df_raw[AGE_6].apply(pd.to_numeric, errors='coerce').astype(float)
        s = age_df.sum(axis=1)
        bad = (s > 0) & (np.abs(s - 1.0) > 1e-4)
        if bad.any():
            df_raw.loc[bad, AGE_6] = age_df[bad].div(s[bad], axis=0).values

    all_curves = []
    for exp_cfg in EXPOSURES:
        exp = exp_cfg['name']
        log(f"\n=== Exposure: {exp} ===", level=1)
        log(f"[exposure] transform={exp_cfg.get('transform')} direction={exp_cfg.get('direction')} "
            f"percentile_low={exp_cfg.get('percentile_low', 5)} percentile_high={exp_cfg.get('percentile_high', 95)}", level=1)
        diseases_to_plot = EXPOSURE_DISEASE_PANELS.get(exp, DISEASES)
        log(f"[exposure] diseases={len(diseases_to_plot)} panels", level=2)
        
        # Choose config by mode
        if USE_EXTERNAL_CONFIG:
            # External config: use controls from JSON, aligned with check-baseline-1103.py
            best = {
                "exposure": exp,
                "strata_mode": exp_cfg.get("strata_mode", "none"),
                "urban_ctrl_mode": exp_cfg.get("urban_ctrl_mode", "categorical"),
                "age_mode": exp_cfg.get("age_mode", "6band"),
                "use_air": exp_cfg.get("use_air", False),
                "use_health": exp_cfg.get("use_health", False),
                "use_imd": exp_cfg.get("use_imd", USE_IMD or USE_IMD_BY_EXPO.get(exp, False)),
            }
            if not QUIET_CONFIG_LOGS:
                print(f"[config] Exposure {exp} uses external config: {best}")
        elif use_fixed_config:
            # Use fixed config
            best = {
                "exposure": exp,
                "strata_mode": FIXED_PRESET["strata_mode"],
                "urban_ctrl_mode": FIXED_PRESET["urban_ctrl_mode"],
                "age_mode": FIXED_PRESET["age_mode"],
                "use_air": FIXED_PRESET["use_air"],
                "use_health": FIXED_PRESET["use_health"],
                "use_imd": False,
            }
            if not QUIET_CONFIG_LOGS:
                print(f"[fixed-config] Exposure {exp} uses preset: {best}")
        else:
            # Pick best combo from combo_details.csv
            best = pick_best_from_details(df_details, exp)
            if best is None:
                print(f"[skip] {exp}: no record in combo_details.csv")
                continue
            best["use_imd"] = False
            print(f"[selected] Best combo for {exp}: {best}")

        # Always print the *high-level* adjustment settings (never list covariate names)
        log(f"[adjust] strata_mode={best.get('strata_mode')} urban_ctrl_mode={best.get('urban_ctrl_mode')} "
            f"age_mode={best.get('age_mode')} use_air={bool(best.get('use_air'))} "
            f"use_health={bool(best.get('use_health'))} use_imd={bool(best.get('use_imd', False))}", level=1)

        # Stratify + urban control handling
        strata = create_strata_series(df_raw, best['strata_mode'])
        strata_levels = ['All'] if strata is None else [lvl for lvl in pd.Series(strata).dropna().unique()]
        strata_data = {'All': df_raw} if strata is None else {lvl: df_raw[strata == lvl] for lvl in strata_levels}

        # Plot every stratum; record stratum in CSV
        for level in strata_levels:
            df_sub = strata_data[level]
            if len(df_sub) < 100:
                log(f"  [skip] stratum={level}: n={int(len(df_sub))} < 100", level=1)
                continue
            log(f"  --- Stratum: {level} (n={int(len(df_sub))}) ---", level=1)

            # Fit curve per disease
            curves_this_exp = []
            hist_source = None
            for dis in DISEASES:
                combo = best.copy()
                # Urban control only used for none/age_structure; otherwise 'n/a'
                if best['strata_mode'] not in {'none','age_structure'}:
                    combo['urban_ctrl_mode'] = 'n/a'

                res = fit_and_curve_one(df_sub, exp_cfg, combo, dis,
                                        baseline_quantile=args.baseline_quantile,
                                        grid_points=args.grid_points)
                if res is None:
                    continue
                curve_df, x_hist, ref_val, q_eff, diag = res
                curve_df['stratum'] = level  # mark stratum
                curves_this_exp.append(curve_df)
                if hist_source is None:
                    hist_source = x_hist

                if SHOW_RUN_SUMMARY:
                    try:
                        rr_iqr = float(curve_df["rr_iqr"].iloc[0])
                        q25v = float(curve_df["q25_value"].iloc[0])
                        q75v = float(curve_df["q75_value"].iloc[0])
                        n_obs = int(curve_df["n"].iloc[0])
                        # High-signal one-liner (no covariates)
                        pnon = float(curve_df["p_nonlinear"].iloc[0])
                        r2 = float(curve_df["mcfadden_r2"].iloc[0])
                        bval = float(curve_df["baseline_value"].iloc[0])
                        eq = float(curve_df["effective_q"].iloc[0])
                        log(f"    [run] dis={dis} n_fit={n_obs} baseline(q={eq:.2f})={bval:.4g} "
                            f"q25={q25v:.4g} q75={q75v:.4g} rr_iqr={rr_iqr:.4g} "
                            f"p_nonlin={pnon:.3g} r2={r2:.3g}", level=1)
                        # Extra diagnostics if requested
                        if int(LOG_LEVEL) >= 2 and isinstance(diag, dict):
                            pr = diag.get("pearson_ratio", np.nan)
                            llf = diag.get("llf_full", np.nan)
                            lln = diag.get("llf_null", np.nan)
                            log(f"      [diag] n_stratum={diag.get('n_stratum')} n_valid_exposure={diag.get('n_valid_exposure')} "
                                f"pearson_chi2/df={pr:.3g} llf_full={llf:.4g} llf_null={lln:.4g}", level=2)
                    except Exception:
                        pass

            if not curves_this_exp:
                continue

            curves_this_exp = pd.concat(curves_this_exp, ignore_index=True)
            all_curves.append(curves_this_exp)

            # 保存 CSV（按暴露+层）
            out_csv = os.path.join(outdir, f"dose_response_{exp}_stratum-{level}.csv")
            curves_this_exp.to_csv(out_csv, index=False)
            print(f"[saved] dose_response_{exp}_stratum-{level}.csv")

            # 出图（8面板）
            out_png = os.path.join(outdir, f"dose_response_{exp}_stratum-{level}.png")
            plot_panels(curves_this_exp, hist_source, exp, out_png)
            print(f"[saved] dose_response_{exp}_stratum-{level}.png")

    if all_curves:
        df_all = pd.concat(all_curves, ignore_index=True)
        out_csv_all = os.path.join(outdir, "dose_response_all.csv")
        df_all.to_csv(out_csv_all, index=False)
        print("[saved] dose_response_all.csv")
    else:
        print("No curves generated (data may be insufficient or details empty).")

if __name__ == "__main__":
    main()
