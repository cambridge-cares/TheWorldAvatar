# -*- coding: utf-8 -*-
import os
import json
from typing import Any, Dict, Optional, Tuple, Union, List
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.colors import Normalize, PowerNorm, LogNorm
from matplotlib.lines import Line2D
from pandas.api.types import is_numeric_dtype, CategoricalDtype
import re
import hashlib
import requests

# Disable automatic layout to prevent panels from being squeezed due to different label lengths
# This ensures consistent width per column and height per row
plt.rcParams['figure.constrained_layout.use'] = False

# ============== I/O ==============
RESULT_DIR = os.path.join(os.path.dirname(__file__), "results")
csv_path = None
base_out = RESULT_DIR

CONFIG_DIR = os.path.join(os.path.dirname(__file__), "config")
DEFAULT_PATHS_CONFIG_PATH = os.path.join(CONFIG_DIR, "data_paths.json")
DEFAULT_SPARQL_CONFIG_PATH = os.path.join(CONFIG_DIR, "sparql_config.json")
DEFAULT_SPARQL_TEMPLATE_PATH = os.path.join(CONFIG_DIR, "sparql_template.rq")
DEFAULT_SPARQL_TEMPLATES_PATH = os.path.join(CONFIG_DIR, "sparql_templates.json")

DEFAULT_PATHS_CONFIG: Dict[str, Any] = {
    "data": None,
    "outdir": None
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

# ============== Hyperparameters (Main configuration area) ==============
# ——Plot grid (rows × columns per figure)——
PANEL_ROWS = 2
PANEL_COLS = 4
# Single panel size (inches)
PANEL_W_INCH = 4.5
PANEL_H_INCH = 4.0

# ——Panel spacing & canvas margins——
LEFT_MARGIN   = 0.07
RIGHT_MARGIN  = 0.98
BOTTOM_MARGIN = 0.08
TOP_MARGIN    = 0.90
WSPACE        = 0.24
HSPACE        = 0.12

# ——Correlation coefficient display control——
SHOW_CORR_IN_TITLE = False   # True: show correlation and p-value in panel title; False: print to console, remove from figure


# ——Environment variables to plot (X)——
ENV_VARS = ['ah4gpas']  # Fast food, NDVI, Leisure centers

# ——GP accessibility as control variable——
GP_CONTROL_COL       = 'ah4gp'
CONTROL_GP_FOR_CORR  = True
CONTROL_GP_FOR_LINE  = True

# ——IMD deprivation index as control variable——
IMD_CONTROL_COL      = 'imd'
CONTROL_IMD_FOR_CORR = True        # control all IMD coluns covering education, income, employment, et al
CONTROL_IMD_FOR_LINE = True        

# ——Extra control variables (e.g., air pollution, IMD sub-domains)——
# EXTRA_CTRL_COLS = ['ah4no2']        # Examples: [], ['ah4no2'], ['ah4pm10'], ['ah4no2','ah4pm10']
EXTRA_CTRL_COLS = [
    'ah4no2',
    'ah4pm10'
    # 'Income Score (rate)',
    # 'Education, Skills and Training Score',
    # 'Employment Score (rate)',
    # 'Wider Barriers Sub-domain Score',
    # 'Outdoors Sub-domain Score'
]  # IMD sub-domain scores for sensitivity analysis
CONTROL_EXTRA_FOR_CORR = True       # Whether to control these extra columns in partial correlation/correlation
CONTROL_EXTRA_FOR_LINE = True       # Whether to control these extra columns in fitted line
RANK_EXTRA_CONTROLS   = False        # Rank extra controls in Spearman/partial_spearman (for non-0/1 variables)

# ——Disease list to plot (Y)——
DISEASE_VARS = [
    'prevalence-DM','prevalence-OB','prevalence-HYP','prevalence-CHD',
    'prevalence-STIA','prevalence-PAD','prevalence-HF',
    'prevalence-COPD'
]
DISEASE_FULL_NAMES = {
    'prevalence-DM': 'Diabetes',
    'prevalence-OB': 'Obesity',
    'prevalence-HYP': 'Hypertension',
    'prevalence-CHD': 'CHD',
    'prevalence-STIA': 'Stroke/TIA',
    'prevalence-PAD': 'PAD',
    'prevalence-HF': 'HF',
    'prevalence-COPD': 'COPD',
    'prevalence-AF': 'AF',
    'prevalence-MH': 'MH'
}
DISEASE_ABBR = {
    'prevalence-DM':'DM','prevalence-OB':'OB','prevalence-HYP':'HYP','prevalence-CHD':'CHD',
    'prevalence-AF':'AF','prevalence-STIA':'STIA','prevalence-PAD':'PAD','prevalence-HF':'HF',
    'prevalence-COPD':'COPD','prevalence-MH':'MH'
}

# Age control mode: 'single' | '4band' | '6band' | 'ai'
AGE_CTRL_MODE      = '6band'
AGE_GROUP_KEY      = 'G_65_plus'
AGE_REF_DROP_4     = 'age_15_44'
AGE_REF_DROP_6     = 'prop_15_44'

# Percentile trimming
TRIM_X_FRAC: Tuple[float, float] = (0.00, 0.9999)
TRIM_Y_FRAC: Tuple[float, float] = (0.00, 0.9999)

# Correlation coefficient type (default: partial Spearman)
CORR_TYPE           = 'partial_pearson'

# When partial Spearman correlation |ρₚ| <= threshold, draw horizontal line as zero correlation
APPLY_ZERO_CORR_FOR_PARTIAL_SPEARMAN = True
ZERO_CORR_ABS_THRESHOLD = 0.01

# Control variable selection
CONTROL_AGE_FOR_CORR   = True
CONTROL_URBAN_FOR_CORR = False
CONTROL_AGE_FOR_LINE   = True
CONTROL_URBAN_FOR_LINE = False

# ——Urban-rural control mode——
URBAN_MODE             = 'binary'          # 'categorical' | 'binary'
URBAN_CATEGORICAL_COL  = 'RUC21CD'
URBAN_BINARY_COL       = 'Urban_rural_flag'

# Color (Ageing Index: 65+/0–14 ×100)
COLOR_CMAP         = 'plasma'
COLOR_SCALE        = 'power'           # 'linear'|'power'|'log'
COLOR_GAMMA        = 0.6
COLOR_PCTLS        = (1, 99)

# ——Scatter and line (urban-rural grouping style)——
URBAN_MARKER_SIZE  = 36
RURAL_MARKER_SIZE  = 36
SCATTER_ALPHA      = 0.85
SCATTER_EDGE_COLOR = 'k'
SCATTER_EDGE_WIDTH = 0.2
SPLIT_URBAN_MARKERS = True
URBAN_MARKER       = 'o'
RURAL_MARKER       = '^'

# Partial correlation fitted line style
PARTIAL_LINE_STYLE = '-'
PARTIAL_LINE_COLOR = 'orange'
PARTIAL_LINE_WIDTH = 2.5
PARTIAL_LINE_ALPHA = 0.9

# Raw correlation line
SHOW_RAW_FIT_LINE  = False
RAW_LINE_STYLE     = '--'
RAW_LINE_COLOR     = 'gray'
RAW_LINE_WIDTH     = 2.0
RAW_LINE_ALPHA     = 0.9

# Partial residual median line
SHOW_PARTIAL_MEDIAN_LINE = False
MEDIAN_BINS              = 20

# === Zero correlation reference line and fill ===
SHOW_REFERENCE_ZERO_LINE      = True
REF_LINE_STYLE                = '--'
REF_LINE_COLOR                = 'black'
REF_LINE_WIDTH                = 1.2
REF_LINE_ALPHA               = 0.85

FILL_BETWEEN_REF_AND_PARTIAL  = True
FILL_COLOR                    = 'gray'
FILL_ALPHA                    = 0.6

# Statistical annotation box (bottom-right corner of each panel)
ANNOTATE_STATS_BOX   = True
ANNO_FONT_SIZE       = 16
ANNO_TEXT_COLOR      = 'black'
ANNO_BOX_FACE        = 'white'
ANNO_BOX_EDGE        = '0.8'
ANNO_BOX_ALPHA       = 0.9
ANNO_BOX_PAD         = 0.2
ANNO_POS_X_FRAC      = 0.98
ANNO_POS_Y_FRAC      = 0.02

# Axes and grid
SHARE_X            = True
SHARE_Y            = False
Y_PAD_FRAC         = 0.03
TICK_SIZE          = 16
GRID_COLOR         = '0.92'
GRID_WIDTH         = 1.0

# ——Population weighting (Total column)——
USE_POP_WEIGHT   = True
WEIGHT_COL       = 'Total'
WEIGHT_POLICY    = 'full'            # 'full' | 'y_only' | 'none'
WEIGHT_SCALING   = 'identity'        # 'identity' | 'sqrt' | 'log1p'
WEIGHT_MIN       = 1e-6

# Export image resolution (dpi)
SAVE_DPI         = 600

# p-value display configuration
P_SCI_SIG_DIGITS  = 3
P_MIN_DISPLAY     = 1e-300
RHO_LABEL_THRESHOLD = 0.10

# Axes and labels
ENV_LABELS = {
    "ah4gp":"Drive-time to nearest GP Practice (min)",
    "ah4leis":"Drive-time to nearest Leisure Centre (min)",
    "ah4ffood":"Drive-time to nearest\nFast Food Outlet (min)",
    "ah4gpas":"NDVI value indicating Green Space (-)",
    "ah4no2":"NO₂ concentration (µg/m³)",
    "ah4so2":"SO₂ concentration (µg/m³)",
    "ah4pm10":"PM₁₀ concentration (µg/m³)",
    "ah4hosp":"Drive-time to nearest Hospital (min)",
    "ah4phar":"Drive-time to nearest Pharmacy (min)",
}
AGE_GROUP_DEFS = {
    'G_0_14':    (['prop_0_14'],                         'Aged 0–14 years'),
    'G_15_44':   (['prop_15_44'],                        'Aged 15–44 years'),
    'G_45_64':   (['prop_45_64'],                        'Aged 45–64 years'),
    'G_65_plus': (['prop_65_74','prop_75_84','prop_85_plus'], 'Aged ≥65 years'),
}

# ============== Style ==============
plt.rcParams['font.family']    = ['DejaVu Sans', 'DejaVu Sans']
plt.rcParams['axes.titlesize'] = 16
plt.rcParams['axes.labelsize'] = 16
plt.rcParams['xtick.labelsize']= TICK_SIZE
plt.rcParams['ytick.labelsize']= TICK_SIZE

# Custom Y-axis tick format
def _format_y_tick(val, pos):
    import math
    if not np.isfinite(val):
        return ""
    x = float(val)
    if abs(x) < 1e-12:
        return "0"
    a = abs(x)
    e = math.floor(math.log10(a))
    scale = 10 ** (e - 1)
    y = a / scale
    y_rounded = math.floor(y + 0.5)
    result = y_rounded * scale
    if x < 0:
        result = -result
    a2 = abs(result)
    if a2 < 1e-12:
        return "0"
    if a2 >= 10:
        return f"{int(round(result))}"
    if a2 >= 1:
        return f"{result:.1f}"
    s = f"{result:.10f}".rstrip('0').rstrip('.')
    if s in ("0", "-0"):
        return "0"
    return s

# X-axis tick format
def _format_x_tick(val, pos):
    if not np.isfinite(val):
        return ""
    x = float(val)
    if abs(x) < 1e-12:
        return "0"
    return f"{x:g}"

# ============== Age/Urban-rural construction ==============
def prevalence_to_pct(series: pd.Series) -> pd.Series:
    s = pd.to_numeric(series, errors='coerce').astype(float)
    return s*100.0 if s.max(skipna=True) is not None and s.max(skipna=True) <= 1.0 else s

def compute_age_share_pct(df: pd.DataFrame, age_key: str) -> pd.Series:
    cols, _ = AGE_GROUP_DEFS[age_key]
    share = pd.Series(0.0, index=df.index, dtype=float)
    for c in cols:
        share = share.add(pd.to_numeric(df[c], errors='coerce'), fill_value=0.0)
    return share * 100.0

def compute_ageing_index(df: pd.DataFrame) -> pd.Series:
    p65  = compute_age_share_pct(df, 'G_65_plus')
    p014 = compute_age_share_pct(df, 'G_0_14')
    ai = (p65 / p014) * 100.0
    ai.replace([np.inf, -np.inf], np.nan, inplace=True)
    return ai

def build_age_controls(df: pd.DataFrame) -> pd.DataFrame:
    if AGE_CTRL_MODE in ('ai', 'ageing_index', 'aging_index', 'ageingindex'):
        ai = compute_ageing_index(df)
        return ai.rename('__AGEING_INDEX__').to_frame()
    if AGE_CTRL_MODE == 'single':
        return compute_age_share_pct(df, AGE_GROUP_KEY).rename('__AGE__').to_frame()
    if AGE_CTRL_MODE == '4band':
        df_age = pd.DataFrame({
            'age_0_14':   pd.to_numeric(df['prop_0_14'],  errors='coerce')*100.0,
            'age_15_44':  pd.to_numeric(df['prop_15_44'], errors='coerce')*100.0,
            'age_45_64':  pd.to_numeric(df['prop_45_64'], errors='coerce')*100.0,
            'age_65_plus':(pd.to_numeric(df['prop_65_74'],errors='coerce')
                          +pd.to_numeric(df['prop_75_84'],errors='coerce')
                          +pd.to_numeric(df['prop_85_plus'],errors='coerce'))*100.0
        })
        drop_col = AGE_REF_DROP_4 if AGE_REF_DROP_4 in df_age.columns else df_age.columns[1]
        return df_age.drop(columns=[drop_col])
    df_age6 = pd.DataFrame({
        'prop_0_14':   pd.to_numeric(df['prop_0_14'],   errors='coerce')*100.0,
        'prop_15_44':  pd.to_numeric(df['prop_15_44'],  errors='coerce')*100.0,
        'prop_45_64':  pd.to_numeric(df['prop_45_64'],  errors='coerce')*100.0,
        'prop_65_74':  pd.to_numeric(df['prop_65_74'],  errors='coerce')*100.0,
        'prop_75_84':  pd.to_numeric(df['prop_75_84'],  errors='coerce')*100.0,
        'prop_85_plus':pd.to_numeric(df['prop_85_plus'],errors='coerce')*100.0,
    })
    drop_col = AGE_REF_DROP_6 if AGE_REF_DROP_6 in df_age6.columns else df_age6.columns[1]
    return df_age6.drop(columns=[drop_col])

# ——Urban-rural column reading (based on URBAN_MODE)——
def _normalize_binary_urban(ser: pd.Series) -> pd.Series:
    s = ser.astype('string').str.strip().str.lower()
    mapped = s.map({'urban': 'Urban', 'rural': 'Rural'})
    cat = pd.Categorical(mapped, categories=['Rural','Urban'], ordered=False)
    return pd.Series(cat, index=ser.index, name=ser.name)

def detect_urban_series(df: pd.DataFrame) -> Optional[pd.Series]:
    if URBAN_MODE == 'binary':
        if URBAN_BINARY_COL in df.columns:
            ser = _normalize_binary_urban(df[URBAN_BINARY_COL]); return ser
        else:
            return None
    if URBAN_CATEGORICAL_COL in df.columns:
        return df[URBAN_CATEGORICAL_COL]
    return None

def prepare_urban(ser: Optional[pd.Series]) -> Optional[pd.Series]:
    if ser is None: return None
    s = ser.copy()
    if not isinstance(s.dtype, CategoricalDtype):
        if is_numeric_dtype(s) and s.nunique(dropna=True) <= 100:
            s = s.astype('Int64').astype('category')
        else:
            s = s.astype('string').astype('category')
    return s

# ============== Weighting/Ranking utility functions ==============
def _prep_weights(w: pd.Series) -> pd.Series:
    w = pd.to_numeric(w, errors='coerce').astype(float).clip(lower=0)
    if WEIGHT_SCALING == 'sqrt':
        w = np.sqrt(w)
    elif WEIGHT_SCALING == 'log1p':
        w = np.log1p(w)
    w = w.replace([np.inf, -np.inf], np.nan)
    fill_val = np.nanmedian(w) if np.isfinite(np.nanmedian(w)) else 1.0
    w = w.fillna(fill_val)
    return w.clip(lower=WEIGHT_MIN)

def _is_binary_dummy(s: pd.Series) -> bool:
    x = pd.to_numeric(s, errors='coerce')
    vals = pd.unique(x.dropna())
    if len(vals) == 0: return False
    try:
        as_int = np.unique(np.round(vals).astype(int))
        return set(as_int).issubset({0,1}) and len(as_int) <= 2
    except Exception:
        return False

def weighted_rank(s: pd.Series, w: pd.Series) -> pd.Series:
    s = pd.to_numeric(s, errors='coerce').astype(float)
    w = _prep_weights(w).reindex(s.index)
    df = pd.DataFrame({'v': s, 'w': w}).replace([np.inf, -np.inf], np.nan).dropna()
    out = pd.Series(np.nan, index=s.index, dtype=float)
    if df.empty: return out
    grp = df.groupby('v', sort=True)['w'].sum()
    cum = grp.cumsum(); total = grp.sum()
    ridit_map = (cum.shift(fill_value=0.0) + 0.5*grp) / (total if total>0 else np.nan)
    out.loc[df.index] = df['v'].map(ridit_map).astype(float).values
    return out

def weighted_pearson(a: np.ndarray, b: np.ndarray, w: np.ndarray) -> float:
    w = np.asarray(w, float); a = np.asarray(a, float); b = np.asarray(b, float)
    if a.size<2 or b.size<2 or w.size<2: return float('nan')
    W = w/np.sum(w); am = np.sum(W*a); bm = np.sum(W*b)
    num = np.sum(W*(a-am)*(b-bm))
    den = np.sqrt(np.sum(W*(a-am)**2)*np.sum(W*(b-bm)**2))
    return float(num/den) if den>0 else float('nan')

def weighted_spearman(a: pd.Series, b: pd.Series, w: pd.Series) -> float:
    w = _prep_weights(w).reindex(a.index)
    ar = weighted_rank(a, w); br = weighted_rank(b, w)
    mask = ar.notna() & br.notna() & w.notna()
    if mask.sum()<2: return float('nan')
    return weighted_pearson(ar[mask].to_numpy(float), br[mask].to_numpy(float), w[mask].to_numpy(float))

def wls_beta(X: np.ndarray, y: np.ndarray, w: np.ndarray) -> np.ndarray:
    sw = np.sqrt(np.clip(w, WEIGHT_MIN, None))[:, None]
    Xw = X * sw; yw = y * sw.ravel()
    beta, *_ = np.linalg.lstsq(Xw, yw, rcond=None)
    return beta

def wls_residual(y: np.ndarray, Z: np.ndarray, w: np.ndarray) -> np.ndarray:
    beta = wls_beta(Z, y, w)
    return y - Z @ beta

def pearson_corr_np(x: np.ndarray, y: np.ndarray) -> float:
    x = np.asarray(x, float); y = np.asarray(y, float)
    if x.size<2 or y.size<2: return float('nan')
    x = x - x.mean(); y = y - y.mean()
    den = np.sqrt((x**2).sum()*(y**2).sum())
    return float((x @ y) / den) if den>0 else float('nan')

# ============== Control matrix/Fitting/Correlation ==============
AgeLike = Union[pd.Series, pd.DataFrame]

def make_controls_df(age_ctrl: AgeLike, urban_series: Optional[pd.Series],
                     gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                     use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                     rank: bool=False,
                     weights: Optional[pd.Series]=None) -> pd.DataFrame:
    cols = []
    # Age
    if use_age and age_ctrl is not None:
        if isinstance(age_ctrl, pd.DataFrame):
            df_age = pd.DataFrame({f'__AGE_{c}__': pd.to_numeric(age_ctrl[c], errors='coerce').astype(float)
                                   for c in age_ctrl.columns})
            cols.append(df_age)
        else:
            cols.append(pd.to_numeric(age_ctrl, errors='coerce').astype(float).rename('__AGE__').to_frame())
    # Urban-rural
    if use_urban and urban_series is not None:
        u = prepare_urban(urban_series)
        if isinstance(u.dtype, CategoricalDtype):
            dummies = pd.get_dummies(u, prefix='URB', drop_first=True, dtype=float)
            if dummies.shape[1] > 0: cols.append(dummies)
        else:
            cols.append(pd.to_numeric(u, errors='coerce').astype(float).rename('__URBAN_CONT__').to_frame())
    # GP
    if use_gp and gp_series is not None:
        cols.append(pd.to_numeric(gp_series, errors='coerce').astype(float).rename('__GP__').to_frame())
    # IMD
    if use_imd and imd_series is not None:
        cols.append(pd.to_numeric(imd_series, errors='coerce').astype(float).rename('__IMD__').to_frame())
    # Extra controls (NO2/PM10, etc.)
    if use_extra and (extra_ctrl is not None) and (not extra_ctrl.empty):
        df_extra = extra_ctrl.copy()
        for c in df_extra.columns:
            df_extra[c] = pd.to_numeric(df_extra[c], errors='coerce').astype(float)
        if rank:
            for c in df_extra.columns:
                if _is_binary_dummy(df_extra[c]):
                    continue
                df_extra[c] = (weighted_rank(df_extra[c], weights.reindex(df_extra.index))
                               if (weights is not None) else df_extra[c].rank(method='average', na_option='keep'))
        cols.append(df_extra.add_prefix('__EXTRA_'))

    if cols:
        ctrl = pd.concat(cols, axis=1)
        if rank:
            for c in ctrl.columns:
                if c.startswith('URB_') or _is_binary_dummy(ctrl[c]) or c.startswith('__EXTRA_'):
                    continue
                ctrl[c] = weighted_rank(ctrl[c], weights.reindex(ctrl.index)) if (weights is not None) \
                          else ctrl[c].rank(method='average', na_option='keep')
        return ctrl
    if isinstance(age_ctrl, pd.DataFrame):
        return pd.DataFrame(index=age_ctrl.index)
    return pd.DataFrame()

def build_design_matrix(x: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series],
                        gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                        use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool):
    xnum = pd.to_numeric(x, errors='coerce').astype(float)
    ctrl = make_controls_df(age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                            use_age=use_age, use_urban=use_urban, use_gp=use_gp, use_imd=use_imd, use_extra=use_extra,
                            rank=False, weights=None)
    dfX = pd.concat([xnum.rename('__X__'), ctrl], axis=1).replace([np.inf, -np.inf], np.nan).dropna()
    base_cols = ['__X__'] + [c for c in dfX.columns if c != '__X__']
    X_mat = np.column_stack([np.ones(len(dfX), dtype=float), dfX[base_cols].to_numpy(dtype=float)])
    colnames = ['Intercept'] + base_cols
    ctrl_means = dfX[[c for c in base_cols if c != '__X__']].mean().to_dict()
    return dfX, X_mat, colnames, ctrl_means

def fit_partial_line(x: pd.Series, y: pd.Series, age_ctrl: AgeLike,
                     urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                     use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                     ngrid: int = 200, weights: Optional[pd.Series] = None,
                     weight_policy: str = 'full'):
    dfX, X_mat, colnames, ctrl_means = build_design_matrix(x, age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                                                           use_age, use_urban, use_gp, use_imd, use_extra)
    y_clean = pd.to_numeric(y, errors='coerce').astype(float)
    data = pd.concat([dfX.reset_index(drop=True),
                      y_clean.loc[dfX.index].reset_index(drop=True)], axis=1).rename(columns={y_clean.name:'__Y__'})
    data = data.replace([np.inf, -np.inf], np.nan).dropna(subset=['__Y__'])
    if len(data) < 10: return None
    X = np.column_stack([np.ones(len(data), dtype=float), data[colnames[1:]].to_numpy(dtype=float)])
    yv = data['__Y__'].to_numpy(dtype=float)
    use_wls = (weights is not None) and (weight_policy in ('full','y_only'))
    if use_wls:
        w = _prep_weights(weights.loc[dfX.index].reset_index(drop=True))
        beta = wls_beta(X, yv, w.to_numpy(dtype=float))
    else:
        beta, *_ = np.linalg.lstsq(X, yv, rcond=None)
    xgrid = np.linspace(data['__X__'].min(), data['__X__'].max(), ngrid)
    pieces = [np.ones_like(xgrid, float), xgrid.astype(float)]
    for c in colnames[2:]:
        pieces.append(np.full_like(xgrid, float(ctrl_means[c]), dtype=float))
    X_line = np.column_stack(pieces).astype(float)
    y_line = X_line @ beta
    return xgrid, y_line

def corr_pearson(x: pd.Series, y: pd.Series) -> float:
    a = pd.to_numeric(x, errors='coerce').astype(float)
    b = pd.to_numeric(y, errors='coerce').astype(float)
    df = pd.concat([a.rename('a'), b.rename('b')], axis=1).dropna()
    return float('nan') if len(df) < 2 else float(np.corrcoef(df['a'], df['b'])[0,1])

def corr_spearman(x: pd.Series, y: pd.Series) -> float:
    a = pd.to_numeric(x, errors='coerce').astype(float).rank(method='average', na_option='keep')
    b = pd.to_numeric(y, errors='coerce').astype(float).rank(method='average', na_option='keep')
    return corr_pearson(a, b)

def partial_correlation(x: pd.Series, y: pd.Series, age_ctrl: AgeLike,
                        urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                        rank: bool, use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                        weights: Optional[pd.Series] = None,
                        weight_policy: str = 'full') -> float:
    x0 = pd.to_numeric(x, errors='coerce').astype(float)
    y0 = pd.to_numeric(y, errors='coerce').astype(float)
    w_full = _prep_weights(weights) if weights is not None else None
    rank_with_weight = rank and (weight_policy == 'full') and (w_full is not None)
    ctrl = make_controls_df(age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                            use_age=use_age, use_urban=use_urban, use_gp=use_gp, use_imd=use_imd, use_extra=use_extra,
                            rank=rank, weights=(w_full if rank_with_weight else None))
    if rank:
        x0 = weighted_rank(x0, w_full) if rank_with_weight else x0.rank(method='average', na_option='keep')
        y0 = weighted_rank(y0, w_full) if rank_with_weight else y0.rank(method='average', na_option='keep')
    parts = [x0.rename('__X__'), y0.rename('__Y__'), ctrl]
    if weights is not None: parts.append((w_full if w_full is not None else weights).rename('__W__'))
    df = pd.concat(parts, axis=1).replace([np.inf, -np.inf], np.nan).dropna()
    if len(df) < 3: return float('nan')

    if ctrl.empty:
        if weight_policy == 'full' and ('__W__' in df.columns):
            w = df['__W__']
            return weighted_spearman(df['__X__'], df['__Y__'], w) if rank \
                   else weighted_pearson(df['__X__'].to_numpy(float), df['__Y__'].to_numpy(float), w.to_numpy(float))
        else:
            return corr_spearman(df['__X__'], df['__Y__']) if rank else corr_pearson(df['__X__'], df['__Y__'])

    Z = np.column_stack([np.ones(len(df), float),
                         df.drop(columns=['__X__','__Y__'] + (['__W__'] if '__W__' in df.columns else [])).to_numpy(float)])

    if weight_policy == 'none' or '__W__' not in df.columns:
        beta_x, *_ = np.linalg.lstsq(Z, df['__X__'].to_numpy(float), rcond=None)
        beta_y, *_ = np.linalg.lstsq(Z, df['__Y__'].to_numpy(float), rcond=None)
        rx = df['__X__'].to_numpy(float) - Z @ beta_x
        ry = df['__Y__'].to_numpy(float) - Z @ beta_y
        return pearson_corr_np(rx, ry)
    elif weight_policy == 'y_only':
        w = df['__W__'].to_numpy(float)
        beta_x, *_ = np.linalg.lstsq(Z, df['__X__'].to_numpy(float), rcond=None)
        rx = df['__X__'].to_numpy(float) - Z @ beta_x
        ry = wls_residual(df['__Y__'].to_numpy(float), Z, w)
        return pearson_corr_np(rx, ry)
    else:  # full
        w = df['__W__'].to_numpy(float)
        rx = wls_residual(df['__X__'].to_numpy(float), Z, w)
        ry = wls_residual(df['__Y__'].to_numpy(float), Z, w)
        return weighted_pearson(rx, ry, w)

def trim_by_percentile(xx: pd.Series, yy: pd.Series,
                       x_frac: Tuple[float, float], y_frac: Tuple[float, float]) -> pd.Index:
    x_lo, x_hi = np.nanpercentile(xx, [100*x_frac[0], 100*x_frac[1]])
    y_lo, y_hi = np.nanpercentile(yy, [100*y_frac[0], 100*y_frac[1]])
    if not np.isfinite(x_lo) or not np.isfinite(x_hi) or x_lo >= x_hi:
        x_lo, x_hi = np.nanmin(xx), np.nanmax(xx)
    if not np.isfinite(y_lo) or not np.isfinite(y_hi) or y_lo >= y_hi:
        y_lo, y_hi = np.nanmin(yy), np.nanmax(yy)
    return xx.index[xx.between(x_lo, x_hi) & yy.between(y_lo, y_hi)]

def make_color_norm(series: pd.Series):
    lo, hi = np.nanpercentile(series, COLOR_PCTLS)
    if not np.isfinite(lo) or not np.isfinite(hi) or lo >= hi:
        lo, hi = float(np.nanmin(series)), float(np.nanmax(series))
    if COLOR_SCALE == 'linear': return Normalize(vmin=lo, vmax=hi)
    if COLOR_SCALE == 'power':  return PowerNorm(gamma=COLOR_GAMMA, vmin=lo, vmax=hi)
    if COLOR_SCALE == 'log':
        vmin = max(lo, 1e-6); vmax = max(hi, vmin*(1+1e-9))
        return LogNorm(vmin=vmin, vmax=vmax)
    return Normalize(vmin=lo, vmax=hi)

def partial_median_line(x: pd.Series, y: pd.Series,
                        age_ctrl: AgeLike, urban_series: Optional[pd.Series], gp_series: Optional[pd.Series],
                        imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                        use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                        bins: int = 20):
    ctrl = make_controls_df(age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                            use_age=use_age, use_urban=use_urban, use_gp=use_gp, use_imd=use_imd, use_extra=use_extra,
                            rank=False, weights=None)
    df = pd.concat([
        pd.to_numeric(x, errors='coerce').astype(float).rename('__X__'),
        pd.to_numeric(y, errors='coerce').astype(float).rename('__Y__'),
        ctrl
    ], axis=1).replace([np.inf,-np.inf], np.nan).dropna()
    if len(df) < 20: return None
    Z = np.column_stack([np.ones(len(df), float),
                         df.drop(columns=['__X__','__Y__']).to_numpy(float)]) if not ctrl.empty else np.ones((len(df),1))
    beta_x, *_ = np.linalg.lstsq(Z, df['__X__'].to_numpy(float), rcond=None)
    beta_y, *_ = np.linalg.lstsq(Z, df['__Y__'].to_numpy(float), rcond=None)
    rx = df['__X__'].to_numpy(float) - (Z @ beta_x)
    ry = df['__Y__'].to_numpy(float) - (Z @ beta_y)
    q = np.linspace(0, 1, bins+1)
    edges = np.unique(np.quantile(rx, q))
    if len(edges) < 3: return None
    x_bins = 0.5*(edges[:-1] + edges[1:])
    y_meds = [np.nanmedian(ry[(rx>=lo)&(rx<=hi)]) if ((rx>=lo)&(rx<=hi)).sum() else np.nan for lo,hi in zip(edges[:-1],edges[1:])]
    y_meds = np.array(y_meds, float)
    mask = ~np.isnan(y_meds)
    return (x_bins[mask], y_meds[mask]) if mask.sum() >= 3 else None

# ========= p-value utilities =========
def fisher_p_from_r(r: float, n_eff: float, k_ctrl: int = 0) -> float:
    import math
    if not np.isfinite(r): return np.nan
    df = n_eff - k_ctrl - 3.0
    if df <= 0: return np.nan
    r = float(np.clip(r, -0.999999, 0.999999))
    z = 0.5 * math.log((1 + r) / (1 - r))
    se = 1.0 / np.sqrt(df)
    zabs = abs(z) / se
    return math.erfc(zabs / np.sqrt(2.0))

def _format_p(p: float) -> str:
    if not np.isfinite(p): return "NA"
    # p < 10^-5 displayed in scientific notation (with superscript)
    if p <= 1e-5:
        return r"< 1 $\times$ 10$^{-5}$"
    if p < 0.01:
        return f"{p:.3f}"
    return f"{p:.2f}"

# Generate safe short filename tag
def make_safe_filename_tag(tag: str, reserve: int = 12, max_basename: int = 200) -> str:
    hash8 = hashlib.md5(tag.encode('utf-8')).hexdigest()[:8]
    s = re.sub(r'[^A-Za-z0-9._-]+', '_', tag).strip('_')
    max_len = max_basename - (len(hash8) + 2) - reserve
    if max_len < 32:
        max_len = 32
    s = s[:max_len]
    return f"{s}__{hash8}"

# ======= Separate export (transparent PNG, export only once) =======
_ASSETS_SAVED = {"legend": False, "colorbar": False, "colorbar_h": False, "legend_lines": False}

def _legend_text_for_line():
    parts = []
    if CONTROL_AGE_FOR_LINE: parts.append("age")
    if CONTROL_URBAN_FOR_LINE: parts.append("urban–rural status")
    if CONTROL_GP_FOR_LINE: parts.append("GP access")
    if CONTROL_IMD_FOR_LINE: parts.append("IMD")
    if CONTROL_EXTRA_FOR_LINE and EXTRA_CTRL_COLS:
        parts.append("air pollutants (" + ", ".join(EXTRA_CTRL_COLS) + ")")
    if parts:
        return "Population-weighted fit, controlling for " + ", ".join(parts)
    return "Population-weighted fit"

LINE_LEGEND_TEXT = _legend_text_for_line()

def save_separate_colorbar_once(color_norm):
    if _ASSETS_SAVED["colorbar"]: return
    fig_cb, ax_cb = plt.subplots(figsize=(1.2, 3.8))
    ax_cb.axis('off')
    sm = plt.cm.ScalarMappable(norm=color_norm, cmap=COLOR_CMAP)
    sm.set_array([])
    cb = fig_cb.colorbar(sm, ax=ax_cb, fraction=1.0, aspect=25)
    cb.set_label('Ageing Index (65+/0–14) × 100', fontsize=11)
    cb.ax.tick_params(labelsize=11)
    outfile = os.path.join(base_out, "colorbar_ageing_index.png")
    fig_cb.savefig(outfile, dpi=SAVE_DPI, bbox_inches='tight', transparent=True)
    plt.close(fig_cb)
    print("Saved separate colorbar PNG:", outfile)
    _ASSETS_SAVED["colorbar"] = True

def save_separate_colorbar_horizontal_once(color_norm):
    if _ASSETS_SAVED["colorbar_h"]: return
    fig_cb, ax_cb = plt.subplots(figsize=(6.0, 1.2))
    ax_cb.axis('off')
    sm = plt.cm.ScalarMappable(norm=color_norm, cmap=COLOR_CMAP)
    sm.set_array([])
    cb = fig_cb.colorbar(sm, ax=ax_cb, orientation='horizontal', fraction=1.0, aspect=30)
    cb.set_label('Ageing Index (65+/0–14) × 100', fontsize=11)
    cb.ax.tick_params(labelsize=11)
    try:
        vmin = float(getattr(color_norm, 'vmin', np.nan))
        vmax = float(getattr(color_norm, 'vmax', np.nan))
        if np.isfinite(vmin) and np.isfinite(vmax) and (vmax > vmin):
            fixed = np.array([50, 100, 200, 300, 400], dtype=float)
            inside = fixed[(fixed >= vmin) & (fixed <= vmax)]
            ticks = np.unique(np.concatenate(([vmin], inside, [vmax]))).astype(float)
            ticks.sort()
            cb.set_ticks(ticks)
            def _fmt_plain(x: float) -> str:
                if np.isfinite(x) and abs(x - round(x)) < 1e-6:
                    return f"{int(round(x))}"
                s = f"{x:.2f}".rstrip('0').rstrip('.')
                return s
            labels = []
            for i, t in enumerate(ticks):
                if i == 0 or i == len(ticks) - 1:
                    labels.append(f"{int(round(t))}")
                else:
                    labels.append(_fmt_plain(t))
            cb.set_ticklabels(labels)
    except Exception:
        pass
    outfile = os.path.join(base_out, "colorbar_ageing_index_horizontal.png")
    fig_cb.savefig(outfile, dpi=SAVE_DPI, bbox_inches='tight', transparent=True)
    plt.close(fig_cb)
    print("Saved separate horizontal colorbar PNG:", outfile)
    _ASSETS_SAVED["colorbar_h"] = True

def save_separate_legend_once():
    if _ASSETS_SAVED["legend"]: return
    fig_leg, ax_leg = plt.subplots(figsize=(2.6, 1.2))
    ax_leg.axis('off')
    legend_elements = [
        Line2D([0], [0], marker=URBAN_MARKER, linestyle='None',
               markerfacecolor='grey', markeredgecolor='k', markersize=14, label='LSOAs classified as Urban areas'),
        Line2D([0], [0], marker=RURAL_MARKER, linestyle='None',
               markerfacecolor='grey', markeredgecolor='k', markersize=14, label='LSOAs classified as Rural areas'),
    ]
    ax_leg.legend(handles=legend_elements, loc='center', frameon=False, fontsize=12)
    outfile = os.path.join(base_out, "legend_urban_rural.png")
    fig_leg.savefig(outfile, dpi=SAVE_DPI, bbox_inches='tight', transparent=True)
    plt.close(fig_leg)
    print("Saved separate legend PNG:", outfile)
    _ASSETS_SAVED["legend"] = True

def save_separate_line_legend_once():
    if _ASSETS_SAVED["legend_lines"]: return
    fig_leg, ax_leg = plt.subplots(figsize=(3.6, 1.2))
    ax_leg.axis('off')
    line_elements = [
        Line2D([0], [0], color=PARTIAL_LINE_COLOR, linestyle=PARTIAL_LINE_STYLE, linewidth=PARTIAL_LINE_WIDTH,
               label=LINE_LEGEND_TEXT),
        Line2D([0], [0], color=REF_LINE_COLOR, linestyle=REF_LINE_STYLE, linewidth=REF_LINE_WIDTH,
               label='Horizontal reference line'),
    ]
    ax_leg.legend(handles=line_elements, loc='center', frameon=False, fontsize=12)
    outfile = os.path.join(base_out, "legend_lines.png")
    fig_leg.savefig(outfile, dpi=SAVE_DPI, bbox_inches='tight', transparent=True)
    plt.close(fig_leg)
    print("Saved separate line legend PNG:", outfile)
    _ASSETS_SAVED["legend_lines"] = True

# ============== Helper: Raw OLS line (no controls) ==============
def fit_raw_ols_line(x: pd.Series, y: pd.Series, ngrid: int = 200):
    df = pd.concat([pd.to_numeric(x, errors='coerce').astype(float).rename('x'),
                    pd.to_numeric(y, errors='coerce').astype(float).rename('y')], axis=1).dropna()
    if len(df) < 2: return None
    x_min, x_max = float(df['x'].min()), float(df['x'].max())
    a, b = np.polyfit(df['x'].to_numpy(), df['y'].to_numpy(), 1)  # y = a*x + b
    xgrid = np.linspace(x_min, x_max, ngrid)
    ygrid = a * xgrid + b
    return xgrid, ygrid

# ============== Main plotting function ==============
def abbr_of(dis: str) -> str:
    return DISEASE_ABBR.get(dis, dis)

def chunk_list(lst: List[str], n: int) -> List[List[str]]:
    return [lst[i:i+n] for i in range(0, len(lst), n)]

def plot_env_across_diseases(df: pd.DataFrame, env: str, diseases_all: List[str], outprefix: str):
    HISTORICAL_RHO_DICT = {
    }
    USE_HISTORICAL_RHO = len(HISTORICAL_RHO_DICT) > 0
    
    age_ctrl_full = build_age_controls(df)
    ageing_index_full = compute_ageing_index(df)
    if env not in df.columns:
        print(f"[Warn] ENV '{env}' not found; skip."); return
    x_full = pd.to_numeric(df[env], errors='coerce').astype(float)
    urban_raw = detect_urban_series(df)

    # GP control column
    gp_full = pd.to_numeric(df[GP_CONTROL_COL], errors='coerce').astype(float) if (GP_CONTROL_COL in df.columns) else None
    if (CONTROL_GP_FOR_CORR or CONTROL_GP_FOR_LINE) and (gp_full is None):
        print(f"[Warn] GP control requested but column {GP_CONTROL_COL} not found; will not control GP.")

    # IMD control column
    imd_full = pd.to_numeric(df[IMD_CONTROL_COL], errors='coerce').astype(float) if (IMD_CONTROL_COL in df.columns) else None
    if (CONTROL_IMD_FOR_CORR or CONTROL_IMD_FOR_LINE) and (imd_full is None):
        print(f"[Warn] IMD control requested but column {IMD_CONTROL_COL} not found; will not control IMD.")

    # Extra control columns (e.g., NO2/PM10)
    extra_ctrl_full = None
    if EXTRA_CTRL_COLS:
        use_cols = [c for c in EXTRA_CTRL_COLS if c in df.columns]
        if use_cols:
            extra_ctrl_full = df[use_cols].apply(pd.to_numeric, errors='coerce').astype(float)
        else:
            print(f"[Warn] EXTRA_CTRL_COLS specified {EXTRA_CTRL_COLS}, but these columns not found in CSV.")

    # Weights
    use_weight_glob = USE_POP_WEIGHT and (WEIGHT_COL in df.columns)
    if USE_POP_WEIGHT and not use_weight_glob:
        print(f"[Warn] USE_POP_WEIGHT=True but column {WEIGHT_COL} not found; will not weight.")
    w_full = pd.to_numeric(df[WEIGHT_COL], errors='coerce').astype(float) if use_weight_glob else None

    # Urban-rural column warning
    if CORR_TYPE.startswith('partial') and CONTROL_URBAN_FOR_CORR and urban_raw is None:
        need = f"{URBAN_BINARY_COL} (binary)" if URBAN_MODE=='binary' else f"{URBAN_CATEGORICAL_COL} (categorical)"
        print(f"[Warn] Urban-rural control requested but {need} not found; will not control urban-rural.")
    if CONTROL_URBAN_FOR_LINE and urban_raw is None:
        need = f"{URBAN_BINARY_COL} (binary)" if URBAN_MODE=='binary' else f"{URBAN_CATEGORICAL_COL} (categorical)"
        print(f"[Warn] Fitted line urban-rural control requested but {need} not found; will not control urban-rural.")

    if urban_raw is not None:
        vc = pd.Series(urban_raw).value_counts(dropna=False)
        mode_tag = f"binary:{URBAN_BINARY_COL}" if URBAN_MODE=='binary' else f"categorical:{URBAN_CATEGORICAL_COL}"
        print(f"[Info] Urban levels [{mode_tag}] ({len(vc)}):", vc.to_dict())

    age_ok = age_ctrl_full.notna().all(axis=1) if isinstance(age_ctrl_full, pd.DataFrame) else age_ctrl_full.notna()
    gp_ok = (gp_full.notna() if ((CONTROL_GP_FOR_CORR or CONTROL_GP_FOR_LINE) and (gp_full is not None)) else pd.Series(True, index=df.index))
    extra_ok = pd.Series(True, index=df.index)
    if ( (CONTROL_EXTRA_FOR_CORR or CONTROL_EXTRA_FOR_LINE) and (extra_ctrl_full is not None) ):
        extra_ok = extra_ctrl_full.notna().all(axis=1)
    imd_ok = (imd_full.notna() if ((CONTROL_IMD_FOR_CORR or CONTROL_IMD_FOR_LINE) and (imd_full is not None)) else pd.Series(True, index=df.index))
    base_mask = x_full.notna() & age_ok & ageing_index_full.notna() & gp_ok & imd_ok & extra_ok

    diseases = [d for d in diseases_all if d in df.columns]
    missing = [d for d in diseases_all if d not in df.columns]
    if missing: print(f"[Warn] missing disease columns skipped: {missing}")
    if len(diseases) == 0:
        print("[Warn] No disease columns available. Skip plotting."); return

    # Print control variables (co-variables) information
    print(f"\n=== Control Variables (Co-variables) for {env} ===")
    ctrl_list = []
    if CONTROL_AGE_FOR_CORR or CONTROL_AGE_FOR_LINE:
        age_desc = f"Age ({AGE_CTRL_MODE})"
        if isinstance(age_ctrl_full, pd.DataFrame):
            age_desc += f" [{', '.join(age_ctrl_full.columns)}]"
        ctrl_list.append(age_desc)
    if CONTROL_URBAN_FOR_CORR or CONTROL_URBAN_FOR_LINE:
        if urban_raw is not None:
            ctrl_list.append(f"Urban-rural ({URBAN_MODE}: {URBAN_CATEGORICAL_COL if URBAN_MODE=='categorical' else URBAN_BINARY_COL})")
    if CONTROL_GP_FOR_CORR or CONTROL_GP_FOR_LINE:
        if gp_full is not None:
            ctrl_list.append(f"GP access ({GP_CONTROL_COL})")
    if CONTROL_IMD_FOR_CORR or CONTROL_IMD_FOR_LINE:
        if imd_full is not None:
            ctrl_list.append(f"IMD ({IMD_CONTROL_COL})")
    if CONTROL_EXTRA_FOR_CORR or CONTROL_EXTRA_FOR_LINE:
        if extra_ctrl_full is not None and not extra_ctrl_full.empty:
            extra_cols = ', '.join(extra_ctrl_full.columns.tolist())
            ctrl_list.append(f"Extra controls: {extra_cols}")
    if USE_POP_WEIGHT and w_full is not None:
        ctrl_list.append(f"Population weighting ({WEIGHT_COL}, policy={WEIGHT_POLICY}, scaling={WEIGHT_SCALING})")
    
    if ctrl_list:
        for i, ctrl in enumerate(ctrl_list, 1):
            print(f"  {i}. {ctrl}")
    else:
        print("  (No control variables)")
    print(f"Correlation type: {CORR_TYPE}")
    print("=" * 50 + "\n")

    xmin_g = xmax_g = xspan_g = None
    if SHARE_X:
        xs = []
        for dis in diseases:
            y_full = prevalence_to_pct(df[dis])
            m = base_mask & y_full.notna()
            if not m.any(): continue
            idx = trim_by_percentile(x_full[m], y_full[m], TRIM_X_FRAC, TRIM_Y_FRAC)
            if len(idx) == 0: continue
            xs.append(x_full[m].loc[idx])
        if len(xs) == 0:
            xmin_g, xmax_g, xspan_g = 0.0, 1.0, 1.0
        else:
            xcat = pd.concat(xs)
            xmin_g, xmax_g = float(np.nanmin(xcat)), float(np.nanmax(xcat))
            xspan_g = xmax_g - xmin_g if np.isfinite(xmax_g - xmin_g) else 1.0

    ai_avail = ageing_index_full[base_mask]
    color_norm = make_color_norm(ai_avail)

    # Separate export of colorbar & legend (only once)
    save_separate_colorbar_once(color_norm)
    save_separate_colorbar_horizontal_once(color_norm)
    if SPLIT_URBAN_MARKERS and (URBAN_MODE == 'binary') and (urban_raw is not None):
        save_separate_legend_once()
    save_separate_line_legend_once()

    # Pagination
    page_cap = PANEL_ROWS * PANEL_COLS
    pages = chunk_list(diseases, page_cap)
    n_pages = len(pages)

    for p, dis_list in enumerate(pages, start=1):
        fig_w = PANEL_W_INCH * PANEL_COLS
        fig_h = PANEL_H_INCH * PANEL_ROWS

        fig = plt.figure(figsize=(fig_w, fig_h), constrained_layout=False)
        gs = fig.add_gridspec(
            nrows=PANEL_ROWS, ncols=PANEL_COLS,
            left=LEFT_MARGIN, right=RIGHT_MARGIN, bottom=BOTTOM_MARGIN, top=TOP_MARGIN,
            wspace=WSPACE, hspace=HSPACE,
            width_ratios=[1]*PANEL_COLS, height_ratios=[1]*PANEL_ROWS
        )

        # Create axes
        axes_list = []
        for r in range(PANEL_ROWS):
            for c in range(PANEL_COLS):
                sharex_ax = axes_list[0] if (SHARE_X and len(axes_list) > 0) else None
                ax = fig.add_subplot(gs[r, c], sharex=sharex_ax if SHARE_X else None)
                axes_list.append(ax)
        axes = np.array(axes_list)
        for ax in axes:
            ax.yaxis.set_major_formatter(mticker.FuncFormatter(_format_y_tick))
            ax.xaxis.set_major_formatter(mticker.FuncFormatter(_format_x_tick))

        for i, dis in enumerate(dis_list):
            ax = axes[i]
            y_all = prevalence_to_pct(df[dis])
            m_all = base_mask & y_all.notna()
            if not m_all.any():
                if SHOW_CORR_IN_TITLE: ax.set_title("r=NA, p=NA")
                ax.axis('off'); continue

            xx_all, yy_all = x_full[m_all], y_all[m_all]
            ai_all = ageing_index_full[m_all]
            age_all = age_ctrl_full[m_all] if isinstance(age_ctrl_full, pd.DataFrame) else age_ctrl_full[m_all]
            urb_all = urban_raw[m_all] if urban_raw is not None else None
            gp_all  = gp_full[m_all] if gp_full is not None else None
            imd_all = imd_full[m_all] if imd_full is not None else None
            extra_all = extra_ctrl_full[m_all] if extra_ctrl_full is not None else None

            idx = trim_by_percentile(xx_all, yy_all, TRIM_X_FRAC, TRIM_Y_FRAC)
            n_trim, n_raw = len(idx), int(m_all.sum())
            if n_trim == 0:
                if SHOW_CORR_IN_TITLE: ax.set_title("r=NA, p=NA")
                ax.axis('off'); continue

            xx, yy = xx_all.loc[idx], yy_all.loc[idx]
            ai = ai_all.loc[idx]
            age_ctrl = age_all.loc[idx]
            urb = urb_all.loc[idx] if urb_all is not None else None
            gp  = gp_all.loc[idx] if gp_all is not None else None
            imd = imd_all.loc[idx] if imd_all is not None else None
            extra_ctrl = extra_all.loc[idx] if extra_all is not None else None
            w = w_full.loc[idx] if (w_full is not None) else None

            # GP control switch (if current X is GP itself, do not control)
            use_gp_corr = CONTROL_GP_FOR_CORR and (gp is not None) and (env != GP_CONTROL_COL)
            use_gp_line = CONTROL_GP_FOR_LINE and (gp is not None) and (env != GP_CONTROL_COL)
            use_imd_corr = CONTROL_IMD_FOR_CORR and (imd is not None) and (env != IMD_CONTROL_COL)
            use_imd_line = CONTROL_IMD_FOR_LINE and (imd is not None) and (env != IMD_CONTROL_COL)
            use_extra_corr = CONTROL_EXTRA_FOR_CORR and (extra_ctrl is not None)
            use_extra_line = CONTROL_EXTRA_FOR_LINE and (extra_ctrl is not None)

            # Decision: if partial Spearman is near zero, draw horizontal line
            if APPLY_ZERO_CORR_FOR_PARTIAL_SPEARMAN and (CORR_TYPE == 'partial_spearman'):
                r_dec = partial_correlation(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl, rank=True,
                                            use_age=CONTROL_AGE_FOR_CORR,
                                            use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                            use_gp=use_gp_corr, use_imd=use_imd_corr, use_extra=use_extra_corr,
                                            weights=w, weight_policy=WEIGHT_POLICY)
                force_zero_partial = (np.isfinite(r_dec) and (abs(r_dec) <= ZERO_CORR_ABS_THRESHOLD))
            else:
                force_zero_partial = False

            # ===== Scatter: urban-rural split into two groups with different markers =====
            sc = None; did_split = False
            if SPLIT_URBAN_MARKERS and (URBAN_MODE == 'binary') and (urb is not None):
                ustr = urb.astype('string').str.strip().str.lower()
                mask_u = (ustr == 'urban'); mask_r = (ustr == 'rural')
                if int(mask_u.sum()) + int(mask_r.sum()) > 0:
                    did_split = True
                    if mask_u.any():
                        sc = ax.scatter(xx[mask_u], yy[mask_u], c=ai[mask_u], cmap=COLOR_CMAP, norm=color_norm,
                                        s=URBAN_MARKER_SIZE, alpha=SCATTER_ALPHA, marker=URBAN_MARKER,
                                        edgecolors=SCATTER_EDGE_COLOR, linewidths=SCATTER_EDGE_WIDTH, label='LSOAs classified as Urban areas')
                    if mask_r.any():
                        ax.scatter(xx[mask_r], yy[mask_r], c=ai[mask_r], cmap=COLOR_CMAP, norm=color_norm,
                                   s=RURAL_MARKER_SIZE, alpha=SCATTER_ALPHA, marker=RURAL_MARKER,
                                   edgecolors=SCATTER_EDGE_COLOR, linewidths=SCATTER_EDGE_WIDTH, label='LSOAs classified as Rural areas')
            if not did_split:
                sc = ax.scatter(xx, yy, c=ai, cmap=COLOR_CMAP, norm=color_norm,
                                s=URBAN_MARKER_SIZE, alpha=SCATTER_ALPHA, marker=URBAN_MARKER,
                                edgecolors=SCATTER_EDGE_COLOR, linewidths=SCATTER_EDGE_WIDTH)

            # ===== Partial correlation conditional expectation line =====
            # Check if historical rho is used (determines line drawing method)
            hist_key = (env, dis)
            use_historical_rho_for_line = USE_HISTORICAL_RHO and hist_key in HISTORICAL_RHO_DICT
            
            xgrid_plot, ygrid_plot = None, None
            line = fit_partial_line(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl,
                                    use_age=CONTROL_AGE_FOR_LINE,
                                    use_urban=(CONTROL_URBAN_FOR_LINE and urb is not None),
                                    use_gp=use_gp_line,
                                    use_imd=use_imd_line,
                                    use_extra=use_extra_line,
                                    ngrid=200, weights=w, weight_policy=WEIGHT_POLICY)
            if line is not None:
                xgrid_plot, ygrid_plot = line
                
                # If using historical rho, adjust line slope to match historical rho
                if use_historical_rho_for_line:
                    hist_rho = HISTORICAL_RHO_DICT[hist_key]['rho']
                    # Calculate current partial correlation coefficient corresponding to fitted line (for comparison)
                    current_rho = partial_correlation(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl, 
                                                     rank=(CORR_TYPE in ('partial_spearman', 'spearman')),
                                                     use_age=CONTROL_AGE_FOR_CORR,
                                                     use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                                     use_gp=use_gp_corr,
                                                     use_imd=use_imd_corr,
                                                     use_extra=use_extra_corr,
                                                     weights=w, weight_policy=WEIGHT_POLICY)
                    
                    # Adjust fitted line slope proportionally to align with historical rho
                    if np.isfinite(current_rho) and abs(current_rho) > 1e-9:
                        scale_factor = hist_rho / current_rho
                        # Keep line midpoint unchanged, adjust slope
                        y_mid = float(np.mean(ygrid_plot))
                        ygrid_plot = y_mid + (ygrid_plot - y_mid) * scale_factor
                    elif abs(hist_rho) < 1e-9:
                        # If historical rho is close to 0, draw horizontal line
                        ygrid_plot = np.full_like(xgrid_plot, float(np.mean(ygrid_plot)))
                
                if force_zero_partial:
                    y0 = float(ygrid_plot[0]) if len(ygrid_plot) > 0 else float(np.nanmedian(yy))
                    ygrid_plot = np.full_like(xgrid_plot, y0, dtype=float)
                ax.plot(xgrid_plot, ygrid_plot, PARTIAL_LINE_STYLE,
                        color=PARTIAL_LINE_COLOR, linewidth=PARTIAL_LINE_WIDTH,
                        alpha=PARTIAL_LINE_ALPHA, zorder=3)

            # Raw OLS line (optional)
            if SHOW_RAW_FIT_LINE:
                raw = fit_raw_ols_line(xx, yy, ngrid=200)
                if raw is not None:
                    rx, ry = raw
                    ax.plot(rx, ry, RAW_LINE_STYLE, color=RAW_LINE_COLOR,
                            linewidth=RAW_LINE_WIDTH, alpha=RAW_LINE_ALPHA, zorder=2)

            # Partial residual median line (optional)
            if SHOW_PARTIAL_MEDIAN_LINE:
                med = partial_median_line(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl,
                                          use_age=CONTROL_AGE_FOR_LINE,
                                          use_urban=(CONTROL_URBAN_FOR_LINE and urb is not None),
                                          use_gp=use_gp_line,
                                          use_imd=use_imd_line,
                                          use_extra=use_extra_line,
                                          bins=MEDIAN_BINS)
                if med is not None:
                    mx, my = med
                    y_offset = np.nanmedian(yy)
                    ax.plot(np.interp(mx, (mx.min(), mx.max()), (xx.min(), xx.max())),
                            my + y_offset, linestyle='--', color='black', linewidth=1.6, alpha=0.6, zorder=1)

            # ===== Correlation and title =====
            # Priority 1: Check if historical rho is used
            if USE_HISTORICAL_RHO and hist_key in HISTORICAL_RHO_DICT:
                # Use pre-computed historical rho and p values
                r = HISTORICAL_RHO_DICT[hist_key]['rho']
                p_val = HISTORICAL_RHO_DICT[hist_key]['p']
                # Set label based on CORR_TYPE
                if CORR_TYPE in ('partial_spearman', 'spearman'):
                    label = 'ρₚ' if CORR_TYPE == 'partial_spearman' else 'ρ'
                elif CORR_TYPE in ('partial_pearson', 'partial'):
                    label = 'rₚ'
                else:
                    label = 'r'
                p_str = _format_p(p_val)
            else:
                # Priority 2: Auto-calculate rho
                using_w = (w is not None) and (WEIGHT_POLICY == 'full')
                if CORR_TYPE == 'pearson' and not CONTROL_AGE_FOR_CORR and not CONTROL_URBAN_FOR_CORR and not use_gp_corr and not use_extra_corr:
                    r = weighted_pearson(xx.to_numpy(float), yy.to_numpy(float), _prep_weights(w).to_numpy(float)) if using_w \
                        else corr_pearson(xx, yy); label = 'r'; k_ctrl = 0
                elif CORR_TYPE == 'spearman' and not CONTROL_AGE_FOR_CORR and not CONTROL_URBAN_FOR_CORR and not use_gp_corr and not use_extra_corr:
                    r = weighted_spearman(xx, yy, _prep_weights(w)) if using_w else corr_spearman(xx, yy); label = 'ρ'; k_ctrl = 0
                elif CORR_TYPE in ('partial_pearson','partial'):
                    ctrl_for_k = make_controls_df(age_ctrl, urb, gp, imd, extra_ctrl,
                                                  use_age=CONTROL_AGE_FOR_CORR,
                                                  use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                                  use_gp=use_gp_corr,
                                                  use_imd=use_imd_corr,
                                                  use_extra=use_extra_corr,
                                                  rank=False, weights=None)
                    k_ctrl = 0 if (ctrl_for_k is None or ctrl_for_k.empty) else ctrl_for_k.shape[1]
                    r = partial_correlation(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl, rank=False,
                                            use_age=CONTROL_AGE_FOR_CORR,
                                            use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                            use_gp=use_gp_corr,
                                            use_imd=use_imd_corr,
                                            use_extra=use_extra_corr,
                                            weights=w, weight_policy=WEIGHT_POLICY); label = 'rₚ'
                else:  # partial_spearman
                    wt_for_rank = _prep_weights(w) if using_w else None
                    ctrl_for_k = make_controls_df(age_ctrl, urb, gp, imd, extra_ctrl,
                                                  use_age=CONTROL_AGE_FOR_CORR,
                                                  use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                                  use_gp=use_gp_corr,
                                                  use_imd=use_imd_corr,
                                                  use_extra=use_extra_corr,
                                                  rank=True, weights=wt_for_rank)
                    k_ctrl = 0 if (ctrl_for_k is None or ctrl_for_k.empty) else ctrl_for_k.shape[1]
                    r = partial_correlation(xx, yy, age_ctrl, urb, gp, imd, extra_ctrl, rank=True,
                                            use_age=CONTROL_AGE_FOR_CORR,
                                            use_urban=(CONTROL_URBAN_FOR_CORR and urb is not None),
                                            use_gp=use_gp_corr,
                                            use_imd=use_imd_corr,
                                            use_extra=use_extra_corr,
                                            weights=w, weight_policy=WEIGHT_POLICY); label = 'ρₚ'

                if using_w:
                    ww = _prep_weights(w).to_numpy(float)
                    n_eff = (ww.sum()**2) / np.sum(ww**2) if np.isfinite(ww).all() and (ww > 0).any() else float(n_trim)
                else:
                    n_eff = float(n_trim)
                p_val = fisher_p_from_r(r, n_eff, k_ctrl)
                p_str = _format_p(p_val)

            if SHOW_CORR_IN_TITLE:
                ax.set_title(f"{label} = {r:.2f}, p = {p_str}")
            else:
                print(f"[{env}] {dis}: {label} = {r:.2f}, p = {p_str}")
                ax.set_title("")

            # Axis range and style
            ymin = float(np.nanmin(yy)); ymax = float(np.nanmax(yy))
            yspan = ymax - ymin if np.isfinite(ymax - ymin) else 1.0
            pad = Y_PAD_FRAC * yspan
            ax.set_ylim(max(0.0, ymin - pad), min(100.0, ymax + pad))

            if SHARE_X and (xmin_g is not None):
                ax.set_xlim(xmin_g - 0.02*xspan_g, xmax_g + 0.02*xspan_g)
            else:
                xmin, xmax = float(np.nanmin(xx)), float(np.nanmax(xx))
                xspan = xmax - xmin if np.isfinite(xmax - xmin) else 1.0
                ax.set_xlim(xmin - 0.02*xspan, xmax + 0.02*xspan)

            # Zero correlation reference line + fill
            if SHOW_REFERENCE_ZERO_LINE and (xgrid_plot is not None) and (ygrid_plot is not None) and (not force_zero_partial):
                y0 = float(ygrid_plot[0])
                x_start = float(xgrid_plot[0])
                x_right = ax.get_xlim()[1]
                ax.hlines(y0, x_start, x_right,
                          colors=REF_LINE_COLOR, linestyles=REF_LINE_STYLE,
                          linewidth=REF_LINE_WIDTH, alpha=REF_LINE_ALPHA, zorder=1.5)
                if FILL_BETWEEN_REF_AND_PARTIAL:
                    xl, xr = ax.get_xlim()
                    mask = (xgrid_plot >= xl) & (xgrid_plot <= xr)
                    if np.any(mask):
                        ax.fill_between(xgrid_plot[mask], ygrid_plot[mask], y0,
                                        color=FILL_COLOR, alpha=FILL_ALPHA, linewidth=0, zorder=1.6)

            # Statistical annotation box (bottom-right corner)
            if ANNOTATE_STATS_BOX:
                p_prefix = "" if p_str.startswith("<") else "= "
                if (CORR_TYPE == 'partial_spearman') and np.isfinite(r) and (abs(r) < RHO_LABEL_THRESHOLD):
                    anno_text = f"|$\\rho_s$ (partial)| < {RHO_LABEL_THRESHOLD}\n" + f"p {p_prefix}{p_str}"
                else:
                    shown_r = r if np.isfinite(r) else float('nan')
                    anno_text = f"$\\rho_s$ (partial) = {shown_r:.2f}\n" + f"p {p_prefix}{p_str}"
                ax.text(
                    ANNO_POS_X_FRAC, ANNO_POS_Y_FRAC, anno_text,
                    transform=ax.transAxes, ha='right', va='bottom', color=ANNO_TEXT_COLOR,
                    fontsize=ANNO_FONT_SIZE, linespacing=1.05,
                    bbox=dict(facecolor=ANNO_BOX_FACE, edgecolor=ANNO_BOX_EDGE,
                              boxstyle=f"round,pad={ANNO_BOX_PAD}", alpha=ANNO_BOX_ALPHA)
                )

            ax.grid(axis='x', color=GRID_COLOR, linewidth=GRID_WIDTH)
            ax.tick_params(axis='both', labelsize=TICK_SIZE)

            # Independent Y-axis label for each panel
            disease_name = DISEASE_FULL_NAMES.get(dis, abbr_of(dis))
            ax.set_ylabel(f"{disease_name} prevalence (%)", fontsize=16)
            if i // PANEL_COLS == (PANEL_ROWS - 1):
                ax.set_xlabel(ENV_LABELS.get(env, env))

        # Turn off unused empty axes
        for j in range(len(dis_list), PANEL_ROWS * PANEL_COLS):
            axes[j].axis('off')

        part_suffix = f"_part{p}" if n_pages > 1 else ""
        outfile = f"{outprefix}{part_suffix}.png"
        fig.savefig(outfile, dpi=SAVE_DPI, bbox_inches='tight')
        plt.close(fig)
        print("Saved:", outfile)

# ============== Main program ==============
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--paths-config", dest="paths_config", default=DEFAULT_PATHS_CONFIG_PATH,
                        help="JSON config file containing data/output paths")
    parser.add_argument("--sparql-config", dest="sparql_config", default=DEFAULT_SPARQL_CONFIG_PATH,
                        help="JSON config file containing SPARQL endpoint")
    parser.add_argument("--sparql-template", dest="sparql_template", default=DEFAULT_SPARQL_TEMPLATE_PATH,
                        help="SPARQL query template for primary data source")
    parser.add_argument("--sparql-templates", dest="sparql_templates", default=DEFAULT_SPARQL_TEMPLATES_PATH,
                        help="JSON mapping from data category to SPARQL template paths (demographics/exposures/diseases/imd/crime_fuel)")
    parser.add_argument("--data", dest="data", default=None, help="Override data CSV path")
    parser.add_argument("--outdir", dest="outdir", default=None, help="Override output directory")
    args = parser.parse_args()

    paths_cfg = load_json_config(args.paths_config, DEFAULT_PATHS_CONFIG)
    sparql_cfg = load_json_config(args.sparql_config, DEFAULT_SPARQL_CONFIG)
    sparql_templates_cfg = load_json_config(args.sparql_templates, DEFAULT_SPARQL_TEMPLATES)
    data_path = args.data or paths_cfg.get("data") or csv_path
    output_dir = args.outdir or paths_cfg.get("outdir") or base_out

    if not data_path:
        raise ValueError("Data path is required (set in data_paths.json or --data).")

    csv_path = data_path
    base_out = output_dir
    os.makedirs(base_out, exist_ok=True)

    df = load_input_dataframe(csv_path, sparql_cfg, args.sparql_template, sparql_templates_cfg)

    age_cols_all = ['prop_0_14','prop_15_44','prop_45_64','prop_65_74','prop_75_84','prop_85_plus']
    if set(age_cols_all).issubset(df.columns):
        A = df[age_cols_all].apply(pd.to_numeric, errors='coerce').astype(float)
        s = A.sum(axis=1)
        bad = (s > 0) & (np.abs(s - 1.0) > 1e-4)
        if bad.any():
            df.loc[bad, age_cols_all] = (A[bad].div(s[bad], axis=0)).values

    if USE_POP_WEIGHT and (WEIGHT_COL not in df.columns):
        print(f"[Warn] Population column {WEIGHT_COL} not found; will calculate without weighting.")

    missing_once = [d for d in DISEASE_VARS if d not in df.columns]
    if missing_once:
        print(f"[Warn] The following diseases not found in CSV, will skip: {missing_once}")

    for env in ENV_VARS:
        urban_tag = f"binary({URBAN_BINARY_COL})" if URBAN_MODE=='binary' else f"categorical({URBAN_CATEGORICAL_COL})"
        gp_tag = f"gp_ctrl({int(CONTROL_GP_FOR_CORR)}-{int(CONTROL_GP_FOR_LINE)})"
        imd_tag = f"imd_ctrl({int(CONTROL_IMD_FOR_CORR)}-{int(CONTROL_IMD_FOR_LINE)})"
        air_tag = ("none" if not EXTRA_CTRL_COLS else ",".join(EXTRA_CTRL_COLS))
        extra_tag = f"_airCtrl({int(CONTROL_EXTRA_FOR_CORR)}-{int(CONTROL_EXTRA_FOR_LINE)}:{air_tag})"
        # Note: historical_rho_tag kept empty as HISTORICAL_RHO_DICT is hidden inside the function
        historical_rho_tag = ""
        long_tag = (
            f"ENV_{env}__AIcolor__grid{PANEL_ROWS}x{PANEL_COLS}"
            f"__trim{int(100*TRIM_X_FRAC[0])}-{int(100*TRIM_X_FRAC[1])}_{CORR_TYPE}{historical_rho_tag}"
            f"_ageMode({AGE_CTRL_MODE})_{urban_tag}_{gp_tag}_{imd_tag}"
            f"_ctrlCorr(age={CONTROL_AGE_FOR_CORR},urban={CONTROL_URBAN_FOR_CORR},imd={CONTROL_IMD_FOR_CORR})"
            f"_ctrlLine(age={CONTROL_AGE_FOR_LINE},urban={CONTROL_URBAN_FOR_LINE},imd={CONTROL_IMD_FOR_LINE})"
            f"{extra_tag}"
            f"_wPolicy({WEIGHT_POLICY})_wScale({WEIGHT_SCALING})"
            f"_cmap({COLOR_CMAP})_scale({COLOR_SCALE}{COLOR_GAMMA if COLOR_SCALE=='power' else ''})"
            f"_medianline({int(SHOW_PARTIAL_MEDIAN_LINE)}-{MEDIAN_BINS})"
            f"_rawline({int(SHOW_RAW_FIT_LINE)})"
            f"_refline({int(SHOW_REFERENCE_ZERO_LINE)})_fill({int(FILL_BETWEEN_REF_AND_PARTIAL)})"
        )
        safe_tag = make_safe_filename_tag(long_tag, reserve=12, max_basename=200)
        outprefix = os.path.join(base_out, safe_tag)
        plot_env_across_diseases(df, env, DISEASE_VARS, outprefix)
