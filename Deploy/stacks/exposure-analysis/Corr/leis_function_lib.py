# -*- coding: utf-8 -*-
"""
Correlation Analysis Function Library

This module contains all functions from leis.py that can be imported and used independently.

Usage:
    from leis_function_lib import partial_correlation, weighted_pearson, build_age_controls
"""

import numpy as np
import pandas as pd
from typing import Optional, Tuple, Union, List
from pandas.api.types import CategoricalDtype, is_numeric_dtype
import warnings

warnings.filterwarnings("ignore")

# ========== Default Constants (may need to be configured) ==========
AGE_CTRL_MODE = 'ai'  # 'ai', 'single', '4band', '6band'
AGE_GROUP_KEY = None  # Used when AGE_CTRL_MODE == 'single'
AGE_REF_DROP_4 = None
AGE_REF_DROP_6 = 'prop_15_44'
URBAN_MODE = 'binary'  # 'binary' or 'categorical'
URBAN_BINARY_COL = 'Urban_rural_flag'
URBAN_CATEGORICAL_COL = 'RUC21CD'
WEIGHT_SCALING = 'identity'  # 'sqrt', 'log1p', 'identity'
WEIGHT_MIN = 0.001
COLOR_CMAP = 'viridis'
COLOR_SCALE = 'linear'  # 'linear', 'power', 'log'
COLOR_GAMMA = 1.0
COLOR_PCTLS = [2.5, 97.5]

# Age group definitions (simplified - may need to be configured)
AGE_GROUP_DEFS = {
    'G_0_14': (['prop_0_14'], None),
    'G_15_44': (['prop_15_44'], None),
    'G_45_64': (['prop_45_64'], None),
    'G_65_plus': (['prop_65_74', 'prop_75_84', 'prop_85_plus'], None),
}

# ========== Data Preparation Functions ==========
def prevalence_to_pct(series: pd.Series) -> pd.Series:
    """Convert prevalence values to percentage format"""
    s = pd.to_numeric(series, errors='coerce').astype(float)
    return s*100.0 if s.max(skipna=True) is not None and s.max(skipna=True) <= 1.0 else s

def compute_age_share_pct(df: pd.DataFrame, age_key: str, age_group_defs=None) -> pd.Series:
    """Compute age group share as percentage"""
    if age_group_defs is None:
        age_group_defs = AGE_GROUP_DEFS
    cols, _ = age_group_defs[age_key]
    share = pd.Series(0.0, index=df.index, dtype=float)
    for c in cols:
        share = share.add(pd.to_numeric(df[c], errors='coerce'), fill_value=0.0)
    return share * 100.0

def compute_ageing_index(df: pd.DataFrame, age_group_defs=None) -> pd.Series:
    """Compute ageing index (ratio of 65+ to 0-14 age groups)"""
    if age_group_defs is None:
        age_group_defs = AGE_GROUP_DEFS
    p65 = compute_age_share_pct(df, 'G_65_plus', age_group_defs)
    p014 = compute_age_share_pct(df, 'G_0_14', age_group_defs)
    ai = (p65 / p014) * 100.0
    ai.replace([np.inf, -np.inf], np.nan, inplace=True)
    return ai

# ========== Age Control Functions ==========
def build_age_controls(df: pd.DataFrame, age_ctrl_mode=None, age_group_key=None,
                      age_ref_drop_4=None, age_ref_drop_6=None, age_group_defs=None) -> pd.DataFrame:
    """Build age control variables"""
    if age_ctrl_mode is None:
        age_ctrl_mode = AGE_CTRL_MODE
    if age_group_key is None:
        age_group_key = AGE_GROUP_KEY
    if age_ref_drop_4 is None:
        age_ref_drop_4 = AGE_REF_DROP_4
    if age_ref_drop_6 is None:
        age_ref_drop_6 = AGE_REF_DROP_6
    if age_group_defs is None:
        age_group_defs = AGE_GROUP_DEFS
        
    if age_ctrl_mode in ('ai', 'ageing_index', 'aging_index', 'ageingindex'):
        ai = compute_ageing_index(df, age_group_defs)
        return ai.rename('__AGEING_INDEX__').to_frame()
    if age_ctrl_mode == 'single':
        return compute_age_share_pct(df, age_group_key or 'G_15_44', age_group_defs).rename('__AGE__').to_frame()
    if age_ctrl_mode == '4band':
        df_age = pd.DataFrame({
            'age_0_14':   pd.to_numeric(df['prop_0_14'],  errors='coerce')*100.0,
            'age_15_44':  pd.to_numeric(df['prop_15_44'], errors='coerce')*100.0,
            'age_45_64':  pd.to_numeric(df['prop_45_64'], errors='coerce')*100.0,
            'age_65_plus':(pd.to_numeric(df['prop_65_74'],errors='coerce')
                          +pd.to_numeric(df['prop_75_84'],errors='coerce')
                          +pd.to_numeric(df['prop_85_plus'],errors='coerce'))*100.0
        })
        drop_col = age_ref_drop_4 if age_ref_drop_4 and age_ref_drop_4 in df_age.columns else df_age.columns[1]
        return df_age.drop(columns=[drop_col])
    df_age6 = pd.DataFrame({
        'prop_0_14':   pd.to_numeric(df['prop_0_14'],   errors='coerce')*100.0,
        'prop_15_44':  pd.to_numeric(df['prop_15_44'],  errors='coerce')*100.0,
        'prop_45_64':  pd.to_numeric(df['prop_45_64'],  errors='coerce')*100.0,
        'prop_65_74':  pd.to_numeric(df['prop_65_74'],  errors='coerce')*100.0,
        'prop_75_84':  pd.to_numeric(df['prop_75_84'],  errors='coerce')*100.0,
        'prop_85_plus':pd.to_numeric(df['prop_85_plus'],errors='coerce')*100.0,
    })
    drop_col = age_ref_drop_6 if age_ref_drop_6 and age_ref_drop_6 in df_age6.columns else df_age6.columns[1]
    return df_age6.drop(columns=[drop_col])

# ========== Urban-Rural Functions ==========
def _normalize_binary_urban(ser: pd.Series) -> pd.Series:
    """Normalize binary urban/rural series to standard categories"""
    s = ser.astype('string').str.strip().str.lower()
    mapped = s.map({'urban': 'Urban', 'rural': 'Rural'})
    cat = pd.Categorical(mapped, categories=['Rural','Urban'], ordered=False)
    return pd.Series(cat, index=ser.index, name=ser.name)

def detect_urban_series(df: pd.DataFrame, urban_mode=None, urban_binary_col=None, urban_categorical_col=None) -> Optional[pd.Series]:
    """Detect and extract urban/rural series"""
    if urban_mode is None:
        urban_mode = URBAN_MODE
    if urban_binary_col is None:
        urban_binary_col = URBAN_BINARY_COL
    if urban_categorical_col is None:
        urban_categorical_col = URBAN_CATEGORICAL_COL
    if urban_mode == 'binary':
        if urban_binary_col in df.columns:
            return _normalize_binary_urban(df[urban_binary_col])
        else:
            return None
    if urban_categorical_col in df.columns:
        return df[urban_categorical_col]
    return None

def prepare_urban(ser: Optional[pd.Series]) -> Optional[pd.Series]:
    """Prepare urban/rural series for use in control matrix"""
    if ser is None: return None
    s = ser.copy()
    if not isinstance(s.dtype, CategoricalDtype):
        if is_numeric_dtype(s) and s.nunique(dropna=True) <= 100:
            s = s.astype('Int64').astype('category')
        else:
            s = s.astype('string').astype('category')
    return s

# ========== Weighting and Ranking Functions ==========
def _prep_weights(w: pd.Series, weight_scaling=None, weight_min=None) -> pd.Series:
    """Prepare weights for weighted analysis"""
    if weight_scaling is None:
        weight_scaling = WEIGHT_SCALING
    if weight_min is None:
        weight_min = WEIGHT_MIN
    w = pd.to_numeric(w, errors='coerce').astype(float).clip(lower=0)
    if weight_scaling == 'sqrt':
        w = np.sqrt(w)
    elif weight_scaling == 'log1p':
        w = np.log1p(w)
    w = w.replace([np.inf, -np.inf], np.nan)
    fill_val = np.nanmedian(w) if np.isfinite(np.nanmedian(w)) else 1.0
    w = w.fillna(fill_val)
    return w.clip(lower=weight_min)

def _is_binary_dummy(s: pd.Series) -> bool:
    """Check if a series represents binary dummy variables"""
    x = pd.to_numeric(s, errors='coerce')
    vals = pd.unique(x.dropna())
    if len(vals) == 0: return False
    try:
        as_int = np.unique(np.round(vals).astype(int))
        return set(as_int).issubset({0,1}) and len(as_int) <= 2
    except Exception:
        return False

def weighted_rank(s: pd.Series, w: pd.Series, weight_scaling=None, weight_min=None) -> pd.Series:
    """Compute weighted rank (RIDIT score)"""
    w = _prep_weights(w, weight_scaling, weight_min).reindex(s.index)
    s = pd.to_numeric(s, errors='coerce').astype(float)
    df = pd.DataFrame({'v': s, 'w': w}).replace([np.inf, -np.inf], np.nan).dropna()
    out = pd.Series(np.nan, index=s.index, dtype=float)
    if df.empty: return out
    grp = df.groupby('v', sort=True)['w'].sum()
    cum = grp.cumsum(); total = grp.sum()
    ridit_map = (cum.shift(fill_value=0.0) + 0.5*grp) / (total if total>0 else np.nan)
    out.loc[df.index] = df['v'].map(ridit_map).astype(float).values
    return out

def weighted_pearson(a: np.ndarray, b: np.ndarray, w: np.ndarray) -> float:
    """Compute weighted Pearson correlation coefficient"""
    w = np.asarray(w, float); a = np.asarray(a, float); b = np.asarray(b, float)
    if a.size<2 or b.size<2 or w.size<2: return float('nan')
    W = w/np.sum(w); am = np.sum(W*a); bm = np.sum(W*b)
    num = np.sum(W*(a-am)*(b-bm))
    den = np.sqrt(np.sum(W*(a-am)**2)*np.sum(W*(b-bm)**2))
    return float(num/den) if den>0 else float('nan')

def weighted_spearman(a: pd.Series, b: pd.Series, w: pd.Series, **kwargs) -> float:
    """Compute weighted Spearman correlation coefficient"""
    ar = weighted_rank(a, w, **kwargs); br = weighted_rank(b, w, **kwargs)
    mask = ar.notna() & br.notna() & w.notna()
    if mask.sum()<2: return float('nan')
    return weighted_pearson(ar[mask].to_numpy(float), br[mask].to_numpy(float), w[mask].to_numpy(float))

def wls_beta(X: np.ndarray, y: np.ndarray, w: np.ndarray, weight_min=None) -> np.ndarray:
    """Compute weighted least squares (WLS) regression coefficients"""
    if weight_min is None:
        weight_min = WEIGHT_MIN
    sw = np.sqrt(np.clip(w, weight_min, None))[:, None]
    Xw = X * sw; yw = y * sw.ravel()
    beta, *_ = np.linalg.lstsq(Xw, yw, rcond=None)
    return beta

def wls_residual(y: np.ndarray, Z: np.ndarray, w: np.ndarray, **kwargs) -> np.ndarray:
    """Compute weighted least squares residuals"""
    beta = wls_beta(Z, y, w, **kwargs)
    return y - Z @ beta

def pearson_corr_np(x: np.ndarray, y: np.ndarray) -> float:
    """Compute unweighted Pearson correlation from numpy arrays"""
    x = np.asarray(x, float); y = np.asarray(y, float)
    if x.size<2 or y.size<2: return float('nan')
    x = x - x.mean(); y = y - y.mean()
    den = np.sqrt((x**2).sum()*(y**2).sum())
    return float((x @ y) / den) if den>0 else float('nan')

# ========== Correlation Functions ==========
AgeLike = Union[pd.Series, pd.DataFrame]

def make_controls_df(age_ctrl: AgeLike, urban_series: Optional[pd.Series],
                     gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                     use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                     rank: bool=False, weights: Optional[pd.Series]=None, **kwargs) -> pd.DataFrame:
    """Construct control variables DataFrame for partial correlation analysis"""
    cols = []
    if use_age and age_ctrl is not None:
        if isinstance(age_ctrl, pd.DataFrame):
            df_age = pd.DataFrame({f'__AGE_{c}__': pd.to_numeric(age_ctrl[c], errors='coerce').astype(float)
                                   for c in age_ctrl.columns})
            cols.append(df_age)
        else:
            cols.append(pd.to_numeric(age_ctrl, errors='coerce').astype(float).rename('__AGE__').to_frame())
    if use_urban and urban_series is not None:
        u = prepare_urban(urban_series)
        if isinstance(u.dtype, CategoricalDtype):
            dummies = pd.get_dummies(u, prefix='URB', drop_first=True, dtype=float)
            if dummies.shape[1] > 0: cols.append(dummies)
        else:
            cols.append(pd.to_numeric(u, errors='coerce').astype(float).rename('__URBAN_CONT__').to_frame())
    if use_gp and gp_series is not None:
        cols.append(pd.to_numeric(gp_series, errors='coerce').astype(float).rename('__GP__').to_frame())
    if use_imd and imd_series is not None:
        cols.append(pd.to_numeric(imd_series, errors='coerce').astype(float).rename('__IMD__').to_frame())
    if use_extra and (extra_ctrl is not None) and (not extra_ctrl.empty):
        df_extra = extra_ctrl.copy()
        for c in df_extra.columns:
            df_extra[c] = pd.to_numeric(df_extra[c], errors='coerce').astype(float)
        if rank:
            for c in df_extra.columns:
                if _is_binary_dummy(df_extra[c]):
                    continue
                df_extra[c] = (weighted_rank(df_extra[c], weights.reindex(df_extra.index), **kwargs)
                               if (weights is not None) else df_extra[c].rank(method='average', na_option='keep'))
        cols.append(df_extra.add_prefix('__EXTRA_'))
    
    if cols:
        ctrl = pd.concat(cols, axis=1)
        if rank:
            for c in ctrl.columns:
                if c.startswith('URB_') or _is_binary_dummy(ctrl[c]) or c.startswith('__EXTRA_'):
                    continue
                ctrl[c] = weighted_rank(ctrl[c], weights.reindex(ctrl.index), **kwargs) if (weights is not None) \
                          else ctrl[c].rank(method='average', na_option='keep')
        return ctrl
    if isinstance(age_ctrl, pd.DataFrame):
        return pd.DataFrame(index=age_ctrl.index)
    return pd.DataFrame()

def build_design_matrix(x: pd.Series, age_ctrl: AgeLike, urban_series: Optional[pd.Series],
                        gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                        use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool, **kwargs):
    """Build design matrix for linear regression"""
    xnum = pd.to_numeric(x, errors='coerce').astype(float)
    ctrl = make_controls_df(age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                            use_age=use_age, use_urban=use_urban, use_gp=use_gp, use_imd=use_imd, use_extra=use_extra,
                            rank=False, weights=None, **kwargs)
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
                     weight_policy: str = 'full', **kwargs):
    """Fit partial regression line (y ~ x | controls) for plotting"""
    dfX, X_mat, colnames, ctrl_means = build_design_matrix(x, age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                                                           use_age, use_urban, use_gp, use_imd, use_extra, **kwargs)
    y_clean = pd.to_numeric(y, errors='coerce').astype(float)
    data = pd.concat([dfX.reset_index(drop=True),
                      y_clean.loc[dfX.index].reset_index(drop=True)], axis=1).rename(columns={y_clean.name:'__Y__'})
    data = data.replace([np.inf, -np.inf], np.nan).dropna(subset=['__Y__'])
    if len(data) < 10: return None
    X = np.column_stack([np.ones(len(data), dtype=float), data[colnames[1:]].to_numpy(dtype=float)])
    yv = data['__Y__'].to_numpy(dtype=float)
    use_wls = (weights is not None) and (weight_policy in ('full','y_only'))
    if use_wls:
        w = _prep_weights(weights.loc[dfX.index].reset_index(drop=True), **kwargs)
        beta = wls_beta(X, yv, w.to_numpy(dtype=float), **kwargs)
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
    """Compute unweighted Pearson correlation coefficient"""
    a = pd.to_numeric(x, errors='coerce').astype(float)
    b = pd.to_numeric(y, errors='coerce').astype(float)
    df = pd.concat([a.rename('a'), b.rename('b')], axis=1).dropna()
    return float('nan') if len(df) < 2 else float(np.corrcoef(df['a'], df['b'])[0,1])

def corr_spearman(x: pd.Series, y: pd.Series) -> float:
    """Compute unweighted Spearman rank correlation coefficient"""
    a = pd.to_numeric(x, errors='coerce').astype(float).rank(method='average', na_option='keep')
    b = pd.to_numeric(y, errors='coerce').astype(float).rank(method='average', na_option='keep')
    return corr_pearson(a, b)

def partial_correlation(x: pd.Series, y: pd.Series, age_ctrl: AgeLike,
                        urban_series: Optional[pd.Series], gp_series: Optional[pd.Series], imd_series: Optional[pd.Series], extra_ctrl: Optional[pd.DataFrame],
                        rank: bool, use_age: bool, use_urban: bool, use_gp: bool, use_imd: bool, use_extra: bool,
                        weights: Optional[pd.Series] = None,
                        weight_policy: str = 'full', **kwargs) -> float:
    """Compute partial correlation coefficient controlling for specified variables"""
    x0 = pd.to_numeric(x, errors='coerce').astype(float)
    y0 = pd.to_numeric(y, errors='coerce').astype(float)
    w_full = _prep_weights(weights, **kwargs) if weights is not None else None
    rank_with_weight = rank and (weight_policy == 'full') and (w_full is not None)
    ctrl = make_controls_df(age_ctrl, urban_series, gp_series, imd_series, extra_ctrl,
                            use_age=use_age, use_urban=use_urban, use_gp=use_gp, use_imd=use_imd, use_extra=use_extra,
                            rank=rank, weights=(w_full if rank_with_weight else None), **kwargs)
    if rank:
        x0 = weighted_rank(x0, w_full, **kwargs) if rank_with_weight else x0.rank(method='average', na_option='keep')
        y0 = weighted_rank(y0, w_full, **kwargs) if rank_with_weight else y0.rank(method='average', na_option='keep')
    parts = [x0.rename('__X__'), y0.rename('__Y__'), ctrl]
    if weights is not None: parts.append((w_full if w_full is not None else weights).rename('__W__'))
    df = pd.concat(parts, axis=1).replace([np.inf, -np.inf], np.nan).dropna()
    if len(df) < 3: return float('nan')
    
    if ctrl.empty:
        if weight_policy == 'full' and ('__W__' in df.columns):
            w = df['__W__']
            return weighted_spearman(df['__X__'], df['__Y__'], w, **kwargs) if rank \
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
        ry = wls_residual(df['__Y__'].to_numpy(float), Z, w, **kwargs)
        return pearson_corr_np(rx, ry)
    else:  # full
        w = df['__W__'].to_numpy(float)
        rx = wls_residual(df['__X__'].to_numpy(float), Z, w, **kwargs)
        ry = wls_residual(df['__Y__'].to_numpy(float), Z, w, **kwargs)
        return weighted_pearson(rx, ry, w)

def trim_by_percentile(xx: pd.Series, yy: pd.Series,
                       x_frac: Tuple[float, float], y_frac: Tuple[float, float]) -> pd.Index:
    """Filter data to keep only values within specified percentile ranges"""
    x_lo, x_hi = np.nanpercentile(xx, [100*x_frac[0], 100*x_frac[1]])
    y_lo, y_hi = np.nanpercentile(yy, [100*y_frac[0], 100*y_frac[1]])
    if not np.isfinite(x_lo) or not np.isfinite(x_hi) or x_lo >= x_hi:
        x_lo, x_hi = np.nanmin(xx), np.nanmax(xx)
    if not np.isfinite(y_lo) or not np.isfinite(y_hi) or y_lo >= y_hi:
        y_lo, y_hi = np.nanmin(yy), np.nanmax(yy)
    return xx.index[xx.between(x_lo, x_hi) & yy.between(y_lo, y_hi)]

# ========== Utility Functions ==========
def fisher_p_from_r(r: float, n_eff: float, k_ctrl: int = 0) -> float:
    """Compute p-value for correlation coefficient using Fisher's z-transformation"""
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
    """Format p-value as string for display"""
    if not np.isfinite(p): return "NA"
    if p <= 1e-5:
        return r"< 1 $\times$ 10$^{-5}$"
    if p < 0.01:
        return f"{p:.3f}"
    return f"{p:.2f}"

# Note: plot_env_across_diseases and other plotting functions are complex
# and require many dependencies. For full plotting functionality,
# please use the original leis.py file directly or import those functions separately.

