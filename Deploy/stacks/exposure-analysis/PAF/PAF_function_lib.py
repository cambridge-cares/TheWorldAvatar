# -*- coding: utf-8 -*-
"""
PAF Function Library

This module contains all functions from paf_experments-control-health.py that can be imported and used independently.

Usage:
    from PAF_function_lib import query_twa_stack_exposure_data, compute_paf, run_one_config
    
    # Use functions as needed
    df = query_twa_stack_exposure_data(csv_fallback_path="data.csv")
"""

import os
import warnings
import argparse
import numpy as np
import pandas as pd
import statsmodels.api as sm
from patsy import dmatrix, build_design_matrices
from statsmodels.genmod.families import Poisson
import requests

warnings.filterwarnings("ignore")

# ========== Default Constants (can be overridden) ==========
TWA_SPARQL_ENDPOINT = "http://localhost:5432/ontop/sparql"
TOTAL_COL = "Total"
URBAN_FLAG_COL = "Urban_rural_flag"
RUC_COL = "RUC21CD"
AGE_6 = ['prop_0_14','prop_15_44','prop_45_64','prop_65_74','prop_75_84','prop_85_plus']
AGE_DROP = 'prop_15_44'
PCT_LIST = [50, 40, 30, 20, 10]
MIN_SAMPLES_FOR_GLM = 50
MIN_SAMPLES_FOR_STRATUM = 100
MIN_VALID_EXPOSURE = 50
DEBUG_PRINT = True
USE_IMD_CONTROLS = True
IMD_FULL_COLS = [
    "Income Score (rate)",
    "Education, Skills and Training Score",
    "Employment Score (rate)",
    "Wider Barriers Sub-domain Score",
    "Outdoors Sub-domain Score",
]
SOCIOECONOMIC_IMD_COLLINEARITY_EXPOSURES = [
    "Proportion of households fuel poor (%)",
]
IMD_COLLINEARITY_METHOD = "spearman"
IMD_COLLINEARITY_ABS_CORR_THRESHOLD = 0.90
IMD_COLLINEARITY_MIN_N = 200

# ========== TWA Stack Extraction ==========
def query_twa_stack_exposure_data(csv_fallback_path=None, endpoint=None):
    """
    Query TWA Stack SPARQL endpoint for exposure and health data.
    If query fails, fallback to reading local CSV file.
    
    Parameters:
        csv_fallback_path (str, optional): Path to CSV file used as fallback
        endpoint (str, optional): SPARQL endpoint URL. Defaults to TWA_SPARQL_ENDPOINT
        
    Returns:
        pd.DataFrame: DataFrame with all required columns
    """
    if endpoint is None:
        endpoint = TWA_SPARQL_ENDPOINT
    try:
        print(f"[TWA] Attempting to query SPARQL endpoint: {endpoint}")
        query = """
        PREFIX twa:     <https://theworldavatar.io/kg/>
        PREFIX ontoons: <https://www.ordnancesurvey.co.uk/linked-data/ontology/>
        PREFIX sosa:    <http://www.w3.org/ns/sosa/>
        PREFIX mesh:    <http://id.nlm.nih.gov/mesh/>
        PREFIX om:      <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        
        SELECT ?lsoa_code ?prevalence_CHD ?prevalence_COPD ?prevalence_DM ?prevalence_HF
               ?prevalence_HYP ?prevalence_OB ?prevalence_STIA
        WHERE {
          ?lsoa a ontoons:LSOA ;
                ontoons:hasLSOACode ?lsoa_code .
          
          OPTIONAL {
            ?obs_chd a sosa:Observation ;
                     sosa:hasFeatureOfInterest ?lsoa ;
                     sosa:observedProperty mesh:D029424 ;
                     sosa:hasResult ?m_chd .
            ?m_chd om:hasNumericalValue ?prevalence_CHD .
          }
          OPTIONAL {
            ?obs_copd a sosa:Observation ;
                      sosa:hasFeatureOfInterest ?lsoa ;
                      sosa:observedProperty mesh:D029967 ;
                      sosa:hasResult ?m_copd .
            ?m_copd om:hasNumericalValue ?prevalence_COPD .
          }
          OPTIONAL {
            ?obs_dm a sosa:Observation ;
                    sosa:hasFeatureOfInterest ?lsoa ;
                    sosa:observedProperty mesh:D003920 ;
                    sosa:hasResult ?m_dm .
            ?m_dm om:hasNumericalValue ?prevalence_DM .
          }
          OPTIONAL {
            ?obs_hf a sosa:Observation ;
                    sosa:hasFeatureOfInterest ?lsoa ;
                    sosa:observedProperty mesh:D006333 ;
                    sosa:hasResult ?m_hf .
            ?m_hf om:hasNumericalValue ?prevalence_HF .
          }
          OPTIONAL {
            ?obs_hyp a sosa:Observation ;
                     sosa:hasFeatureOfInterest ?lsoa ;
                     sosa:observedProperty mesh:D006973 ;
                     sosa:hasResult ?m_hyp .
            ?m_hyp om:hasNumericalValue ?prevalence_HYP .
          }
          OPTIONAL {
            ?obs_ob a sosa:Observation ;
                    sosa:hasFeatureOfInterest ?lsoa ;
                    sosa:observedProperty mesh:D009765 ;
                    sosa:hasResult ?m_ob .
            ?m_ob om:hasNumericalValue ?prevalence_OB .
          }
          OPTIONAL {
            ?obs_stia a sosa:Observation ;
                      sosa:hasFeatureOfInterest ?lsoa ;
                      sosa:observedProperty mesh:D020521 ;
                      sosa:hasResult ?m_stia .
            ?m_stia om:hasNumericalValue ?prevalence_STIA .
          }
        }
        LIMIT 100000
        """
        headers = {
            'Accept': 'application/sparql-results+json',
            'Content-Type': 'application/x-www-form-urlencoded'
        }
        params = {'query': query}
        response = requests.post(endpoint, data=params, headers=headers, timeout=60)
        if response.status_code != 200:
            raise Exception(f"SPARQL query failed with status code {response.status_code}: {response.text}")
        result = response.json()
        if 'results' not in result or 'bindings' not in result['results']:
            raise Exception("Invalid SPARQL response format")
        bindings = result['results']['bindings']
        if len(bindings) == 0:
            raise Exception("No data returned from SPARQL query - KG may not contain required data yet")
        columns = list(bindings[0].keys())
        rows = []
        for binding in bindings:
            row = {}
            for col in columns:
                if col in binding:
                    val = binding[col].get('value', None)
                    if binding[col].get('type') == 'literal':
                        try:
                            num_val = float(val)
                            row[col] = int(num_val) if num_val.is_integer() else num_val
                        except (ValueError, TypeError):
                            row[col] = str(val)
                    else:
                        row[col] = val
                else:
                    row[col] = None
            rows.append(row)
        df = pd.DataFrame(rows)
        column_mapping = {
            'lsoa_code': 'lsoa_code',
            'prevalence_CHD': 'prevalence-CHD',
            'prevalence_COPD': 'prevalence-COPD',
            'prevalence_DM': 'prevalence-DM',
            'prevalence_HF': 'prevalence-HF',
            'prevalence_HYP': 'prevalence-HYP',
            'prevalence_OB': 'prevalence-OB',
            'prevalence_STIA': 'prevalence-STIA',
        }
        for old_col, new_col in column_mapping.items():
            if old_col in df.columns:
                df = df.rename(columns={old_col: new_col})
        if 'lsoa_code' not in df.columns or df.empty:
            raise Exception("SPARQL query returned insufficient data - missing required columns")
        print(f"[TWA] Successfully queried {len(df)} rows from SPARQL endpoint")
        return df
    except Exception as e:
        print(f"[TWA] SPARQL query failed: {str(e)}")
        if csv_fallback_path and os.path.exists(csv_fallback_path):
            print(f"[TWA] Falling back to local CSV: {csv_fallback_path}")
            return pd.read_csv(csv_fallback_path)
        elif csv_fallback_path:
            raise FileNotFoundError(f"Fallback CSV file not found: {csv_fallback_path}")
        else:
            raise Exception(f"SPARQL query failed and no fallback CSV provided: {str(e)}")

# ========== Utility Functions ==========
def _parse_bool_or_none(x: str):
    """Parse string to boolean or None"""
    if x is None: return None
    s = x.strip().lower()
    if s in ("1","true","t","yes","y","on"): return True
    if s in ("0","false","f","no","n","off"): return False
    if s in ("none","null","nan",""): return None
    raise argparse.ArgumentTypeError("must be true/false/none")

def _parse_bool(x: str, default: bool = False) -> bool:
    """Parse string to boolean with default"""
    if x is None: return default
    s = str(x).strip().lower()
    if s in ("1","true","t","yes","y","on"): return True
    if s in ("0","false","f","no","n","off"): return False
    return default

def _safe_zscore(s: pd.Series) -> pd.Series:
    """Compute z-scores with robust error handling"""
    s = pd.to_numeric(s, errors="coerce")
    m = s.mean(skipna=True)
    v = s.var(ddof=0, skipna=True)
    if not np.isfinite(v) or v <= 0:
        return pd.Series(0.0, index=s.index)
    z = (s - m) / np.sqrt(v)
    return z.fillna(0.0)

def sanitize_inputs_for_glm(y: pd.Series, N: pd.Series, X: pd.DataFrame, drop_cols_if_allnan=True, min_samples=None):
    """Clean and validate inputs for GLM fitting"""
    if min_samples is None:
        min_samples = MIN_SAMPLES_FOR_GLM
    y = pd.to_numeric(y, errors="coerce").replace([np.inf, -np.inf], np.nan)
    N = pd.to_numeric(N, errors="coerce").replace([np.inf, -np.inf], np.nan)
    X = X.replace([np.inf, -np.inf], np.nan).copy()
    if drop_cols_if_allnan:
        all_nan_cols = [c for c in X.columns if X[c].isna().all()]
        if all_nan_cols:
            X = X.drop(columns=all_nan_cols)
    row_ok = (N > 0) & y.notna() & X.notna().all(axis=1)
    if row_ok.sum() < min_samples:
        raise RuntimeError(f"Insufficient valid samples: {int(row_ok.sum())} < {min_samples}")
    return y.loc[row_ok], N.loc[row_ok], X.loc[row_ok], row_ok

def _as_prop(series):
    """Convert series to proportion format (values between 0 and 1)"""
    x = pd.to_numeric(series, errors='coerce').astype(float)
    result = np.where(x <= 1.0, x, x / 100.0)
    return pd.Series(result, index=series.index)

def exposure_transform(raw, mode, logit_eps: float = 0.001):
    """Apply transformation to exposure variable"""
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

# ========== Control Variable Construction ==========
def build_air_quality_vars(df):
    """Construct air quality control variables"""
    cols = [c for c in ['ah4no2', 'ah4so2', 'ah4pm10'] if c in df.columns]
    if not cols: return pd.DataFrame(index=df.index)
    out = {}
    for c in cols:
        out[f"air_{c}"] = _safe_zscore(pd.to_numeric(df[c], errors='coerce'))
    return pd.DataFrame(out, index=df.index)

def build_healthcare_access_vars(df):
    """Construct healthcare access control variable"""
    if 'ah4gp' not in df.columns:
        return pd.DataFrame(index=df.index)
    gp = pd.to_numeric(df['ah4gp'], errors='coerce')
    gp = gp.where(gp >= 0)
    gp_log = np.log1p(gp)
    gp_z = _safe_zscore(gp_log)
    return pd.DataFrame({'healthcare_access': gp_z}, index=df.index)

def _safe_corr(a: pd.Series, b: pd.Series, method: str = "pearson") -> float:
    """Compute correlation with robust error handling"""
    a = pd.to_numeric(a, errors="coerce").astype(float)
    b = pd.to_numeric(b, errors="coerce").astype(float)
    df = pd.concat([a.rename("a"), b.rename("b")], axis=1).replace([np.inf, -np.inf], np.nan).dropna()
    if len(df) < 3:
        return float("nan")
    if method == "spearman":
        aa = df["a"].rank(method="average", na_option="keep")
        bb = df["b"].rank(method="average", na_option="keep")
        return float(np.corrcoef(aa, bb)[0, 1])
    return float(np.corrcoef(df["a"], df["b"])[0, 1])

def _select_imd_cols_for_exposure(df: pd.DataFrame, exposure_name: str, exposure_series: pd.Series,
                                  imd_cols=None, socioeconomic_exposures=None, method=None, threshold=None, min_n=None):
    """Select IMD columns for exposure, handling collinearity"""
    if imd_cols is None:
        imd_cols = IMD_FULL_COLS
    if socioeconomic_exposures is None:
        socioeconomic_exposures = SOCIOECONOMIC_IMD_COLLINEARITY_EXPOSURES
    if method is None:
        method = IMD_COLLINEARITY_METHOD
    if threshold is None:
        threshold = IMD_COLLINEARITY_ABS_CORR_THRESHOLD
    if min_n is None:
        min_n = IMD_COLLINEARITY_MIN_N
        
    base_cols = [c for c in imd_cols if c in df.columns]
    if (not base_cols) or (exposure_name not in socioeconomic_exposures):
        return base_cols, []
    kept = []
    dropped = []
    for c in base_cols:
        r = _safe_corr(exposure_series, df[c], method=method)
        if np.isfinite(r) and (abs(r) >= float(threshold)):
            dropped.append((c, float(r)))
        else:
            kept.append(c)
    exp_num = pd.to_numeric(exposure_series, errors="coerce").astype(float)
    imd_num = df[base_cols].apply(pd.to_numeric, errors="coerce").astype(float)
    n_eff = pd.concat([exp_num.rename("__EXPO__"), imd_num], axis=1).dropna().shape[0]
    if n_eff < int(min_n):
        return base_cols, []
    return kept, dropped

def build_imd_vars(df, exposure_name: str, exposure_series: pd.Series, debug_print=None, **kwargs):
    """Build IMD control variables with collinearity handling"""
    if debug_print is None:
        debug_print = DEBUG_PRINT
    cols, dropped = _select_imd_cols_for_exposure(df, exposure_name, exposure_series, **kwargs)
    if not cols:
        return pd.DataFrame(index=df.index)
    out = {}
    for c in cols:
        out[f"imd_{c}"] = pd.to_numeric(df[c], errors="coerce").astype(float)
    if dropped and debug_print:
        threshold = kwargs.get('threshold', IMD_COLLINEARITY_ABS_CORR_THRESHOLD)
        dropped_str = ", ".join([f"{c} (r={r:+.2f})" for c, r in dropped])
        print(f"[IMD collinearity] exposure='{exposure_name}': dropped IMD cols due to |r| >= {threshold}: {dropped_str}")
    return pd.DataFrame(out, index=df.index)

def build_age_vars(df, mode='ai', age_cols=None, age_drop=None):
    """Build age control variables"""
    if age_cols is None:
        age_cols = AGE_6
    if age_drop is None:
        age_drop = AGE_DROP
    if mode == 'ai':
        p65 = pd.to_numeric(df.get('prop_65_74', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_75_84', np.nan), errors='coerce') + \
              pd.to_numeric(df.get('prop_85_plus', np.nan), errors='coerce')
        p014 = pd.to_numeric(df.get('prop_0_14', np.nan), errors='coerce')
        ai = (p65 / p014) * 100.0
        ai.replace([np.inf, -np.inf], np.nan, inplace=True)
        return pd.DataFrame({'age_index': ai.fillna(ai.median())}, index=df.index), ['age_index']
    age_data = pd.DataFrame({c: pd.to_numeric(df[c], errors='coerce').astype(float)
                             for c in age_cols if c in df.columns})
    if age_data.empty:
        return pd.DataFrame(index=df.index), []
    if np.nanmax(age_data.to_numpy()) > 1.0:
        age_data = age_data / 100.0
    use_cols = [c for c in age_data.columns if c != age_drop]
    A = age_data[use_cols].add_prefix("age_")
    return A.fillna(0.0), list(A.columns)

def build_urban_control_vars(df, mode='none', urban_flag_col=None, ruc_col=None):
    """Build urban/rural control variables"""
    if urban_flag_col is None:
        urban_flag_col = URBAN_FLAG_COL
    if ruc_col is None:
        ruc_col = RUC_COL
    if mode == 'none':
        return pd.DataFrame(index=df.index)
    if mode == 'binary':
        if urban_flag_col not in df.columns:
            return pd.DataFrame(index=df.index)
        s = (df[urban_flag_col].astype('string').str.strip().str.lower()
             .map({'urban':'Urban','rural':'Rural'}))
        d = pd.get_dummies(pd.Categorical(s, categories=['Rural','Urban']),
                           drop_first=True, prefix='URB', dtype=float)
        return d
    if mode == 'categorical':
        if ruc_col not in df.columns:
            return pd.DataFrame(index=df.index)
        r = df[ruc_col].astype('string').str.strip().str.upper()
        cats = ['RLF1','RLN1','RSF1','RSN1','UF1','UN1']
        d = pd.get_dummies(pd.Categorical(r, categories=cats),
                           drop_first=False, prefix='RUC', dtype=float)
        if 'RUC_UN1' in d.columns:
            d = d.drop('RUC_UN1', axis=1)
        return d
    return pd.DataFrame(index=df.index)

def create_strata(df, mode, urban_flag_col=None, ruc_col=None):
    """Create stratification series"""
    if urban_flag_col is None:
        urban_flag_col = URBAN_FLAG_COL
    if ruc_col is None:
        ruc_col = RUC_COL
    if mode == 'none':
        return None
    if mode == 'binary':
        return df[urban_flag_col]
    if mode == 'ruc_6cat':
        return df[ruc_col]
    if mode == 'age_structure':
        age_65plus = df['prop_65_74'] + df['prop_75_84'] + df['prop_85_plus']
        return np.where(age_65plus < 0.19, 'Young', 'Old')
    raise ValueError(f"Unknown stratification mode: {mode}")

# ========== PAF Computation ==========
def compute_paf(res_full, X_fit, N_fit, x_raw, design_info, spline_cols, transform_mode, direction, *,
                logit_eps: float = 0.001, pct_list=None):
    """
    Compute Population Attributable Fraction (PAF) for multiple percentiles.
    
    Returns a dict with:
      - toP{p}: PAF value (signed), and
      - tgt_p{p}: raw-scale threshold,
    for each p in pct_list (defaults to PCT_LIST).
    """
    if pct_list is None:
        pct_list = PCT_LIST
    mu_obs = res_full.predict(X_fit, offset=np.log(N_fit)) / N_fit
    cases_obs = float(np.sum(mu_obs * N_fit))
    out = {}
    for p in pct_list:
        if direction == "lower_better":
            tgt = float(np.nanpercentile(x_raw, p))
            x_cf_raw = np.minimum(x_raw, tgt)
        else:
            tgt = float(np.nanpercentile(x_raw, 100 - p))
            x_cf_raw = np.maximum(x_raw, tgt)
        out[f"tgt_p{p}"] = tgt
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
        Xs_cf_mat = build_design_matrices([design_info], {"z": np.asarray(x_cf_tr)})[0]
        Xs_cf = pd.DataFrame(np.asarray(Xs_cf_mat), index=X_fit.index, columns=spline_cols)
        X_cf = X_fit.copy(deep=True)
        for c in spline_cols:
            X_cf[c] = np.asarray(Xs_cf[c], dtype=float)
        mu_cf = res_full.predict(X_cf, offset=np.log(N_fit)) / N_fit
        cases_cf = float(np.sum(mu_cf * N_fit))
        paf_raw = (1.0 - (cases_cf / cases_obs)) if cases_obs > 0 else np.nan
        out[f"toP{p}"] = float(paf_raw) if np.isfinite(paf_raw) else np.nan
    return out

# ========== Configuration Fitting ==========
def run_one_config(df_raw, strata_mode, age_mode, use_air, use_health, urban_ctrl_mode, exposure_cfg,
                   *, sens_scenario: str = "base", spline_df: int = 4, logit_eps: float = 0.001,
                   diseases=None, total_col=None, use_imd=None, min_samples_stratum=None, min_valid_exposure=None,
                   debug_print=None, **kwargs):
    """
    Fit Poisson GLM and compute PAF for a single configuration combination.
    
    Parameters:
        df_raw: Input DataFrame
        strata_mode: Stratification mode
        age_mode: Age control mode ('ai' or '6band')
        use_air: Whether to include air quality controls
        use_health: Whether to include healthcare access controls
        urban_ctrl_mode: Urban control mode
        exposure_cfg: Exposure configuration dict with 'name', 'transform', 'direction'
        sens_scenario: Sensitivity scenario name
        spline_df: Degrees of freedom for splines
        logit_eps: Epsilon for logit transformation
        diseases: List of disease column names (defaults to module default)
        total_col: Population column name (defaults to TOTAL_COL)
        use_imd: Whether to use IMD controls (defaults to USE_IMD_CONTROLS)
        min_samples_stratum: Minimum samples per stratum
        min_valid_exposure: Minimum valid exposure count
        debug_print: Whether to print debug info
        
    Returns:
        pd.DataFrame with results
    """
    if diseases is None:
        # Import from module-level default if available, otherwise use common list
        diseases = ['prevalence-CHD','prevalence-COPD','prevalence-DM','prevalence-HF',
                    'prevalence-HYP','prevalence-OB','prevalence-STIA']
    if total_col is None:
        total_col = TOTAL_COL
    if use_imd is None:
        use_imd = USE_IMD_CONTROLS
    if min_samples_stratum is None:
        min_samples_stratum = MIN_SAMPLES_FOR_STRATUM
    if min_valid_exposure is None:
        min_valid_exposure = MIN_VALID_EXPOSURE
    if debug_print is None:
        debug_print = DEBUG_PRINT
        
    exposure_name = exposure_cfg['name']
    transform_mode = exposure_cfg['transform']
    direction = exposure_cfg['direction']
    strata = create_strata(df_raw, strata_mode, **kwargs)
    strata_levels = ['All'] if strata is None else [lvl for lvl in pd.Series(strata).dropna().unique()]
    strata_data = {'All': df_raw} if strata is None else {lvl: df_raw[strata == lvl] for lvl in strata_levels}
    urban_ctrl_effective = strata_mode in {'none', 'age_structure'}
    effective_urban_mode = urban_ctrl_mode if urban_ctrl_effective else 'n/a'
    rows = []
    
    for level in strata_levels:
        df = strata_data[level]
        if len(df) < min_samples_stratum:
            print(f"      [skip] {level}: sample size {len(df)} < {min_samples_stratum}")
            continue
        x_raw_all = pd.to_numeric(df[exposure_name], errors='coerce')
        x_tr_all, x_raw_scale = exposure_transform(x_raw_all, transform_mode, logit_eps=logit_eps)
        valid_mask = x_tr_all.notna()
        if valid_mask.sum() < min_valid_exposure:
            print(f"      [skip] {level}: valid exposure count {valid_mask.sum()} < {min_valid_exposure}")
            continue
        dfc = df.loc[valid_mask].copy()
        x_tr = x_tr_all[valid_mask]
        x_raw = x_raw_scale[valid_mask]
        spline_df_val = int(spline_df) if spline_df is not None else 4
        Xs = dmatrix(f"cr(z, df={spline_df_val})", {"z": x_tr.values}, return_type="dataframe")
        design_info = Xs.design_info
        spline_cols = [f"EXPO_spline_{i+1}" for i in range(Xs.shape[1])]
        Xs.columns = spline_cols
        Xs.index = dfc.index
        age_df, _ = build_age_vars(dfc, age_mode, **kwargs)
        ctrl = [age_df]
        if use_imd:
            ctrl.append(build_imd_vars(dfc, exposure_name=exposure_name, exposure_series=x_raw, **kwargs))
        if use_air:
            ctrl.append(build_air_quality_vars(dfc))
        if use_health:
            ctrl.append(build_healthcare_access_vars(dfc))
        if urban_ctrl_effective:
            ctrl.append(build_urban_control_vars(dfc, mode=urban_ctrl_mode, **kwargs))
        X = pd.concat(ctrl + [Xs], axis=1)
        X = sm.add_constant(X, has_constant='add')
        
        for disease in diseases:
            if disease not in dfc.columns:
                print(f"      [skip] {disease}: column not found")
                continue
            try:
                prevalence = _as_prop(dfc[disease])
                population = pd.to_numeric(dfc[total_col], errors='coerce').astype(float)
                y = prevalence * population
                y_fit, N_fit, X_fit, row_ok = sanitize_inputs_for_glm(y, population, X, **kwargs)
                x_fit = x_raw.loc[row_ok]
                res_full = sm.GLM(y_fit, X_fit, family=Poisson(), offset=np.log(N_fit)).fit()
                pafs = compute_paf(res_full, X_fit, N_fit, x_fit, design_info, spline_cols, 
                                  transform_mode, direction, logit_eps=logit_eps, **kwargs)
                X_null = sm.add_constant(pd.DataFrame(index=y_fit.index), has_constant='add')
                res_null = sm.GLM(y_fit, X_null, family=Poisson(), offset=np.log(N_fit)).fit()
                r2 = 1 - (res_full.llf / res_null.llf) if res_null.llf != 0 else np.nan
                
                if debug_print:
                    mu_hat = res_full.predict(X_fit, offset=np.log(N_fit))
                    pearson_ratio = (res_full.pearson_chi2 / res_full.df_resid) if res_full.df_resid else np.nan
                    print(f"\n=== DEBUG: {level} {exposure_name} {disease} n={int(len(y_fit))} mcfadden_r2={float(r2):.6g} ===")
                
                pct_list = kwargs.get('pct_list', PCT_LIST)
                row = {
                    'sens_scenario': sens_scenario,
                    'spline_df': int(spline_df_val),
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
                for p in pct_list:
                    row[f'paf_p{p}'] = pafs.get(f'toP{p}', np.nan) * 100
                    row[f'tgt_p{p}'] = pafs.get(f'tgt_p{p}', np.nan)
                rows.append(row)
            except Exception as e:
                print(f"      [skip] {disease}: {type(e).__name__}: {str(e)}")
                continue
    
    if rows:
        return pd.DataFrame(rows)
    pct_list = kwargs.get('pct_list', PCT_LIST)
    base_cols = ['sens_scenario','spline_df','logit_eps','strata_mode','stratum','urban_ctrl_mode',
                 'age_mode','use_air','use_health','exposure','disease','n','mcfadden_r2']
    extra = [f'paf_p{p}' for p in pct_list] + [f'tgt_p{p}' for p in pct_list]
    return pd.DataFrame(columns=base_cols + extra)

# ========== Scoring and Selection ==========
def negativity_count(row, pct_list=None):
    """Count negative PAF values in a result row"""
    if pct_list is None:
        pct_list = PCT_LIST
    cnt = 0
    for p in pct_list:
        v = row.get(f'paf_p{p}', np.nan)
        if np.isfinite(v) and v < 0:
            cnt += 1
    return cnt

def score_block(df_block, pct_list=None):
    """Score a block of results"""
    if pct_list is None:
        pct_list = PCT_LIST
    if df_block.empty:
        return 10**9, -np.inf, -np.inf, -np.inf
    negs = df_block.apply(lambda r: negativity_count(r, pct_list), axis=1).sum()
    paf_pos_cols = [f'paf_p{p}' for p in pct_list]
    paf_pos = df_block[paf_pos_cols].clip(lower=0)
    mean_paf = float(paf_pos.stack().mean(skipna=True))
    mean_r2 = float(df_block['mcfadden_r2'].mean(skipna=True))
    score = mean_paf + 20.0*mean_r2
    return int(negs), mean_paf, mean_r2, score

def select_best_combo(df_all, exposure, pct_list=None):
    """Select best configuration for a given exposure"""
    if pct_list is None:
        pct_list = PCT_LIST
    df_exp = df_all[df_all['exposure'] == exposure].copy()
    if df_exp.empty:
        return None, None
    keys = ['sens_scenario','spline_df','logit_eps','strata_mode','urban_ctrl_mode',
            'age_mode','use_air','use_health']
    best = None
    best_row = None
    for combo, df_block in df_exp.groupby(keys):
        neg_total, mean_paf, mean_r2, score = score_block(df_block, pct_list)
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
        if best is None or (rec['negatives'] < best['negatives']) or \
           (rec['negatives'] == best['negatives'] and rec['score'] > best['score']):
            best = rec
            best_row = df_block
    return best, best_row

