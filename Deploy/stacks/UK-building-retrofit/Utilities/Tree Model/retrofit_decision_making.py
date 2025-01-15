import pandas as pd
import numpy as np
import os
import pickle
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler
from xgboost import XGBRegressor
from sklearn.metrics import r2_score
from tqdm import tqdm
from scipy import stats
import logging
import json
import urllib.request
from urllib.parse import urlencode
import matplotlib.pyplot as plt
import seaborn as sns

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Global parameters: sample ratio, default 100%
SAMPLE_RATIO = 1.0
SUMMARY_FILE_PATH = ""

# Load configuration from JSON file
def load_configuration(config_path='xgboost.json'):
    try:
        with open(config_path, 'r') as file:
            return json.load(file)
    except FileNotFoundError:
        logging.error(f"Configuration file {config_path} not found")
        raise
    except json.JSONDecodeError:
        logging.error(f"Configuration file {config_path} format error")
        raise


config = load_configuration()

TRAINING_LA_CODES_PATH = config['TRAINING_LA_CODES_PATH']
ANALYSIS_LA_CODES_PATH = config['ANALYSIS_LA_CODES_PATH']
MODEL_SAVE_PATH = config['XGBOOST_MODEL_PATH']
SCALER_SAVE_PATH = config['SCALER_PATH']

API_BASE_URL = 'https://epc.opendatacommunities.org/api/v1/domestic/search'
API_TOKEN = config['API_TOKEN']

headers = {
    'Accept': 'text/csv',
    'Authorization': f'Basic {API_TOKEN}'
}

# Load Local Authority Codes from CSV
def load_la_codes_from_csv(csv_path):
    try:
        df = pd.read_csv(csv_path)
        return df['LACODE'].tolist()
    except FileNotFoundError:
        logging.error(f"CSV file {csv_path} not found")
        raise
    except Exception as e:
        logging.error(f"Error loading CSV file {csv_path}: {str(e)}")
        raise

TRAINING_LA_CODES = load_la_codes_from_csv(TRAINING_LA_CODES_PATH)
ANALYSIS_LA_CODES = load_la_codes_from_csv(ANALYSIS_LA_CODES_PATH)

# Function to get data from the API for given LACODEs
def get_data_from_api(lacodes):
    data_frames = []
    for lacode in lacodes:
        query_params = {'local-authority': lacode}
        encoded_params = urlencode(query_params)
        full_url = f"{API_BASE_URL}?{encoded_params}"
        try:
            with urllib.request.urlopen(urllib.request.Request(full_url, headers=headers)) as response:
                response_body = response.read()
                df = pd.read_csv(pd.compat.StringIO(response_body.decode()))
                data_frames.append(df)
        except Exception as e:
            logging.error(f"Error retrieving data for LA Code {lacode}: {str(e)}")

    if data_frames:
        return pd.concat(data_frames, ignore_index=True)
    else:
        logging.error("No data retrieved from the API.")
        raise ValueError("No data retrieved from the API.")

# Save summary to Excel
def save_summary(analysis_file, total_rows, low_efficiency_count, sample_ratio, sampled_count,
                 fabric_first_total, system_first_total, fabric_first_by_form, system_first_by_form,
                 authority_code):
    summary_data = {
        'analysis_file': analysis_file,
        'total_rows': total_rows,
        'low_efficiency_count': low_efficiency_count,
        'sample_ratio': sample_ratio,
        'sampled_count': sampled_count,
        'fabric_first_total': fabric_first_total,
        'system_first_total': system_first_total
    }

    # Add counts for each built_form to summary_data
    for form_value in built_form_mapping.values():
        summary_data[f'fabric_first_form_{form_value}'] = fabric_first_by_form.get(form_value, 0)
        summary_data[f'system_first_form_{form_value}'] = system_first_by_form.get(form_value, 0)

    summary_data['authority_code'] = authority_code

    if not os.path.exists(SUMMARY_FILE_PATH):
        summary_df = pd.DataFrame([summary_data])
        summary_df.to_excel(SUMMARY_FILE_PATH, index=False)
    else:
        summary_df = pd.read_excel(SUMMARY_FILE_PATH)
        summary_df = pd.concat([summary_df, pd.DataFrame([summary_data])], ignore_index=True)
        summary_df.to_excel(SUMMARY_FILE_PATH, index=False)

    logging.info(f"Summary results saved to {SUMMARY_FILE_PATH}")

# Impute missing values
def impute_missing_value(df, row, column):
    built_form = row['BUILT_FORM']
    property_type = row['PROPERTY_TYPE']
    floor_area = row['TOTAL_FLOOR_AREA']

    if pd.isnull(floor_area):
        return None

    area_lower_bound = floor_area * 0.9
    area_upper_bound = floor_area * 1.1

    conditions = [
        (df['BUILT_FORM'] == built_form) &
        (df['PROPERTY_TYPE'] == property_type) &
        (df['TOTAL_FLOOR_AREA'].between(area_lower_bound, area_upper_bound)),

        (df['BUILT_FORM'] == built_form) &
        (df['TOTAL_FLOOR_AREA'].between(area_lower_bound, area_upper_bound)),

        (df['TOTAL_FLOOR_AREA'].between(area_lower_bound, area_upper_bound)),

        (df['BUILT_FORM'] == built_form) &
        (df['PROPERTY_TYPE'] == property_type)
    ]

    for condition in conditions:
        matching_rows = df[condition & df[column].notnull()]
        if not matching_rows.empty:
            return stats.mode(matching_rows[column], keepdims=True)[0][0]

    return stats.mode(df[column].dropna(), keepdims=True)[0][0]

# Preprocess data
def preprocess_data(df):
    df_filled = df.copy()
    for column in tqdm(columns_to_process, desc="Processing data"):
        if column in columns_to_map:
            df_filled[column] = df_filled[column].str.lower().map(efficiency_scale)
        elif column == 'PROPERTY_TYPE':
            df_filled[column] = df_filled[column].str.lower().map(property_type_scale)
        elif column == 'BUILT_FORM':
            df_filled[column] = df_filled[column].str.lower().map(built_form_mapping)

        missing_mask = df_filled[column].isnull()
        df_filled.loc[missing_mask, column] = df_filled[missing_mask].apply(
            lambda row: impute_missing_value(df_filled, row, column), axis=1)

    return df_filled

# Analyze houses
def analyze_houses(model, scaler, houses, feature_columns):
    fabric_first_total = 0
    system_first_total = 0

    # Initialize counts for each BUILT_FORM in both strategies
    fabric_first_by_form = {key: 0 for key in built_form_mapping.values()}
    system_first_by_form = {key: 0 for key in built_form_mapping.values()}

    results = []

    logging.info("Analyzing low-efficiency houses...")
    for index, (_, house) in enumerate(tqdm(houses.iterrows(), total=len(houses), desc="Analyzing houses")):
        original_features = house[feature_columns].to_dict()
        original_efficiency = predict_efficiency(model, scaler, original_features)

        floor_area = house['TOTAL_FLOOR_AREA']
        house_type = determine_house_type(floor_area)

        if house_type is None or house_type not in fabric_costs or house_type not in system_costs:
            continue

        fabric_cost = fabric_costs[house_type]
        system_cost = system_costs[house_type]

        # Fabric First strategy
        max_fabric_improvement = 0
        for col in columns_to_map[:3]:
            updated_features = original_features.copy()
            updated_features[col] = get_next_efficiency_level(updated_features[col])
            new_efficiency = predict_efficiency(model, scaler, updated_features)
            improvement = max(0, new_efficiency - original_efficiency)
            max_fabric_improvement = max(max_fabric_improvement, improvement)

        # System First strategy
        max_system_improvement = 0
        for col in ['MAINHEAT_ENERGY_EFF', 'MAINHEATC_ENERGY_EFF', 'HOT_WATER_ENERGY_EFF']:
            updated_features = original_features.copy()
            updated_features[col] = get_next_efficiency_level(updated_features[col])
            new_efficiency = predict_efficiency(model, scaler, updated_features)
            improvement = max(0, new_efficiency - original_efficiency)
            max_system_improvement = max(max_system_improvement, improvement)

        # Adjust system improvement based on cost ratio
        cost_ratio = system_cost / fabric_cost
        adjusted_system_improvement = max_system_improvement / cost_ratio

        built_form_type = original_features['BUILT_FORM']

        # Decision logic
        if max_fabric_improvement > adjusted_system_improvement:
            decision = 'fabric first'
            fabric_first_total += 1
            fabric_first_by_form[built_form_type] += 1
        elif adjusted_system_improvement > max_fabric_improvement:
            decision = 'system first'
            system_first_total += 1
            system_first_by_form[built_form_type] += 1
        else:
            if original_efficiency < 64:
                decision = 'fabric first'
                fabric_first_total += 1
                fabric_first_by_form[built_form_type] += 1
            else:
                decision = 'system first'
                system_first_total += 1
                system_first_by_form[built_form_type] += 1

        results.append({
            'original_efficiency': original_efficiency,
            'fabric_improvement': max_fabric_improvement,
            'system_improvement': adjusted_system_improvement,
            'decision': decision,
            'built_form': built_form_type
        })

    logging.info("Final results:")
    logging.info(f"Total Fabric First houses: {fabric_first_total}")
    for key, count in fabric_first_by_form.items():
        logging.info(f"Fabric First - Form {key}: {count}")
    logging.info(f"Total System First houses: {system_first_total}")
    for key, count in system_first_by_form.items():
        logging.info(f"System First - Form {key}: {count}")

    return pd.DataFrame(results), fabric_first_total, system_first_total, fabric_first_by_form, system_first_by_form

# Predict efficiency level
def predict_efficiency(model, scaler, features):
    features_df = pd.DataFrame([features], columns=feature_columns)
    scaled_features = pd.DataFrame(scaler.transform(features_df), columns=feature_columns)
    return model.predict(scaled_features)[0]

# Training data
try:
    train_df = get_data_from_api(TRAINING_LA_CODES)
    logging.info(f"Total training data rows: {len(train_df)}")

    feature_columns = [
        'WALLS_ENERGY_EFF', 'WINDOWS_ENERGY_EFF', 'ROOF_ENERGY_EFF',
        'MAINHEAT_ENERGY_EFF', 'MAINHEATC_ENERGY_EFF', 'LIGHTING_ENERGY_EFF', 'HOT_WATER_ENERGY_EFF',
        'TOTAL_FLOOR_AREA', 'NUMBER_HEATED_ROOMS', 'PROPERTY_TYPE',
        'BUILT_FORM', 'POTENTIAL_ENERGY_EFFICIENCY',
        'ENERGY_CONSUMPTION_POTENTIAL'
    ]

    X = train_df[feature_columns]
    y = train_df['CURRENT_ENERGY_EFFICIENCY']

    if os.path.exists(MODEL_SAVE_PATH) and os.path.exists(SCALER_SAVE_PATH):
        logging.info("Loading existing model and scaler...")
        with open(MODEL_SAVE_PATH, 'rb') as model_file:
            xgb_model = pickle.load(model_file)
        with open(SCALER_SAVE_PATH, 'rb') as scaler_file:
            scaler = pickle.load(scaler_file)
    else:
        logging.info("Training new model...")
        X_train_val, X_test, y_train_val, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
        X_train, X_val, y_train, y_val = train_test_split(X_train_val, y_train_val, test_size=0.125, random_state=42)

        scaler = StandardScaler()
        X_train_scaled = scaler.fit_transform(X_train)
        X_val_scaled = scaler.transform(X_val)
        X_test_scaled = scaler.transform(X_test)

        xgb_model = XGBRegressor(random_state=42)
        param_grid = {
            'n_estimators': [100, 300, 500],
            'learning_rate': [0.01, 0.1, 0.3],
            'max_depth': [3, 5, 7],
            'subsample': [0.7, 0.8, 0.9],
            'colsample_bytree': [0.7, 0.8, 0.9]
        }

        grid_search = GridSearchCV(xgb_model, param_grid, cv=5, scoring='r2', n_jobs=-1, verbose=2)
        grid_search.fit(X_train_scaled, y_train)
        logging.info(f"Best parameters found: {grid_search.best_params_}")
        xgb_model = grid_search.best_estimator_

        with open(MODEL_SAVE_PATH, 'wb') as model_file:
            pickle.dump(xgb_model, model_file)
        with open(SCALER_SAVE_PATH, 'wb') as scaler_file:
            pickle.dump(scaler, scaler_file)

# Analysis data
    analysis_df = get_data_from_api(ANALYSIS_LA_CODES)
    logging.info(f"Total analysis data rows: {len(analysis_df)}")

    processed_houses = preprocess_data(analysis_df)

    results_df, fabric_first_total, system_first_total, fabric_first_by_form, system_first_by_form = analyze_houses(
        xgb_model, scaler, processed_houses, feature_columns)

    output_file_path = "analysis_results.xlsx"
    results_df.to_excel(output_file_path, index=False)
    logging.info(f"Detailed results saved to {output_file_path}")

    save_summary(
        analysis_file="API Analysis",
        total_rows=len(analysis_df),
        low_efficiency_count=len(processed_houses),
        sample_ratio=SAMPLE_RATIO,
        sampled_count=len(processed_houses),
        fabric_first_total=fabric_first_total,
        system_first_total=system_first_total,
        fabric_first_by_form=fabric_first_by_form,
        system_first_by_form=system_first_by_form,
        authority_code="API Analysis"
    )

except Exception as e:
    logging.error(f"An error occurred during execution: {str(e)}")
