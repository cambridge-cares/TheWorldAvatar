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
import matplotlib.pyplot as plt
import seaborn as sns

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Global parameters: sample ratio, default 100%
SAMPLE_RATIO = 1.0
SUMMARY_FILE_PATH = ""

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

TRAINING_DATA_FILES = config['TRAINING_DATA_PATHS']
ANALYSIS_DATA_FILES = config['ANALYSIS_DATA_PATHS']
MODEL_SAVE_PATH = config['XGBOOST_MODEL_PATH']
SCALER_SAVE_PATH = config['SCALER_PATH']


def mean_absolute_percentage_error(y_true, y_predicted):
    y_true, y_predicted = np.array(y_true), np.array(y_predicted)
    return np.mean(np.abs((y_true - y_predicted) / (y_true + np.finfo(float).eps))) * 100


columns_to_map = [
    'WALLS_ENERGY_EFF', 'WINDOWS_ENERGY_EFF', 'ROOF_ENERGY_EFF',
    'MAINHEAT_ENERGY_EFF', 'MAINHEATC_ENERGY_EFF', 'LIGHTING_ENERGY_EFF', 'HOT_WATER_ENERGY_EFF'
]

columns_to_process = columns_to_map + ['NUMBER_HEATED_ROOMS', 'PROPERTY_TYPE', 'BUILT_FORM',
                                       'POTENTIAL_ENERGY_EFFICIENCY', 'ENERGY_CONSUMPTION_POTENTIAL']

efficiency_scale = {
    "very poor": 0, "poor": 0.25, "average": 0.5, "good": 0.75, "very good": 1.0
}

property_type_scale = {
    "flat": 1, "house": 2, "maisonette": 3, "bungalow": 4, "park home": 5
}

built_form_mapping = {
    "semi-detached": 1, "mid-terrace": 2, "detached": 3, "end-terrace": 4,
    "enclosed end-terrace": 5, "enclosed mid-terrace": 6
}


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


def get_authority_code(path):
    parent_dir = os.path.basename(os.path.dirname(path))
    parts = parent_dir.split('-')
    if len(parts) >= 2:
        return parts[1]
    else:
        return ''


def load_and_sample_houses(file_paths, sample_ratio):
    data_frames = []
    total_rows = 0
    total_low_efficiency = 0
    for file_path in file_paths:
        logging.info(f"Loading data: {file_path}")
        try:
            df = pd.read_csv(file_path)
            total_rows += len(df)

            low_efficiency_houses = df[df['CURRENT_ENERGY_EFFICIENCY'] < 69]
            total_low_efficiency += len(low_efficiency_houses)
            data_frames.append(low_efficiency_houses)
        except Exception as e:
            logging.error(f"Error loading data {file_path}: {str(e)}")

    combined_low_efficiency_houses = pd.concat(data_frames, ignore_index=True)

    sample_size = max(1, int(sample_ratio * total_low_efficiency))
    sampled_houses = combined_low_efficiency_houses.sample(n=sample_size, random_state=42)

    return sampled_houses, total_rows, total_low_efficiency, sample_size


def train_and_evaluate(model, X_train, X_val, X_test, y_train, y_val, y_test):
    logging.info("Training XGBoost model...")

    model.fit(X_train, y_train)

    logging.info("Evaluating model performance...")
    y_pred_train = model.predict(X_train)
    y_pred_val = model.predict(X_val)
    y_pred_test = model.predict(X_test)

    r2_train = r2_score(y_train, y_pred_train)
    r2_val = r2_score(y_val, y_pred_val)
    r2_test = r2_score(y_test, y_pred_test)

    mape_train = mean_absolute_percentage_error(y_train, y_pred_train)
    mape_val = mean_absolute_percentage_error(y_val, y_pred_val)
    mape_test = mean_absolute_percentage_error(y_test, y_pred_test)

    logging.info("Model Performance:")
    logging.info(f"R² (Train): {r2_train:.4f}")
    logging.info(f"R² (Validation): {r2_val:.4f}")
    logging.info(f"R² (Test): {r2_test:.4f}")
    logging.info(f"MAPE (Train): {mape_train:.4f}%")
    logging.info(f"MAPE (Validation): {mape_val:.4f}%")
    logging.info(f"MAPE (Test): {mape_test:.4f}%")

    return model


def train_model(X, y):
    logging.info("Splitting data and scaling features...")
    X_train_val, X_test, y_train_val, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    X_train, X_val, y_train, y_val = train_test_split(
        X_train_val, y_train_val, test_size=0.125, random_state=42)  # 0.125 = 10% / 80%

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

    xgb_model = train_and_evaluate(xgb_model, X_train_scaled, X_val_scaled, X_test_scaled, y_train, y_val, y_test)

    return xgb_model, scaler


def predict_efficiency(model, scaler, features):
    features_df = pd.DataFrame([features], columns=feature_columns)
    scaled_features = pd.DataFrame(scaler.transform(features_df), columns=feature_columns)
    return model.predict(scaled_features)[0]


def get_next_efficiency_level(current_level):
    levels = sorted(list(efficiency_scale.values()))
    for level in levels:
        if current_level < level:
            return level
    return current_level


fabric_costs = {
    'Small flat (<54m²)': 2800,
    'Large flat (>54m²)': 3500,
    'Small mid-terrace house (<76m²)': 3700,
    'Large mid-terrace house (>76m²)': 4000,
    'Small semi-detached or end-terrace (<80m²)': 6800,
    'Large semi-detached or end terrace (>80m²)': 7000,
    'Small detached house (<117m²)': 7200,
    'Large detached house (>117m²)': 9400,
    'Bungalow (around 117m²)': 6300
}

system_costs = {
    'Small flat (<54m²)': 3800,
    'Large flat (>54m²)': 4200,
    'Small mid-terrace house (<76m²)': 4500,
    'Large mid-terrace house (>76m²)': 5000,
    'Small semi-detached or end-terrace (<80m²)': 5500,
    'Large semi-detached or end terrace (>80m²)': 6000,
    'Small detached house (<117m²)': 6500,
    'Large detached house (>117m²)': 8000,
    'Bungalow (around 117m²)': 7000
}


def determine_house_type(floor_area):
    if floor_area < 54:
        return 'Small flat (<54m²)'
    elif 54 <= floor_area < 76:
        return 'Large flat (>54m²)'
    elif 76 <= floor_area < 80:
        return 'Small mid-terrace house (<76m²)'
    elif 80 <= floor_area < 117:
        return 'Large mid-terrace house (>76m²)'
    elif floor_area >= 117:
        return 'Bungalow (around 117m²)'
    else:
        return None


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

        if index == 999:
            logging.info("After 1000 houses:")
            logging.info(f"Fabric First count: {fabric_first_total}")
            logging.info(f"System First count: {system_first_total}")

    logging.info("Final results:")
    logging.info(f"Total Fabric First houses: {fabric_first_total}")
    for key, count in fabric_first_by_form.items():
        logging.info(f"Fabric First - Form {key}: {count}")
    logging.info(f"Total System First houses: {system_first_total}")
    for key, count in system_first_by_form.items():
        logging.info(f"System First - Form {key}: {count}")

    return pd.DataFrame(results), fabric_first_total, system_first_total, fabric_first_by_form, system_first_by_form


def visualize_results(results):
    plt.figure(figsize=(12, 6))
    sns.scatterplot(data=results, x='original_efficiency', y='fabric_improvement', label='Fabric Improvement')
    sns.scatterplot(data=results, x='original_efficiency', y='system_improvement', label='System Improvement')
    plt.title('Improvement vs Original Efficiency')
    plt.xlabel('Original Efficiency')
    plt.ylabel('Improvement')
    plt.legend()
    plt.savefig('improvement_scatter.png')
    plt.close()

    plt.figure(figsize=(10, 6))
    labels = ['Fabric First', 'System First']
    sizes = [results['decision'].value_counts()['fabric first'],
             results['decision'].value_counts()['system first']]
    plt.pie(sizes, labels=labels, autopct='%1.1f%%', startangle=90)
    plt.axis('equal')
    plt.title('Fabric First vs System First Proportion')
    plt.savefig('improvement_pie.png')
    plt.close()


if __name__ == "__main__":
    try:
        feature_columns = columns_to_map + ['TOTAL_FLOOR_AREA', 'NUMBER_HEATED_ROOMS', 'PROPERTY_TYPE',
                                            'BUILT_FORM', 'POTENTIAL_ENERGY_EFFICIENCY',
                                            'ENERGY_CONSUMPTION_POTENTIAL']

        # Check if model and scaler exist
        if os.path.exists(MODEL_SAVE_PATH) and os.path.exists(SCALER_SAVE_PATH):
            logging.info("Loading existing model and scaler...")
            with open(MODEL_SAVE_PATH, 'rb') as model_file:
                xgb_model = pickle.load(model_file)
            with open(SCALER_SAVE_PATH, 'rb') as scaler_file:
                scaler = pickle.load(scaler_file)
        else:
            # Function load_and_clean_data needs to be defined or imported before use
            train_df = load_and_clean_data(TRAINING_DATA_FILES, for_training=True)[0]
            logging.info(f"Total training data rows: {len(train_df)}")

            X = train_df[feature_columns]
            y = train_df['CURRENT_ENERGY_EFFICIENCY']

            xgb_model, scaler = train_model(X, y)

            # Save model and scaler
            with open(MODEL_SAVE_PATH, 'wb') as model_file:
                pickle.dump(xgb_model, model_file)
            with open(SCALER_SAVE_PATH, 'wb') as scaler_file:
                pickle.dump(scaler, scaler_file)

        # Process each analysis file path
        for analysis_file in ANALYSIS_DATA_FILES:
            sampled_houses, total_rows, total_low_efficiency, sampled_size = load_and_sample_houses(
                [analysis_file], SAMPLE_RATIO)
            logging.info(f"Total rows: {total_rows}, Total low-efficiency houses: {total_low_efficiency}, Sampled houses: {sampled_size}")

            processed_houses = preprocess_data(sampled_houses)

            authority_code = get_authority_code(analysis_file)

            results_df, fabric_first_total, system_first_total, fabric_first_by_form, system_first_by_form = analyze_houses(
                xgb_model, scaler, processed_houses, feature_columns)

            output_file_path = os.path.splitext(analysis_file)[0] + '_results.xlsx'
            results_df.to_excel(output_file_path, index=False)
            logging.info(f"Detailed results saved to {output_file_path}")

            save_summary(
                analysis_file=analysis_file,
                total_rows=total_rows,
                low_efficiency_count=total_low_efficiency,
                sample_ratio=SAMPLE_RATIO,
                sampled_count=sampled_size,
                fabric_first_total=fabric_first_total,
                system_first_total=system_first_total,
                fabric_first_by_form=fabric_first_by_form,
                system_first_by_form=system_first_by_form,
                authority_code=authority_code
            )

    except Exception as e:
        logging.error(f"An error occurred during execution: {str(e)}")
