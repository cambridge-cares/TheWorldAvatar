import pandas as pd
import numpy as np
import os
import json
from SPARQLWrapper import SPARQLWrapper, JSON

# Load configuration for criteria weighting method
config_path = os.path.join(os.path.dirname(__file__), "criteria_weight.json")
if os.path.exists(config_path):
    with open(config_path, 'r') as config_file:
        config = json.load(config_file)
else:
    config = {
        "weighting_method": "equal_weight"  # Default: "equal_weight"; Options: "entropy_weight_method"
    }
    with open(config_path, 'w') as config_file:
        json.dump(config, config_file, indent=4)

# Load SPARQL endpoint configuration
stack_config_path = os.path.join(os.path.dirname(__file__), "stack_topsis.json")
if not os.path.exists(stack_config_path):
    raise FileNotFoundError("stack_topsis.json configuration file not found.")

with open(stack_config_path, 'r') as stack_config_file:
    stack_config = json.load(stack_config_file)

sparql_endpoint = stack_config.get("sparql_endpoint")
sparql_query_file = stack_config.get("sparql_query_file")

if not sparql_endpoint or not sparql_query_file:
    raise ValueError("SPARQL endpoint or query file not defined in the configuration.")

sparql_query_path = os.path.join(os.path.dirname(__file__), sparql_query_file)
if not os.path.exists(sparql_query_path):
    raise FileNotFoundError(f"SPARQL query file {sparql_query_file} not found.")

with open(sparql_query_path, 'r') as query_file:
    sparql_query = query_file.read()

# Execute SPARQL query
print("Executing SPARQL query...")
sparql = SPARQLWrapper(sparql_endpoint)
sparql.setQuery(sparql_query)
sparql.setReturnFormat(JSON)

try:
    results = sparql.query().convert()
except Exception as e:
    raise RuntimeError(f"Error executing SPARQL query: {e}")

# Convert SPARQL results to DataFrame
data_list = []
for result in results["results"]["bindings"]:
    data_list.append({
        "LSOA Code": result["lsoa_code"]["value"],
        "Proportion of households fuel poor (%)": float(result["fuel_poor_percentage"]["value"]),
        "AVG_CURRENT_ENERGY_EFFICIENCY": float(result["avg_energy_efficiency"]["value"]),
        "total_consumption_scaled": float(result["total_consumption_scaled"]["value"]),
        "Coronary heart disease": float(result["coronary_heart_disease"]["value"]),
        "COPD": float(result["copd"]["value"])
    })

data = pd.DataFrame(data_list)

# Select columns of interest
columns_of_interest = [
    'Proportion of households fuel poor (%)',
    'AVG_CURRENT_ENERGY_EFFICIENCY',
    'total_consumption_scaled',
    'Coronary heart disease',
    'COPD'
]

data_selected = data[columns_of_interest]

# Define ideal and negative ideal solutions
ideal_solution = {
    'Proportion of households fuel poor (%)': 0,
    'AVG_CURRENT_ENERGY_EFFICIENCY': 1,
    'total_consumption_scaled': 0,
    'Coronary heart disease': 0,
    'COPD': 0
}

negative_ideal_solution = {
    'Proportion of households fuel poor (%)': 1,
    'AVG_CURRENT_ENERGY_EFFICIENCY': 0,
    'total_consumption_scaled': 1,
    'Coronary heart disease': 1,
    'COPD': 1
}

# Normalize the data
print("Normalizing the data...")
data_normalized = (data_selected - data_selected.min()) / (data_selected.max() - data_selected.min())

# Determine weighting method
weighting_method = config.get("weighting_method", "equal_weight")

if weighting_method == "equal_weight":
    # Equal weights
    weights = np.ones(len(columns_of_interest)) / len(columns_of_interest)
elif weighting_method == "entropy_weight_method":
    # Entropy Weight Method (EWM)
    print("Calculating weights using Entropy Weight Method (EWM)...")
    epsilon = 1e-10  # Small constant to avoid log(0)
    p = data_normalized / data_normalized.sum(axis=0)
    entropy = -np.sum(p * np.log(p + epsilon), axis=0) / np.log(len(data_normalized))
    diversity = 1 - entropy
    weights = diversity / diversity.sum()
else:
    raise ValueError("Invalid weighting method specified in the configuration.")

# Apply weights to normalized data
data_weighted = data_normalized * weights

# Calculate distance to ideal and negative ideal solutions
distance_to_ideal = np.sqrt(((data_weighted - pd.Series(ideal_solution))**2).sum(axis=1))
distance_to_negative_ideal = np.sqrt(((data_weighted - pd.Series(negative_ideal_solution))**2).sum(axis=1))

# Calculate relative closeness
relative_closeness = distance_to_ideal / (distance_to_ideal + distance_to_negative_ideal)
normalized_relative_closeness = (relative_closeness - relative_closeness.min()) / (relative_closeness.max() - relative_closeness.min())

data['relative_closeness'] = relative_closeness
data['normalized_relative_closeness'] = normalized_relative_closeness

output_file_path = os.path.join(os.path.dirname(__file__), 'result', 'TOPSIS_result.csv')  # Replace with the desired output path
data.to_csv(output_file_path, index=False)
print(f"Results saved to: {output_file_path}")
print("Ideal Solution:", ideal_solution)
print("Negative Ideal Solution:", negative_ideal_solution)
