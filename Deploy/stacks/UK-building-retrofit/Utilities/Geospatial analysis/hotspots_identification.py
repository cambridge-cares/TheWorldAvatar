import pandas as pd
import numpy as np
import os
import json
from sklearn.preprocessing import MinMaxScaler
from libpysal.weights import KNN, DistanceBand, Kernel
from esda import Moran_Local
from scipy.spatial.distance import cdist
from tqdm import tqdm
import matplotlib.pyplot as plt
from SPARQLWrapper import SPARQLWrapper, JSON

script_dir = os.path.dirname(os.path.abspath(__file__))

# Load model configuration from model_selection.json
model_config_path = os.path.join(script_dir, "model_selection.json")
if os.path.exists(model_config_path):
    with open(model_config_path, 'r') as config_file:
        model_config = json.load(config_file)
else:
    # Default model configuration
    model_config = {
        "spatial_matrix_type": "knn",
        "k": 8,
        "output_file": os.path.join(script_dir, "result/output_results_with_hotspots.csv")
    }
    with open(model_config_path, 'w') as config_file:
        json.dump(model_config, config_file, indent=4)

# Add SPARQL endpoint and query from stack.json
stack_config_path = os.path.join(script_dir, "stack.json")
if os.path.exists(stack_config_path):
    with open(stack_config_path, 'r') as stack_file:
        stack_config = json.load(stack_file)
else:
    # Default stack configuration
    stack_config = {
        "sparql_endpoint": "http://localhost:3838/ontop/sparql",  
        "sparql_query_file": "ontop.sparql"
    }
    with open(stack_config_path, 'w') as stack_file:
        json.dump(stack_config, stack_file, indent=4)

# Read SPARQL endpoint URL and query file from stack.json
sparql_endpoint = stack_config.get("sparql_endpoint")
sparql_query_file = stack_config.get("sparql_query_file")
sparql_query_file = os.path.join(script_dir, sparql_query_file)

# Read SPARQL query from file
print("Reading SPARQL query from file...")
try:
    with open(sparql_query_file, 'r') as query_file:
        sparql_query = query_file.read()
except Exception as e:
    print(f"Error reading SPARQL query file: {e}")
    exit(1)

# Execute SPARQL query
print("Executing SPARQL query...")
sparql = SPARQLWrapper(sparql_endpoint)
sparql.setQuery(sparql_query)
sparql.setReturnFormat(JSON)

try:
    results = sparql.query().convert()
except Exception as e:
    print(f"Error executing SPARQL query: {e}")
    exit(1)

# Parse SPARQL results into DataFrame
print("Parsing SPARQL results...")
data = []
for result in results["results"]["bindings"]:
    lsoa_code = result["lsoa_code"]["value"]
    mean_gas = result["mean_gas_consumption"]["value"]
    mean_elec = result["mean_elec_consumption"]["value"]
    centroid_x = float(result["centroid_x"]["value"])
    centroid_y = float(result["centroid_y"]["value"])
    data.append({
        "LSOA Code": lsoa_code,
        "Mean \ngas consumption\n(kWh per meter)": mean_gas,
        "Mean \nelec. consumption\n(kWh per meter)": mean_elec,
        "x": centroid_x,
        "y": centroid_y
    })

df = pd.DataFrame(data)

# Data preprocessing
print("Preprocessing data...")
df['gas_consumption'] = pd.to_numeric(df['Mean \ngas consumption\n(kWh per meter)'].str.replace(',', ''), errors='coerce')
df['elec_consumption'] = pd.to_numeric(df['Mean \nelec. consumption\n(kWh per meter)'].str.replace(',', ''), errors='coerce')

# Fill missing values with nearest neighbors
def fill_missing_with_nearest(df, column):
    missing_mask = df[column].isna()
    if missing_mask.sum() > 0:
        coords = df[['x', 'y']].values
        distances = cdist(coords[missing_mask], coords[~missing_mask])
        nearest_indices = distances.argmin(axis=1)
        df.loc[missing_mask, column] = df.loc[~missing_mask, column].iloc[nearest_indices].values
    return df

print("Filling missing values...")
for column in tqdm(['gas_consumption', 'elec_consumption']):
    df = fill_missing_with_nearest(df, column)

df['total_consumption'] = df['gas_consumption'] + df['elec_consumption']

# Standardize total energy consumption
print("Standardizing total energy consumption...")
scaler = MinMaxScaler()
df['total_consumption_scaled'] = scaler.fit_transform(df[['total_consumption']])

# Plot data distribution
plt.hist(df['total_consumption_scaled'], bins=30)
plt.title('Distribution of Total Consumption Scaled')
plt.xlabel('Total Consumption Scaled')
plt.ylabel('Frequency')
plt.show()

# Create spatial weight matrix
print("Creating spatial weight matrix...")
coords = df[['x', 'y']].values
spatial_matrix_type = model_config.get("spatial_matrix_type", "knn")

if spatial_matrix_type == "knn":
    k = model_config.get("k", 8)
    w = KNN.from_array(coords, k=k)
elif spatial_matrix_type == "fixed_distance":
    threshold = model_config.get("distance_threshold", 1000)
    w = DistanceBand.from_array(coords, threshold=threshold, binary=True)
elif spatial_matrix_type == "gaussian":
    w = Kernel.from_array(coords, function='gaussian')
elif spatial_matrix_type == "kernel":
    w = Kernel.from_array(coords)
else:
    raise ValueError("Invalid spatial matrix type specified in the configuration.")

# Local Moran's I (LISA) calculation
print("Calculating Local Moran's I...")
df['z_total_consumption'] = (df['total_consumption'] - df['total_consumption'].mean()) / df['total_consumption'].std()
lisa = Moran_Local(df['z_total_consumption'], w)
df['lisa_statistic'] = lisa.Is
df['lisa_pvalue'] = lisa.p_sim

# significance test
df['significant'] = (df['lisa_pvalue'] < 0.05)

# Calculate median total consumption
median_total_consumption = df['total_consumption_scaled'].median()

# Determine hot spots
df['hotspot'] = ((df['lisa_statistic'] > 0) & df['significant'] &
                 (df['total_consumption_scaled'] > median_total_consumption)).astype(int)

print(df[['LSOA Code', 'lisa_statistic', 'lisa_pvalue', 'hotspot']].head(20))

# Save output to CSV for geenrating figures
output_df = df[['LSOA Code', 'x', 'y', 'total_consumption_scaled',
                'lisa_statistic', 'lisa_pvalue', 'significant', 'hotspot']]
output_file = model_config.get("output_file", os.path.join(script_dir, "result/output_results_with_hotspots.csv"))
output_df.to_csv(output_file, index=False)
print(f"Results saved to: {output_file}")

# Report isolated points 
islands = w.islands
if islands:
    print(f"Found {len(islands)} isolated points:")
    for island in islands:
        print(f"LSOA Code: {df.loc[island, 'LSOA Code']}")
else:
    print("No isolated points found.")

hotspot_counts = df['hotspot'].value_counts()
print(f"\nHotspot Statistics:")
print(f"Number of Hotspots (1): {hotspot_counts.get(1, 0)}")
print(f"Number of Non-Hotspots (0): {hotspot_counts.get(0, 0)}")
