import pandas as pd
import numpy as np
import os
import json
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, BoundaryNorm

# Set global font for figures to Arial by default, check journal requirement and make decision here
plt.rcParams['font.family'] = 'Arial'

# Load configuration from JSON
config_path = os.path.join(os.path.dirname(__file__), "model_selection.json")
if os.path.exists(config_path):
    with open(config_path, 'r') as config_file:
        config = json.load(config_file)
else:
    config = {
        "spatial_matrix_type": "knn",  
        "k": 8,
        "output_file": os.path.join(os.path.dirname(__file__), "result/output_results_with_hotspots.csv")
    }
    with open(config_path, 'w') as config_file:
        json.dump(config, config_file, indent=4)

# Set default file path from config or fallback
file_path = config.get("output_file", os.path.join(os.path.dirname(__file__), "result/output_results_with_hotspots.csv"))

# Load the result dataset obtained from hotspots_identification.py
print(f"Loading data from: {file_path}")
df = pd.read_csv(file_path)

# determine the marker size for plotting purposes
if 'total_consumption' in df.columns:
    df['scaled_size'] = (df['total_consumption'] - df['total_consumption'].min()) / (df['total_consumption'].max() - df['total_consumption'].min()) * 100
else:
    df['scaled_size'] = 10  # Default value if 'total_consumption' is not present

# Custom color mapping and value boundaries can be changed here
cmap = ListedColormap(['#fee8c8', '#fdbb84', '#fc8d59', '#e34a33', '#b30000', '#67000d'])
bounds = [0, 10, 20, 30, 40, 50, 60]  # Setting an upper boundary value instead of np.inf
norm = BoundaryNorm(bounds, cmap.N)

# Plot module
fig, ax = plt.subplots(figsize=(10, 10))

# Hexbin plot based on original count values
hb = ax.hexbin(df['x'], df['y'], gridsize=80, cmap=cmap, norm=norm, mincnt=1)

cb = fig.colorbar(hb, ax=ax, ticks=[0, 10, 20, 30, 40, 50, 60])  # 60 is for values greater than 50
cb.set_ticklabels(['0-10', '10-20', '20-30', '30-40', '40-50', '>50'])
cb.set_label('Count')

ax.spines['top'].set_visible(True)
ax.spines['right'].set_visible(True)
ax.spines['top'].set_color('black')
ax.spines['right'].set_color('black')
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

ax.set_title('Hexbin Map of Energy Consumption Hotspots with Specified Count Ranges')
plt.show()
