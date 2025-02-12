import rasterio
import numpy as np
import pandas as pd
from tqdm import tqdm
import re

metadata_file = "C:/Users/printer_admin/Downloads/tz_metadata.txt"

# Load CSV
print("Loading CSV ... ")
df = pd.read_csv('C:/Users/printer_admin/Downloads/satz_01.csv')
print("Completed loading CSV")

# Ensure required columns exist
required_columns = {"longitude", "latitude", "value"}
if not required_columns.issubset(df.columns):
    raise ValueError(f"Missing required columns in CSV: {df.columns}")

# Read metadata
with open(metadata_file, "r") as meta_file:
    metadata = meta_file.readlines()

# Extract transform values safely
transform_line = metadata[1].strip()
transform_line = re.search(r"\[(.*?)\]", transform_line)
if transform_line:
    transform_line=transform_line.group(1)
    transform_values = [float(num) for num in transform_line.split(",")]
else:
    raise ValueError("Transform data not found in metadata file.")    
transform = rasterio.transform.Affine(*transform_values)

crs_wkt = metadata[0].split("CRS: ")[1].strip()
nodata_value = float(0)
print("Completed analysing metadata")

print("Processing pixel array ... ")
# Extract unique x and y coordinates
x_coords = np.sort(df["longitude"].unique())
y_coords = np.sort(df["latitude"].unique())[::-1]  # Reverse for raster order

# Create a 2D array for pixel values
pixel_values = np.full((len(y_coords), len(x_coords)), nodata_value, dtype=np.int16)
print("Completed processing pixel array")

# Create mappings for fast lookup
x_map = {v: i for i, v in enumerate(x_coords)}
y_map = {v: i for i, v in enumerate(y_coords)}

# Drop NaN rows (prevents index errors)
df = df.dropna(subset=["longitude", "latitude", "value"])

# Convert coordinates to indices
df["x_index"] = df["longitude"].map(x_map)
df["y_index"] = df["latitude"].map(y_map)

# Assign values to pixel array
pixel_values[df["y_index"], df["x_index"]] = df["value"].astype(np.int16)

# Define raster metadata
profile = {
    "driver": "GTiff",
    "dtype": "int16",
    "nodata": nodata_value,
    "width": len(x_coords),
    "height": len(y_coords),
    "count": 1,
    "crs": crs_wkt,
    "transform": transform
}

# Write TIFF
output_tif = 'C:/Users/printer_admin/Downloads/out_satz_01.tif'
with rasterio.open(output_tif, 'w', **profile) as dst:
    dst.write(pixel_values, 1)

print(f"Raster saved successfully at {output_tif}")
