import rasterio
import csv
import numpy as np
from tqdm import tqdm

# Input GeoTIFF file
tiff_file = "C:/Users/printer_admin/Downloads/small_area.tif"
csv_file = "C:/Users/printer_admin/Downloads/satz_01.csv"

# Open the raster file
with rasterio.open(tiff_file) as src:
    band = src.read(1)  # Read first band (grayscale values)
    transform = src.transform  # Affine transformation
    nodata_value = src.nodata  # NoData value
    crs = src.crs  # Coordinate Reference System
    
    # Open CSV file for writing
    with open(csv_file, mode="w", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(["longitude", "latitude", "value"])  # CSV Header

        # Use progress bar
        progress_bar = tqdm(total=band.shape[0], desc="Processing", unit="rows")

        # Convert pixel indices to geographic coordinates
        for row in range(band.shape[0]):
            for col in range(band.shape[1]):
                x, y = transform * (col, row)  # Convert pixel to geographic coordinates
                value = band[row, col]

                # Preserve NoData value
                if value == nodata_value:
                    continue  # Skip NoData values (optional)

                writer.writerow([x, y, value])
            progress_bar.update(1)

transform_list = list(transform)
# Save metadata separately
metadata_file = "C:/Users/printer_admin/Downloads/tz_metadata.txt"
with open(metadata_file, "w") as meta_file:
    meta_file.write(f"CRS: {crs.to_wkt()}\n")
    meta_file.write(f"Transform: {transform_list}\n")
    meta_file.write(f"NoData: {nodata_value}\n")

print(f"✅ TIFF converted to CSV: {csv_file}")
print(f"📜 Metadata saved to: {metadata_file}")
