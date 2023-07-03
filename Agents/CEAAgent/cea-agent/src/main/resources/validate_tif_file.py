import argparse
import os
import numpy as np
import rasterio

def validate_tif(source_tif, target_tif):
    with rasterio.open(source_tif) as src:
        data = src.read()
        profile = src.profile
        nodata_value = src.nodata

    valid_data = np.where(data != nodata_value)

    min_row, max_row = np.min(valid_data[1]), np.max(valid_data[1])
    min_col, max_col = np.min(valid_data[2]), np.max(valid_data[2])

    cropped_data = data[:, min_row:max_row+1, min_col:max_col+1]
    profile.update(width = cropped_data.shape[2], height = cropped_data.shape[1])

    with rasterio.open(target_tif, 'w', **profile) as dst:
        dst.write(cropped_data)


def main(argv):
    terrain_file = argv.file_location + os.sep + argv.file_name

    try:
        validate_tif(argv.original, terrain_file)
    except IOError:
        print("Error while processing file: " + argv.file_name)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument("original")
    parser.add_argument("file_location")
    parser.add_argument("file_name")

    args = parser.parse_args()
    main(args)