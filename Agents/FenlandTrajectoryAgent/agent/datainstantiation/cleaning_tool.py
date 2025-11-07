import os
import re
import pandas as pd
import logging

logger = logging.getLogger(__name__)

def clean_gps_data(file_path, output_dir=None):
    """
    Logic for quick understanding:
      1. Only the following columns are cleaned: [UTC DATE, UTC TIME, LATITUDE, LONGITUDE, SPEED, HEADING, HEIGHT, DISTANCE].
         Other columns in the raw data from Fenland Study remain unchanged.
      2. For the LATITUDE and LONGITUDE columns:
         - First, check if the cell contains an isolated uppercase direction letter (N or S for latitude; E or W for longitude).
           If such a letter exists, use it to determine the sign. But for data in Fenland study, this pre-check is unnecessary. We just add this pre-check for general applicability consideration
         - Then, extract the numerical value from the cell using a regular expression that supports scientific notation.
         - If no direction letter is found in the cell, check the optional additional direction columns (N/S for latitude, E/W for longitude)
           to determine the direction. 
         - Adjust the numerical value's sign based on the determined direction. For data in fenland study, the direction indicated by direction columns (N/S for latitude, E/W for longitude) hold a dominant position to determine the direction
         Even if there is any single letters like N or S in cell of lat or lon, the function we defined here still use the infomation indicated by N/S and E/W)
      3. For the other columns, directly extract the numerical value using the regular expression (which also that supports scientific notation)
    """
    logger.info("Starting cleaning of GPS data from file: %s", file_path)
    try:
        df = pd.read_csv(file_path)
        logger.info("CSV file loaded successfully with shape: %s", df.shape)
    except Exception as e:
        logger.error("Error loading CSV file %s: %s", file_path, e, exc_info=True)
        raise e

    # Remove leading and trailing spaces from column names
    df.columns = [col.strip() for col in df.columns]
    logger.info("Normalized columns: %s", df.columns.tolist())

    def norm_col(col):
        return col.strip()

    # Define column names
    utc_date_column_name = "UTC DATE"
    utc_time_column_name = "UTC TIME"
    latitude_column_name  = "LATITUDE"
    longitude_column_name = "LONGITUDE"
    speed_column_name     = "SPEED"
    heading_column_name   = "HEADING"
    height_column_name    = "HEIGHT"
    distance_column_name  = "DISTANCE"
    ns_column_name        = "N/S"  # Optional additional direction column for latitude
    ew_column_name        = "E/W"  # Optional additional direction column for longitude

    columns_to_clean = [
        latitude_column_name,
        longitude_column_name,
        speed_column_name,
        heading_column_name,
        height_column_name,
        distance_column_name
    ]

    num_pattern = re.compile(r"[-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?")
    cleaned_df = df.copy()

    def extract_numeric(value):
        if isinstance(value, str):
            value = value.strip()
            matches = num_pattern.findall(value)
            if matches:
                try:
                    return float(matches[0])
                except ValueError:
                    return None
            return None
        return value

    # Updated process_coord function
    def process_coord(val, row, is_latitude):
        """
        As mentioned at the beginning of def clean_gps_data, the workflow for this process function using following steps:
          1. Check for an isolated uppercase direction letter in the cell. If it is N/S or E/W, set a provisional direction
          2. Extract the numeric value using a regular expression.
          3. Check the optional additional direction column (N/S or E/W) and override the direction if available. 
          4. Adjust the numeric value's sign based on the final direction.
        """
        # Step 1: Convert val to string.
        val_str = str(val)
        
        direction = None
        # Step 2: Check for a direction letter in the cell.
        if is_latitude:
            match = re.search(r'(?<![A-Z])(N|S)(?![A-Z])', val_str)
            if match:
                direction = match.group(1)
        else:
            match = re.search(r'(?<![A-Z])(E|W)(?![A-Z])', val_str)
            if match:
                direction = match.group(1)
        
        # Step 3: Extract the numeric value.
        matches = num_pattern.findall(val_str)
        numeric_val = None
        if matches:
            try:
                numeric_val = float(matches[0])
            except ValueError:
                numeric_val = None

        # Step 4: Check the optional additional direction column and override.
        if is_latitude and ns_column_name in row.index:
            ns_val = row[ns_column_name]
            if isinstance(ns_val, str):
                ns_val = ns_val.strip().upper()
                if ns_val in ['N', 'S']:
                    direction = ns_val
        elif (not is_latitude) and ew_column_name in row.index:
            ew_val = row[ew_column_name]
            if isinstance(ew_val, str):
                ew_val = ew_val.strip().upper()
                if ew_val in ['E', 'W']:
                    direction = ew_val

        # Step 5: Adjust the numeric value based on the final direction.
        if numeric_val is not None and direction is not None:
            if is_latitude:
                numeric_val = -abs(numeric_val) if direction == 'S' else abs(numeric_val)
            else:
                numeric_val = -abs(numeric_val) if direction == 'W' else abs(numeric_val)
        return numeric_val

    for col in columns_to_clean:
        if col in cleaned_df.columns:
            if col == latitude_column_name or col == longitude_column_name:
                cleaned_df[col] = cleaned_df.apply(
                    lambda row: process_coord(row[col], row, is_latitude=(col == latitude_column_name)),
                    axis=1
                )
            else:
                cleaned_df[col] = cleaned_df[col].apply(extract_numeric)

    cleaned_df.columns = [col.strip() for col in cleaned_df.columns]
    logger.info("Cleaning completed. Cleaned DataFrame shape: %s", cleaned_df.shape)

    temp_file = None
    if output_dir:
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
            logger.info("Created output directory: %s", output_dir)
        base_name = os.path.basename(file_path)
        name, ext = os.path.splitext(base_name)
        temp_file = os.path.join(output_dir, f"{name}_cleaned{ext}")
        try:
            cleaned_df.to_csv(temp_file, index=False)
            logger.info("Cleaned data saved to temporary file.")
        except Exception as e:
            logger.error("Error saving cleaned data to file %s: %s", temp_file, e, exc_info=True)
            raise e
    return cleaned_df, temp_file
