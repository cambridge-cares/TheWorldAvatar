import os
import re
import pandas as pd
import logging

logger = logging.getLogger(__name__)

def clean_gps_data(file_path, output_dir=None):
    """
    This function reads a CSV file, extracts numeric values (removing any units), and returns the cleaned DataFrame.
    """
    logger.info("Starting cleaning of GPS data from file: %s", file_path)
    try:
        df = pd.read_csv(file_path)
        logger.info("CSV file loaded successfully with shape: %s", df.shape)
    except Exception as e:
        logger.error("Error loading CSV file %s: %s", file_path, e, exc_info=True)
        raise e

    def extract_numeric(value):
        if isinstance(value, str):
            matches = re.findall(r"[-+]?(?:\d*\.*\d+)", value)
            return float(matches[0]) if matches else None
        return value

    logger.info("Applying numeric extraction to all cells")
    cleaned_df = df.applymap(extract_numeric)
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
            logger.info("Cleaned data saved to temporary file: %s", temp_file)
        except Exception as e:
            logger.error("Error saving cleaned data to file %s: %s", temp_file, e, exc_info=True)
            raise e
    return cleaned_df, temp_file
