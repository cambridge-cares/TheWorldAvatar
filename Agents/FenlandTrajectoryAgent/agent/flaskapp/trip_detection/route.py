import os
import sys
import glob
import configparser
import logging

from flask import Blueprint, request, jsonify
import pandas as pd
import psycopg2

from trip_detection import detect_trips, DEFAULT_PARAMS

from agent.exposure_calculator.exposure_utilities import (
    connect_to_database,
    get_table_name_for_timeseries,
    fetch_trajectory_data_from_db,
    execute_query
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

CONFIG_FILE = os.path.join(os.path.dirname(__file__), "..", "exposure", "config.properties")
if not os.path.exists(CONFIG_FILE):
    logger.error(f"Config file not found at {CONFIG_FILE}. Please provide one.")
    sys.exit(1)

config = configparser.ConfigParser()
config.read(CONFIG_FILE)
try:
    ENV_DATA_ENDPOINT_URL = config.get("DEFAULT", "ENV_DATA_ENDPOINT_URL")
    TRAJECTORY_DB_HOST = config.get("DEFAULT", "TRAJECTORY_DB_HOST")
    TRAJECTORY_DB_PASSWORD = config.get("DEFAULT", "TRAJECTORY_DB_PASSWORD")
except (configparser.NoOptionError, configparser.NoSectionError) as e:
    logger.error(f"Configuration error: {e}")
    sys.exit(1)

DB_PORT = 5432
DB_USER = "postgres"
DB_NAME = "postgres"

trip_detection_bp = Blueprint('trip_detection_bp', __name__)

@trip_detection_bp.route('/trip_detection', methods=['POST'])
def trip_detection_route():
    """
    Points in segment, x_utm_sd, y_utm_sd, kernel, zone,
    norm_kernel, modfied_kernel, norm_modified_kernel, snap_to_hs,
    Trip index, Visit index, Gap
    """
    data = request.json
    trajectoryIRI = data.get("trajectoryIRI")
    if not trajectoryIRI:
        return jsonify({"error": "trajectoryIRI is required"}), 400

    try:
        conn = connect_to_database(TRAJECTORY_DB_HOST, DB_PORT, DB_USER, TRAJECTORY_DB_PASSWORD, DB_NAME)
        table_name = get_table_name_for_timeseries(conn, trajectoryIRI)
        trajectory_df = fetch_trajectory_data_from_db(trajectoryIRI)
    except Exception as e:
        return jsonify({"error": str(e)}), 500

    trajectory_df["Timestamp"] = trajectory_df["time"]

    columns_mapping = {
        "utc_date": "time",
        "lat": "LATITUDE",
        "lon": "LONGITUDE",
        "utm_n": "y_utm_sd",  
        "utm_e": "x_utm_sd"
    }
    for col in ["y_utm_sd", "x_utm_sd"]:
        if col in trajectory_df.columns:
            trajectory_df.drop(columns=[col], inplace=True)

    try:
        detected_gps, incidents_table, width, height, cell_size = detect_trips(
            trajectory_df.copy(), trajectoryIRI, columns_mapping,
            params=DEFAULT_PARAMS, interpolate_helper_func=None
        )
    except Exception as e:
        return jsonify({"error": f"Trip detection failed: {str(e)}"}), 500

    detected_gps["Points in segment"] = ""
    new_columns = [
        ("Points in segment", "TEXT"),
        ("x_utm_sd", "DOUBLE PRECISION"),
        ("y_utm_sd", "DOUBLE PRECISION"),
        ("kernel", "DOUBLE PRECISION"),
        ("zone", "INTEGER"),
        ("norm_kernel", "DOUBLE PRECISION"),
        ("modfied_kernel", "DOUBLE PRECISION"),
        ("norm_modified_kernel", "DOUBLE PRECISION"),
        ("snap_to_hs", "INTEGER"),
        ("Trip index", "INTEGER"),
        ("Visit index", "INTEGER"),
        ("Gap", "INTEGER")
    ]

    try:
        with conn.cursor() as cursor:
            for col_name, col_type in new_columns:
                alter_sql = f"""
                DO $$ BEGIN
                    IF NOT EXISTS (
                        SELECT FROM information_schema.columns
                        WHERE table_name = %s AND column_name = %s
                    ) THEN
                        EXECUTE 'ALTER TABLE "{table_name}" ADD COLUMN "{col_name}" {col_type}';
                    END IF;
                END $$;
                """
                cursor.execute(alter_sql, (table_name, col_name))
        conn.commit()
    except Exception as e:
        conn.rollback()
        return jsonify({"error": f"Failed to alter table: {str(e)}"}), 500

    try:
        with conn.cursor() as cursor:
            for idx, row in detected_gps.iterrows():
                update_sql = f'''
                UPDATE "{table_name}"
                SET "Points in segment" = %s,
                    "x_utm_sd" = %s,
                    "y_utm_sd" = %s,
                    "kernel" = %s,
                    "zone" = %s,
                    "norm_kernel" = %s,
                    "modfied_kernel" = %s,
                    "norm_modified_kernel" = %s,
                    "snap_to_hs" = %s,
                    "Trip index" = %s,
                    "Visit index" = %s,
                    "Gap" = %s
                WHERE "time" = %s;
                '''
                values = (
                    row.get("Points in segment"),
                    row.get("x_utm_sd"),
                    row.get("y_utm_sd"),
                    row.get("kernel"),
                    row.get("zone"),
                    row.get("norm_kernel"),
                    row.get("modfied_kernel"),
                    row.get("norm_modified_kernel"),
                    row.get("snap_to_hs"),
                    row.get("Trip index"),
                    row.get("Visit index"),
                    row.get("Gap"),
                    row.get("time")
                )
                cursor.execute(update_sql, values)
        conn.commit()
    except Exception as e:
        conn.rollback()
        return jsonify({"error": f"Failed to update table rows: {str(e)}"}), 500
    finally:
        conn.close()

    return jsonify({"message": "Trip detection applied and table updated successfully."}), 200
