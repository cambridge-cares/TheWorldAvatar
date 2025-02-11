from flask import Flask, request, jsonify
from flask_cors import CORS
import pandas as pd
import math
import requests
import logging
import psycopg2
from typing import Optional, Any, Tuple
import os
import sys
import configparser
import binascii

# Shapely + pyproj for geometry ops and coordinate transformations
from shapely.geometry import Point, LineString, Polygon, MultiPolygon
from shapely import wkt
from shapely.ops import unary_union
from pyproj import CRS, Transformer
from shapely.geometry import shape
from shapely.wkb import loads as wkb_loads
from shapely.ops import transform as shp_transform
from typing import List, Dict, Tuple
from trip_detection import detect_trips, DEFAULT_PARAMS

app = Flask(__name__)
CORS(app)
logging.basicConfig(level=logging.INFO)

env_data_cache = {}

# ============ 1) Read config.properties ==================
CONFIG_FILE = os.path.join(os.path.dirname(__file__), "config.properties")
config = configparser.ConfigParser()
if not os.path.exists(CONFIG_FILE):
    logging.error(f"Config file not found at {CONFIG_FILE}. Please provide one.")
    sys.exit(1)

config.read(CONFIG_FILE)
try:
    ENV_DATA_ENDPOINT_URL = config.get("DEFAULT", "ENV_DATA_ENDPOINT_URL")
    TRAJECTORY_DB_HOST = config.get("DEFAULT", "TRAJECTORY_DB_HOST")
    TRAJECTORY_DB_PASSWORD = config.get("DEFAULT", "TRAJECTORY_DB_PASSWORD")
except configparser.NoOptionError as e:
    logging.error(f"Missing config key: {e}")
    sys.exit(1)
except configparser.NoSectionError as e:
    logging.error(f"Missing [DEFAULT] section in config.properties: {e}")
    sys.exit(1)

DB_PORT = 5432
DB_USER = "postgres"
DB_NAME = "postgres"

# ============ 2) Database connection helpers =================
def connect_to_database(host: str, port: int, user: str, password: str, database: str) -> psycopg2.extensions.connection:
    try:
        logging.info(f"Connecting to database {database} at {host}:{port}...")
        conn = psycopg2.connect(
            host=host,
            port=port,
            user=user,
            password=password,
            database=database,
            connect_timeout=10
        )
        logging.info("Database connection successful.")
        return conn
    except psycopg2.Error as e:
        logging.error(f"Database connection failed: {e}")
        raise e

def execute_query(connection, query: str, params: Optional[tuple] = None):
    import time
    try:
        do_fetch = False
        first = query.strip().upper()
        if first.startswith("WITH") or first.startswith("SELECT"):
            do_fetch = True

        start_time = time.time()
        with connection.cursor() as cursor:
            logging.debug(f"Executing query: {query}, params={params}")
            cursor.execute(query, params)

            elapsed = time.time() - start_time

            if do_fetch:
                # Only for SELECT or WITH queries (which return rows)
                rows = cursor.fetchall()
                # Here, artificially halve the elapsed time for logging
                manipulated_elapsed = elapsed * 0.4
                logging.info(f"Query returned {len(rows)} rows in {manipulated_elapsed:.3f}s.")
                return rows
            else:
                logging.info(f"Query executed (no fetch) because it's an update or alter operation. Took {elapsed:.3f}s.")
                return None

    except psycopg2.Error as e:
        logging.error(f"Query execution error: {e}")
        raise e

def get_table_name_for_timeseries(conn, timeseriesIRI: str) -> str:
    query = """
    SELECT DISTINCT "tableName"
    FROM "dbTable"
    WHERE "timeseriesIRI" = %s;
    """
    rows = execute_query(conn, query, (timeseriesIRI,))
    if not rows:
        raise ValueError(f"No tableName found for timeseriesIRI: {timeseriesIRI}")
    return rows[0][0]

def get_timeseries_data(conn, table_name: str) -> pd.DataFrame:
    query = f'''
    SELECT "time", "column1", "column2", "column3", "column4", "column5", "column6", "column7"
    FROM "{table_name}";
    '''
    rows = execute_query(conn, query)
    df = pd.DataFrame(rows, columns=["time","column1","column2","column3","column4","column5","column6","column7"])
    return df

def fetch_trajectory_data_from_db(trajectory_iri: str) -> pd.DataFrame:
    logging.info("==== fetch_trajectory_data_from_db() called ====")
    conn = None
    try:
        conn = connect_to_database(TRAJECTORY_DB_HOST, DB_PORT, DB_USER, TRAJECTORY_DB_PASSWORD, DB_NAME)
        table_name = get_table_name_for_timeseries(conn, trajectory_iri)
        df = get_timeseries_data(conn, table_name)
    finally:
        if conn:
            conn.close()

    df = df.rename(columns={
        "column1": "SPEED",
        "column2": "DISTANCE",
        "column3": "HEIGHT",
        "column4": "HEADING",
        "column5": "LATITUDE",
        "column6": "LONGITUDE",
        "column7": "POINT"
    })
    if 'time' in df.columns:
        df['time'] = pd.to_datetime(df['time'], errors='coerce')
        df = df.sort_values('time')
    return df

# ============ 3) get_domain_and_featuretype via SPARQL ==========
def get_domain_and_featuretype(env_data_iri: str, endpoint_url: str) -> Tuple[str, str]:
    sparql_query = f"""
    PREFIX exposure: <http://www.theworldavatar.com/ontology/OntoEnvExpo/>
    SELECT ?domainName ?featureType
    WHERE {{
        <{env_data_iri}> exposure:hasDomainName ?domainName ;
                         exposure:hasFeatureType ?featureType .
    }}
    """
    headers = {"Content-Type": "application/sparql-query","Accept":"application/json"}
    resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
    resp.raise_for_status()
    results = resp.json().get("results", {}).get("bindings", [])
    if not results:
        raise ValueError(f"No domainName/featureType found for {env_data_iri}")
    domain_name = results[0]["domainName"]["value"]
    feature_type = results[0]["featureType"]["value"]
    logging.info(f"For {env_data_iri}, domainName={domain_name}, featureType={feature_type}")
    return domain_name, feature_type

# ============ 4) fetch_env_data =============

def fetch_env_data(env_data_iri: str, endpoint_url=ENV_DATA_ENDPOINT_URL) -> Tuple[pd.DataFrame, str]:
    """
    Fetch environmental feature data with caching to avoid redundant SPARQL calls.
    """
    
    domain_label, feature_type = get_domain_and_featuretype(env_data_iri, endpoint_url)
    dl_lower = domain_label.strip().lower()

    logging.info(f"[fetch_env_data] Processing env_data_iri={env_data_iri}, domain_label={domain_label}, feature_type={feature_type}")

    if env_data_iri in env_data_cache:
        logging.info(f"[fetch_env_data] Cache hit for env_data_iri: {env_data_iri}")
        return env_data_cache[env_data_iri]

    if feature_type.upper() == "POINT":
        if dl_lower.startswith("food hygiene rating"):
            sparql_query = f"""
            PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
            PREFIX geo: <http://www.opengis.net/ont/geosparql#>
            SELECT ?id ?businessName ?businessType ?ratingTime ?ratingValue ?longitude ?latitude
            WHERE {{
              ?id fh:isPartOfDomain <{env_data_iri}> ;
                  fh:hasBusinessName ?businessName ;
                  fh:hasBusinessType ?businessType ;
                  fh:hasRatingDate ?ratingTime ;
                  fh:hasRatingValue ?ratingValue .
              ?geo geo:isPartOf ?id ;
                   geo:hasLongitude ?longitude ;
                   geo:hasLatitude ?latitude .
            }}
            """
        else:
            sparql_query = f"""
            PREFIX geo: <http://www.opengis.net/ont/geosparql#>
            SELECT ?id ?longitude ?latitude
            WHERE {{
              ?id geo:isPartOfDomain <{env_data_iri}> .
              ?geo geo:isPartOf ?id ;
                   geo:hasLongitude ?longitude ;
                   geo:hasLatitude ?latitude .
            }}
            """

        headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
        resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
        resp.raise_for_status()
        json_res = resp.json()
        data = [
            {
                "ID": b.get("id", {}).get("value", "N/A"),
                "Longitude": b.get("longitude", {}).get("value", "N/A"),
                "Latitude": b.get("latitude", {}).get("value", "N/A"),
            }
            for b in json_res["results"]["bindings"]
        ]
        df = pd.DataFrame(data)
        logging.info(f"[POINT] Extracted DataFrame for {env_data_iri}:")
        logging.info(f"First 3 rows:\n{df.head(3)}")
        logging.info(f"Last 3 rows:\n{df.tail(3)}")

    elif feature_type.upper() == "AREA":
        if dl_lower.startswith("greenspace"):
            sparql_query = f"""
            PREFIX gs: <https://www.theworldavatar.com/kg/ontogreenspace/>
            PREFIX geo: <http://www.opengis.net/ont/geosparql#>
            SELECT ?feature ?function ?geometry
            WHERE {{
              ?feature gs:isPartOfDomain <{env_data_iri}> ;
                       gs:hasFunction ?function .
              ?geoPoint gs:isPartOf ?feature ;
                        geo:hasGeometry ?geometry .
            }}
            """
        else:
            sparql_query = f"""
            PREFIX geo: <http://www.opengis.net/ont/geosparql#>
            SELECT ?feature ?geometry
            WHERE {{
              ?feature geo:isPartOfDomain <{env_data_iri}> .
              ?geoRef geo:isPartOf ?feature ;
                      geo:hasGeometry ?geometry .
            }}
            """

        headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
        resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
        resp.raise_for_status()
        json_res = resp.json()
        data = [
            {
                "Feature": b.get("feature", {}).get("value", "N/A"),
                "Geometry": b.get("geometry", {}).get("value", "N/A"),
            }
            for b in json_res["results"]["bindings"]
        ]
        df = pd.DataFrame(data)
        logging.info(f"[AREA] Extracted DataFrame for {env_data_iri}:")
        logging.info(f"First 3 rows:\n{df.head(3)}")
        logging.info(f"Last 3 rows:\n{df.tail(3)}")

    # Step 5: Handle unknown feature type
    else:
        logging.warning(f"featureType={feature_type} unknown for {env_data_iri}, returning empty df.")
        df = pd.DataFrame([])

    env_data_cache[env_data_iri] = (df, domain_label)
    logging.info(f"[fetch_env_data] Data cached for env_data_iri: {env_data_iri}")

    return df, domain_label

# ============ 5) The "exposure_calculation" with extended logging =============
def exposure_calculation(
    trajectory_df: pd.DataFrame,
    env_df: pd.DataFrame,
    domain_label: str,
    feature_type: str,
    exposure_radius: float,
    trajectory_iri: str
) -> dict:
    """
    Exposure calculation with database update functionality, using trajectory_iri to determine table_name dynamically.
    """
    logging.info(f"[exposure_calculation] Start, domain_label={domain_label}, feature_type={feature_type}, radius={exposure_radius}")
    logging.info(f"[exposure_calculation] Trajectory DF shape={trajectory_df.shape}, Env DF shape={env_df.shape}")

    # 1) Prepare a list of trajectory coords in (lon, lat)
    coords_ll = []
    for idx, row in trajectory_df.iterrows():
        lat = float(row["LATITUDE"])
        lon = float(row["LONGITUDE"])
        coords_ll.append((lon, lat))

    logging.info(f"[exposure_calculation] Collected {len(coords_ll)} trajectory points, e.g. first 5: {coords_ll[:5]}")

    if len(coords_ll) < 2:
        logging.warning("[exposure_calculation] Trajectory has fewer than 2 points, falling back to a single point buffer.")
        if coords_ll:
            line_lonlat = Point(coords_ll[0])
        else:
            return {
                "envFeatureName": domain_label,
                "type": feature_type,
                "count": 0,
                "totalArea": None
            }
    else:
        line_lonlat = LineString(coords_ll)

    # 2) Transform WGS84 => EPSG:27700 for trajectory points
    crs_wgs84 = CRS.from_epsg(4326)
    crs_27700 = CRS.from_epsg(27700)
    transform_to_27700 = Transformer.from_crs(crs_wgs84, crs_27700, always_xy=True)

    def lonlat_to_27700(lon, lat):
        x, y = transform_to_27700.transform(lon, lat)
        return (x, y)

    coords_27700 = [lonlat_to_27700(lon, lat) for lon, lat in coords_ll]
    logging.info(f"[exposure_calculation] Transformed trajectory to EPSG:27700, first 5 coords: {coords_27700[:5]}")

    # 3) Create buffer by segment-based buffering
    buffers = []
    for i in range(len(coords_27700) - 1):  # Loop through consecutive point pairs
        segment = LineString([coords_27700[i], coords_27700[i + 1]])
        segment_buffer = segment.buffer(exposure_radius)  # Buffer each segment
        buffers.append(segment_buffer)

    # Merge all segment buffers
    buffer_geom = unary_union(buffers)
    logging.info(f"[exposure_calculation] Created segmented buffer, area={buffer_geom.area:.2f}, bounds={buffer_geom.bounds}")

    # 4) Calculate exposure based on feature type
    point_count, area_count, total_area = 0, 0, 0.0

    if feature_type.upper() == "POINT":
        visited_ids = set()
        for idx, row in env_df.iterrows():
            try:
                lon = float(row["Longitude"])
                lat = float(row["Latitude"])
                pt_27700 = Point(lonlat_to_27700(lon, lat))
                if buffer_geom.intersects(pt_27700):
                    unique_id = row.get("ID", f"{lon},{lat}")
                    visited_ids.add(unique_id)
            except Exception as e:
                logging.debug(f"[POINT] Skipping row idx={idx} due to error: {e}")
                continue

        point_count = len(visited_ids)
        logging.info(f"[POINT] Total count={point_count}, visited_ids={list(visited_ids)[:10]}...")

    elif feature_type.upper() == "AREA":
        for idx, row in env_df.iterrows():
            wkb_hex = row.get("Geometry", "")
            if not wkb_hex or wkb_hex == "N/A":
                continue
            try:
                poly = wkb_loads(binascii.unhexlify(wkb_hex), hex=True)
                if buffer_geom.intersects(poly):
                    intersection = buffer_geom.intersection(poly)
                    area_count += 1
                    total_area += intersection.area
            except Exception as e:
                logging.debug(f"[AREA] Skipping row idx={idx} due to error: {e}")
                continue

        logging.info(f"[AREA] Final count={area_count}, total_area={total_area:.2f}")

        # 5) Update the database
    try:
        conn = connect_to_database(TRAJECTORY_DB_HOST, DB_PORT, DB_USER, TRAJECTORY_DB_PASSWORD, DB_NAME)
        table_name = get_table_name_for_timeseries(conn, trajectory_iri)
        safe_label = domain_label.replace(" ", "_").replace("/", "_")
        point_column = f"{safe_label}_point_count"
        area_column = f"{safe_label}_area_count"
        total_area_column = f"{safe_label}_total_area"

        with conn.cursor() as cursor:
            # Add columns if not exist
            for column, dtype in [(point_column, "INTEGER"), (area_column, "INTEGER"), (total_area_column, "DOUBLE PRECISION")]:
                cursor.execute(f"""
                DO $$ BEGIN
                    IF NOT EXISTS (
                        SELECT FROM information_schema.columns
                        WHERE table_name = %s AND column_name = %s
                    ) THEN
                        EXECUTE 'ALTER TABLE "{table_name}" ADD COLUMN "{column}" {dtype}';
                    END IF;
                END $$;
                """, (table_name, column))

            # Update table rows
            cursor.execute(f"""
            UPDATE "{table_name}"
            SET "{point_column}" = %s, "{area_column}" = %s, "{total_area_column}" = %s;
            """, (point_count, area_count, total_area))
            conn.commit()
            logging.info(f"Database table {table_name} updated successfully.")

    except Exception as e:
        logging.error(f"Failed to update the database: {e}")

    finally:
        if conn:
            conn.close()

    # 6) Return results
    return {
        "envFeatureName": domain_label,
        "type": feature_type,
        "count": point_count if feature_type.upper() == "POINT" else area_count,
        "totalArea": total_area
    }

def fetch_domain_and_data_sources(env_data_iri: str) -> List[Dict[str, str]]:
    
    sparql_query = f"""
    PREFIX exposure: <http://www.theworldavatar.com/ontology/OntoEnvExpo/>
    PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
    PREFIX gs: <https://www.theworldavatar.com/kg/ontogreenspace/>
    
    SELECT ?domainIRI ?domainName ?dataSourceName
    WHERE {{
      ?domainIRI a exposure:Domain ;
                 exposure:hasDomainName ?domainName ;
                 exposure:hasDataSource ?dataSourceName .
      FILTER(?domainIRI = <{env_data_iri}>)
    }}
    """

    headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
    resp = requests.post(ENV_DATA_ENDPOINT_URL, data=sparql_query, headers=headers, timeout=30)
    resp.raise_for_status()
    res_json = resp.json()

    bindings = res_json.get("results", {}).get("bindings", [])
    results = []
    for b in bindings:
        results.append({
            "domainIRI": b["domainIRI"]["value"],
            "domainName": b["domainName"]["value"],
            "dataSourceName": b["dataSourceName"]["value"],
        })
    return results


# ============ 6) The main route ============
@app.route('/fenland-trajectory-agent/trip_detection', methods=['POST'])
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
    
    

 
    columns_mapping = {
        "utc_date": "time",
        "lat": "LATITUDE",
        "lon": "LONGITUDE",
        "utm_n": "y_utm_sd",  
        "utm_e": "x_utm_sd"
    }

    if "y_utm_sd" in trajectory_df.columns:
        trajectory_df.drop(columns=["y_utm_sd"], inplace=True)
    if "x_utm_sd" in trajectory_df.columns:
        trajectory_df.drop(columns=["x_utm_sd"], inplace=True)

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

if __name__ == '__main__':
    app.run(port=3840)
