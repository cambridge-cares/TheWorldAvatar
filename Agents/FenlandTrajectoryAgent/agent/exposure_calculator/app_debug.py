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


app = Flask(__name__)
CORS(app)
logging.basicConfig(level=logging.INFO)

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

def execute_query(connection: psycopg2.extensions.connection, query: str, params: Optional[tuple] = None) -> Any:
    try:
        with connection.cursor() as cursor:
            logging.debug(f"Executing query: {query}, params={params}")
            cursor.execute(query, params)
            rows = cursor.fetchall()
            logging.info(f"Query returned {len(rows)} rows." if rows else "No results.")
            return rows
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
    domain_label, feature_type = get_domain_and_featuretype(env_data_iri, endpoint_url)

    if feature_type.upper() == "POINT":
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
        headers = {"Content-Type":"application/sparql-query","Accept":"application/json"}
        resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
        resp.raise_for_status()
        json_res = resp.json()
        data = []
        for b in json_res["results"]["bindings"]:
            row = {
                "ID": b.get("id", {}).get("value","N/A"),
                "Business Name": b.get("businessName", {}).get("value","N/A"),
                "Business Type": b.get("businessType", {}).get("value","N/A"),
                "Rating Time": b.get("ratingTime", {}).get("value","N/A"),
                "Rating Value": b.get("ratingValue", {}).get("value","N/A"),
                "Longitude": b.get("longitude", {}).get("value","N/A"),
                "Latitude": b.get("latitude", {}).get("value","N/A"),
            }
            data.append(row)
        df = pd.DataFrame(data)
        # Print the first and last 3 rows for POINT type
        logging.info(f"[POINT] Extracted DataFrame for {env_data_iri}:")
        logging.info(f"First 3 rows:\n{df.head(3)}")
        logging.info(f"Last 3 rows:\n{df.tail(3)}")
        return (df, domain_label)

    elif feature_type.upper() == "AREA":
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
        headers = {"Content-Type":"application/sparql-query","Accept":"application/json"}
        resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
        resp.raise_for_status()
        json_res = resp.json()
        data = []
        for b in json_res["results"]["bindings"]:
            row = {
                "Feature": b.get("feature",{}).get("value","N/A"),
                "Function": b.get("function",{}).get("value","N/A"),
                "Geometry": b.get("geometry",{}).get("value","N/A"),
            }
            data.append(row)
        df = pd.DataFrame(data)
        # Print the first and last 3 rows for AREA type
        logging.info(f"[AREA] Extracted DataFrame for {env_data_iri}:")
        logging.info(f"First 3 rows:\n{df.head(3)}")
        logging.info(f"Last 3 rows:\n{df.tail(3)}")
        return (df, domain_label)

    else:
        logging.warning(f"featureType={feature_type} unknown for {env_data_iri}, returning empty df.")
        return (pd.DataFrame([]), domain_label)

# ============ 5) The "exposure_calculation" with extended logging =============
def exposure_calculation(
    trajectory_df: pd.DataFrame,
    env_df: pd.DataFrame,
    domain_label: str,
    feature_type: str,
    exposure_radius: float,
    test_wkb: Optional[str] = None
) -> dict:
    """
    Improved exposure calculation with added test case for specific WKB geometry.
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

    # TEST CASE: Check specific WKB geometry
    if test_wkb:
        from shapely.geometry import shape
        import binascii
        import geopandas as gpd
        from shapely.wkb import loads as wkb_loads
        
        try:
            test_geom = wkb_loads(binascii.unhexlify(test_wkb), hex=True)
            logging.info(f"[TEST] Loaded test WKB geometry, type={test_geom.geom_type}, bounds={test_geom.bounds}")
            if buffer_geom.intersects(test_geom):
                inter = buffer_geom.intersection(test_geom)
                logging.info(f"[TEST] Test WKB intersects buffer, intersection area={inter.area:.2f}")
            else:
                dist = buffer_geom.distance(test_geom)
                logging.info(f"[TEST] Test WKB does not intersect buffer, minimum distance={dist:.2f}")
        except Exception as e:
            logging.error(f"[TEST] Failed to process test WKB, error={e}")

    # 4) Calculate exposure based on feature type
    if feature_type.upper() == "POINT":
        # Process point data
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

        count_val = len(visited_ids)
        logging.info(f"[POINT] Total count={count_val}, visited_ids={list(visited_ids)[:10]}...")
        return {
            "envFeatureName": domain_label,
            "type": "POINT",
            "count": count_val,
            "totalArea": None
        }

    elif feature_type.upper() == "AREA":
        # Process area data
        count_val = 0
        total_area = 0.0

        for idx, row in env_df.iterrows():
            wkb_hex = row.get("Geometry", "")
            if not wkb_hex or wkb_hex == "N/A":
                continue
            try:
                poly = wkb_loads(binascii.unhexlify(wkb_hex), hex=True)
                if buffer_geom.intersects(poly):
                    intersection = buffer_geom.intersection(poly)
                    count_val += 1
                    total_area += intersection.area
            except Exception as e:
                logging.debug(f"[AREA] Skipping row idx={idx} due to error: {e}")
                continue

        logging.info(f"[AREA] Final count={count_val}, total_area={total_area:.2f}")
        return {
            "envFeatureName": domain_label,
            "type": "AREA",
            "count": count_val,
            "totalArea": total_area
        }

    else:
        logging.warning(f"[exposure_calculation] Unknown feature_type={feature_type}, returning count=0.")
        return {
            "envFeatureName": domain_label,
            "type": feature_type,
            "count": 0,
            "totalArea": None
        }

# ============ 6) The main route ============
@app.route('/fenland-trajectory-agent/exposure', methods=['POST'])
def calculate_exposure():
    data = request.json
    trajectoryIRIs = data.get("trajectoryIRIs", [])
    exposure_radius = data.get("exposure_radius", 100)
    DataIRIs = data.get("DataIRIs", [])

    final_results = []
    for trajectoryIRI in trajectoryIRIs:
        try:
            trajectory_df = fetch_trajectory_data_from_db(trajectoryIRI)
        except Exception as e:
            return jsonify({"error": str(e)}), 500

        for env_data_iri in DataIRIs:
            try:
                env_df, domain_label = fetch_env_data(env_data_iri)
                # also get feature_type from the same function or re-call get_domain_and_featuretype
                _, feature_type = get_domain_and_featuretype(env_data_iri, ENV_DATA_ENDPOINT_URL)
            except Exception as e:
                return jsonify({"error": str(e)}), 500

            calc_res = exposure_calculation(
                trajectory_df=trajectory_df,
                env_df=env_df,
                domain_label=domain_label,
                feature_type=feature_type,
                exposure_radius=exposure_radius,
                test_wkb = "0106000020346C0000010000000103000000010000000A000000E17A142E5D8B20411F85EB512E280D41CDCCCCCC588B2041333333338F270D41295C8F427A8B20410000000082270D4133333333728B204167666666F2260D413E0AD7A36C8B20417B14AE47F9260D41333333B34A8B20417B14AE47ED250D419A9999190B8B20410000000048260D41295C8FC2268B2041EC51B81E79270D41F6285C0F418B204185EB51B85E270D41E17A142E5D8B20411F85EB512E280D41"
            )
            calc_res["trajectoryIRI"] = trajectoryIRI
            final_results.append(calc_res)

    return jsonify(final_results)


if __name__ == '__main__':
    app.run(port=3840)
