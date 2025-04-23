import os
import sys
import time
import logging
import configparser
import binascii
import re
from typing import Optional, Tuple, List, Dict
import math

import pandas as pd
import requests
import psycopg2

from shapely.geometry import Point, LineString, Polygon, MultiPolygon
from shapely import wkt
from pyproj import Geod
from shapely.ops import unary_union
from shapely.wkb import loads as wkb_loads
from pyproj import CRS, Transformer
from agent.utils.env_configs import DATABASE
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD
from contextlib import contextmanager
import numpy as np
from shapely.ops import transform
from shapely.prepared import prep
from shapely.strtree import STRtree
from agent.exposure_calculator.trip_detector.trip_detection import detect_trips, DEFAULT_PARAMS
from agent.exposure_calculator.trip_detector.utilities import *

class ExposureUtils:
    @staticmethod
    def geodesic_area(geom) -> float:
        """Calculate actual surface area in WGS84 coordinate system (unit: square meters)"""
        if geom is None or geom.is_empty:
            return 0.0
        geod = Geod(ellps='WGS84')
        if geom.geom_type == 'Polygon':
            lons, lats = zip(*list(geom.exterior.coords))
            area, _ = geod.polygon_area_perimeter(lons, lats)
            return abs(area)
        elif geom.geom_type == 'MultiPolygon':
            return sum(ExposureUtils.geodesic_area(poly) for poly in geom.geoms)
        else:
            return 0.0
        
    @staticmethod
    def buffer_point_geodesic(lon: float, lat: float, radius_m: float, n=60) -> Polygon:
        """
        Deprecated, no longer called internally. Signature kept for compatibility.
        """
        raise NotImplementedError("Use _make_fast_buffer instead")

    @staticmethod
    def buffer_linestring_geodesic(coords: list, radius_m: float, seg_step_m=20.0) -> Polygon:
        """
        Deprecated, no longer called internally. Signature kept for compatibility.
        """
        raise NotImplementedError("Use _make_fast_buffer instead")

    @staticmethod
    def create_geodesic_buffer(coords: list, radius_m: float):
        """
        Backward compatibility: Wrap as Geometry, then use _make_fast_buffer.
        """
        from shapely.geometry import Point, LineString
        eu = ExposureUtils()
        if not coords:
            return None
        if len(coords) == 1:
            geom = Point(coords[0])
        else:
            geom = LineString(coords)
        return eu._make_fast_buffer(geom, radius_m)

    def _make_fast_buffer(self, geom, radius_m: float):
        """
        Perform local equidistant projection (+buffer) + inverse projection on given WGS84 Geometry.
        Much faster than geodesic fwd version.
        """
        # 1) Calculate centroid for projection center
        centroid = geom.centroid
        # 2) Construct Azimuthal Equidistant projection
        aeqd = CRS.from_proj4(f"+proj=aeqd +lat_0={centroid.y} +lon_0={centroid.x} +units=m")
        to_aeqd = Transformer.from_crs(4326, aeqd, always_xy=True).transform
        to_wgs84 = Transformer.from_crs(aeqd, 4326, always_xy=True).transform
        # 3) Project, Euclidean buffer, inverse project
        proj = transform(to_aeqd, geom)
        buf_proj = proj.buffer(radius_m)
        return transform(to_wgs84, buf_proj)

    def __init__(self):
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(self.__class__.__name__)

        config_file = os.path.join(
            os.path.dirname(__file__),
            "..", "flaskapp", "exposure", "config.properties"
        )
        if not os.path.exists(config_file):
            self.logger.error(f"Config file not found at {config_file}")
            sys.exit(1)
        self.config = configparser.ConfigParser()
        self.config.read(config_file)
        try:
            self.ENV_DATA_ENDPOINT_URL = self.config.get("DEFAULT", "ENV_DATA_ENDPOINT_URL")
            self.TRAJECTORY_DB_HOST     = self.config.get("DEFAULT", "TRAJECTORY_DB_HOST")
            self.TRAJECTORY_DB_PASSWORD = self.config.get("DEFAULT", "TRAJECTORY_DB_PASSWORD")
        except (configparser.NoOptionError, configparser.NoSectionError) as e:
            self.logger.error(f"Config error: {e}")
            sys.exit(1)

        # Reuse geodetic and projection instances
        self.geod = Geod(ellps='WGS84')
        self.trans_27700_to_4326 = Transformer.from_crs(27700, 4326, always_xy=True)

        self.DB_PORT = DB_URL.split(':')[-1].split('/')[0]
        self.DB_USER = DB_USER
        self.DB_NAME = DATABASE

        self.env_data_cache = {}
        self.template_dir = os.path.join(os.path.dirname(__file__), "templates")

    def fetch_env_crs(self, env_data_iri: str, feature_type: str) -> str:
        """
        Returns a CRS URI for a geometry; falls back to 4326 for POINT if not found.
        """
        # Minimal change: directly load template
        template = self.load_template("get_crs_for_env_data.sparql")
        sparql = template.format(env_data_iri=env_data_iri)

        try:
            resp = requests.post(
                self.ENV_DATA_ENDPOINT_URL,
                data=sparql,
                headers={
                    "Content-Type": "application/sparql-query",
                    "Accept":       "application/json",
                },
                timeout=10
            )   
            resp.raise_for_status()
            bnd = resp.json().get("results", {}).get("bindings", [])
            if bnd:
                return bnd[0]["crs"]["value"]
        except Exception:
            # Ignore network/SPARQL issues
            pass

        # If not found or error, force fallback to WGS84 for POINT
        if feature_type.upper() == "POINT":
            return "http://www.opengis.net/def/crs/EPSG/0/4326"
        # AREA also falls back to 4326 (can be adjusted if needed)
        return "http://www.opengis.net/def/crs/EPSG/0/4326"

    def exposure_calculation(
        self,
        trajectory_df: pd.DataFrame,
        env_df: pd.DataFrame,
        env_data_iri: str,
        domain_label: str,
        feature_type: str,
        exposure_radius: float,
        trajectory_iri: str
    ) -> dict:
        """High-speed global geodesic buffer based on single AEQD projection + spatial index + true geodesic area calculation"""
        # 1) Extract trajectory lat/lon
        coords_ll = [
            (float(r["LONGITUDE"]), float(r["LATITUDE"]))
            for _, r in trajectory_df.iterrows()
            if r["LONGITUDE"] and r["LATITUDE"]
        ]
        if not coords_ll:
            return {"envFeatureName": domain_label, "type": feature_type, "count": 0, "totalArea": None}

        # 2) Construct trajectory geometry (WGS84)
        base_geom = Point(coords_ll[0]) if len(coords_ll) == 1 else LineString(coords_ll)

        # 3) Define Azimuthal Equidistant projection at trajectory centroid (units: meters)
        centroid = base_geom.centroid
        aeqd_crs = CRS.from_proj4(
            f"+proj=aeqd +lat_0={centroid.y} +lon_0={centroid.x} +units=m"
        )
        to_aeqd  = Transformer.from_crs(4326, aeqd_crs, always_xy=True).transform
        to_wgs84 = Transformer.from_crs(aeqd_crs, 4326, always_xy=True).transform

        # 4) Project to plane and buffer once
        proj_base = transform(to_aeqd, base_geom)
        buf_proj  = proj_base.buffer(exposure_radius)
        if buf_proj.is_empty:
            return {"envFeatureName": domain_label, "type": feature_type, "count": 0, "totalArea": None}

        # 5) Project all environmental features to the same plane CRS at once
        proj_env = []
        if feature_type.upper() == "POINT":
            for _, row in env_df.iterrows():
                try:
                    lon, lat = float(row["Longitude"]), float(row["Latitude"])
                    pt_proj = Point(to_aeqd(lon, lat))
                    proj_env.append((pt_proj, row.get("ID", f"{lon},{lat}")))
                except:
                    continue
        else:
            # AREA: First check source CRS, then project to AEQD
            crs_uri = self.fetch_env_crs(env_data_iri, feature_type)
            m = re.search(r"EPSG/0/(\d+)", crs_uri)
            src_epsg = int(m.group(1)) if m else 4326
            to_env_proj = Transformer.from_crs(
                f"EPSG:{src_epsg}", aeqd_crs, always_xy=True
            ).transform
            for _, row in env_df.iterrows():
                wkb_hex = row.get("Geometry", "")
                if not wkb_hex or wkb_hex == "N/A":
                    continue
                try:
                    poly_src  = wkb_loads(binascii.unhexlify(wkb_hex), hex=True)
                    poly_proj = transform(to_env_proj, poly_src)
                    proj_env.append((poly_proj, None))
                except:
                    continue

        # 6) Build spatial index and calculate plane intersections
        all_geoms = [g for g, _ in proj_env]
        tree = STRtree(all_geoms)
        idxs = tree.query(buf_proj)

        point_count = 0
        area_count  = 0
        total_area  = 0.0

        if feature_type.upper() == "POINT":
            visited = set()
            for i in idxs:
                pt_proj, uid = proj_env[i]
                if buf_proj.intersects(pt_proj):
                    if uid and uid not in visited:
                        self.logger.info(f"[EXPOSURE][{trajectory_iri}] POI matched: {uid}")
                    visited.add(uid)
            point_count = len(visited)
        else:
            for i in idxs:
                poly_proj, _ = proj_env[i]
                inter_proj = poly_proj.intersection(buf_proj)
                if inter_proj.is_empty:
                    continue
                # Project back to WGS84 and calculate true geodesic area
                inter_ll = transform(to_wgs84, inter_proj)
                total_area += ExposureUtils.geodesic_area(inter_ll)
                area_count  += 1

        # 7) Write back to database (ALTER TABLE + UPDATE + commit, maintain compatibility with old version)
        conn = None
        try:
            conn = self.connect_to_database(
                self.TRAJECTORY_DB_HOST,
                self.DB_PORT,
                self.DB_USER,
                self.TRAJECTORY_DB_PASSWORD,
                self.DB_NAME
            )
            table_name = self.get_table_name_for_timeseries(conn, trajectory_iri)
            safe_label = domain_label.replace(" ", "_").replace("/", "_")
            point_col = f"{safe_label}_point_count"
            area_col  = f"{safe_label}_area_count"
            ta_col    = f"{safe_label}_total_area"

            # 7a) Add columns
            for col, dtype in [
                (point_col, "INTEGER"),
                (area_col,  "INTEGER"),
                (ta_col,    "DOUBLE PRECISION")
            ]:
                alter_sql = f"""
                DO $$ BEGIN
                  IF NOT EXISTS (
                    SELECT FROM information_schema.columns
                    WHERE table_name = %s AND column_name = %s
                  ) THEN
                    EXECUTE 'ALTER TABLE "{table_name}" ADD COLUMN "{col}" {dtype}';
                  END IF;
                END $$;
                """
                self.execute_query(conn, alter_sql, (table_name, col))

            # 7b) Update values
            update_sql = f"""
            UPDATE "{table_name}"
            SET "{point_col}" = %s,
                "{area_col}"  = %s,
                "{ta_col}"    = %s;
            """
            self.execute_query(conn, update_sql, (
                point_count,
                area_count,
                total_area
            ))

            # 7c) Commit
            self.commit_if_psycopg2(conn)

        except Exception as e:
            self.logger.error(f"Failed to update the database: {e}")
        finally:
            if conn:
                conn.close()

        # 8) Return result
        result = {
            "envFeatureName": domain_label,
            "type":           feature_type,
            "count":          point_count if feature_type.upper()=="POINT" else area_count,
        }
        if feature_type.upper() == "AREA":
            result["totalArea"] = total_area

        return result

    def commit_if_psycopg2(self, conn):
        try:
            test_cursor = conn.cursor()  # Will throw an exception if using JDBC
            test_cursor.close()
            # Success means we're using psycopg2
            conn.commit()
            self.logger.debug("Committed (psycopg2).")
        except:
            self.logger.debug("Skipping commit (TSClient autoCommit).")
            
    def load_template(self, filename: str) -> str:
        filepath = os.path.join(self.template_dir, filename)
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()
        return content
    
    def connect_to_database(self, host: str, port: int, user: str, password: str, database: str) -> psycopg2.extensions.connection:
        """
        If TRAJECTORY_DB_HOST or TRAJECTORY_DB_PASSWORD is empty, consider external configuration incomplete,
        use internal connection method from TSClient; otherwise use psycopg2 to connect to external server.
        Note: Caller needs to manually call conn.close() after using the connection.
        """
        if not host.strip() or not password.strip():
            self.logger.info("External DB parameters not provided; using internal TSClient connection via setup_clients().")
            try:
                # Call setup_clients() from gps_client module, which has already successfully initialized TSClient (internal connection method)
                import agent.datainstantiation.gps_client as gdi
                kg_client, ts_client, double_class, point_class = gdi.setup_clients()
                # Use TSClient's connection method, return internal connection
                conn = ts_client.connection.getConnection()
                self.logger.info("Internal TSClient connection established successfully.")
                return conn
            except Exception as ex:
                self.logger.error(f"Internal TSClient connection failed: {ex}")
                raise ex
        else:
            self.logger.info(f"Connecting to database {database} at {host}:{port} using external parameters...")
            try:
                conn = psycopg2.connect(
                    host=host,
                    port=port,
                    user=user,
                    password=password,
                    database=database,
                    connect_timeout=10
                )
                self.logger.info("External DB connection established successfully.")
                return conn
            except psycopg2.Error as e:
                self.logger.error(f"External DB connection failed: {e}")
                raise e
            
    def execute_query(self, connection, query: str, params: Optional[tuple] = None):
        try:
            do_fetch = False
            first = query.strip().upper()
            if first.startswith("WITH") or first.startswith("SELECT"):
                do_fetch = True

            start_time = time.time()
            # Try getting a cursor via psycopg2
            try:
                cursor = connection.cursor()
                using_psycopg = True
            except Exception:
                # If .cursor() throws an exception (including Py4JException), we assume it's TSClient JDBC
                cursor = connection.createStatement()
                using_psycopg = False

            self.logger.debug(f"Executing query: {query}, params={params}")

            if using_psycopg:
                # psycopg2 mode
                cursor.execute(query, params)
            else:
                # JDBC mode
                if params:
                    # For each parameter: escape special characters and wrap in quotes
                    safe_params = []
                    for p in params:
                        if p is None:
                            # For NULL parameters, use the 'NULL' string
                            safe_params.append("NULL") 
                            continue
                        p_escaped = str(p).replace("'", "''")
                        safe_params.append(f"'{p_escaped}'")
                    query = query % tuple(safe_params)

                cursor.execute(query)

            elapsed = time.time() - start_time

            if do_fetch:
                try:
                    if using_psycopg:
                        rows = cursor.fetchall()
                    else:
                        rs = cursor.getResultSet()
                        rows = []
                        meta = rs.getMetaData()
                        ncols = meta.getColumnCount()
                        while rs.next():
                            row = tuple(rs.getString(i + 1) for i in range(ncols))
                            rows.append(row)
                    self.logger.info(f"Query returned {len(rows)} rows in {elapsed:.3f}s.")
                    return rows
                except Exception as fetch_e:
                    self.logger.error(f"Error fetching query results: {fetch_e}")
                    raise fetch_e
            else:
                self.logger.info(f"Query executed (no fetch) in {elapsed:.3f}s.")
                return None

        except Exception as e:
            self.logger.error(f"Query execution error: {e}")
            raise e

    def get_table_name_for_timeseries(self, conn, timeseriesIRI: str) -> str:
        try:
            query_template = self.load_template("get_table_name_for_timeseries.sql")
            rows = self.execute_query(conn, query_template, (timeseriesIRI,))
            if rows:
                return rows[0][0]
        except Exception as e:
            self.logger.warning(f"[Fallback] Primary get_table_name failed: {e}")

        # Try fallback
        fallback_template = self.load_template("get_table_name_for_timeseries_fallback.sql")
        rows = self.execute_query(conn, fallback_template, (timeseriesIRI,))
        if not rows:
            raise ValueError(f"No fallback tableName found for timeseriesIRI: {timeseriesIRI}")
        return rows[0][0]

    def get_timeseries_data(self, conn, table_name: str) -> pd.DataFrame:
        """
        Retrieve raw timeseries data from database.
        Automatically detects whether 'Trip index' column exists and selects the correct SQL template.
        Returns DataFrame with appropriate columns.
        """
        try:
            check_sql = self.load_template("check_trip_index_column.sql")
            result = self.execute_query(conn, check_sql, (table_name,))
            has_trip_index = bool(result)
        except Exception as e:
            self.logger.warning(f"[Trip index] Failed to check for 'Trip index' column: {e}")
            has_trip_index = False

        try:
            if has_trip_index:
                self.logger.info(f"[Trip index] Detected in table: {table_name}")
                sql_template_name = "get_timeseries_data_with_trip.sql"
            else:
                self.logger.info(f"[Trip index] Not found, using fallback SQL.")
                sql_template_name = "get_timeseries_data.sql"

            query_template = self.load_template(sql_template_name)
            query = query_template.format(table_name=table_name)
            rows = self.execute_query(conn, query)

            base_cols = ["time", "column1", "column2", "column3", "column4", "column5", "column6", "column7"]
            if has_trip_index:
                base_cols.append("Trip index")

            return pd.DataFrame(rows, columns=base_cols)

        except Exception as primary_exception:
            self.logger.warning(f"[Fallback] Primary SQL failed for {table_name}: {primary_exception}")
            
            # Run fallback version
            try:
                fallback_template = self.load_template("get_timeseries_data_fallback.sql")
                query = fallback_template.format(table_name=table_name)
                rows = self.execute_query(conn, query, (table_name,))
                return pd.DataFrame(rows, columns=["time", "LATITUDE", "LONGITUDE"])
            except Exception as fallback_exception:
                self.logger.error(f"[Fallback] Failed for {table_name}: {fallback_exception}")
                raise fallback_exception


    def fetch_trajectory_data_from_db(self, trajectory_iri: str) -> pd.DataFrame:
        """
        Fetch and process trajectory data from database.
        Handles basic column renaming and time formatting.
        """
        self.logger.info("==== fetch_trajectory_data_from_db() called ====")
        conn = None
        try:
            conn = self.connect_to_database(
                self.TRAJECTORY_DB_HOST,
                self.DB_PORT,
                self.DB_USER,
                self.TRAJECTORY_DB_PASSWORD,
                self.DB_NAME
            )
            table_name = self.get_table_name_for_timeseries(conn, trajectory_iri)
            df = self.get_timeseries_data(conn, table_name)
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
            df = df.sort_values('time').reset_index(drop=True)

        # Find earliest time, store to df['trajectory_start_time']
        if not df.empty:
            earliest_dt = df['time'].iloc[0]
            earliest_str = earliest_dt.strftime('%Y-%m-%dT%H:%M:%SZ')
            df['trajectory_start_time'] = earliest_str
            self.logger.info(f"Trajectory earliest time = {earliest_str}")
        else:
            self.logger.warning("Trajectory DataFrame is empty; no earliest time")

        return df

    def get_domain_and_featuretype(self, env_data_iri: str, endpoint_url: Optional[str] = None) -> Tuple[str, str]:
        if endpoint_url is None:
            endpoint_url = self.ENV_DATA_ENDPOINT_URL
        template = self.load_template("get_domain_and_featuretype.sparql")
        query = template.format(env_data_iri=env_data_iri)
        headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
        resp = requests.post(endpoint_url, data=query, headers=headers, timeout=30)
        resp.raise_for_status()
        results = resp.json().get("results", {}).get("bindings", [])
        if not results:
            raise ValueError(f"No domainName/featureType found for {env_data_iri}")
        domain_name = results[0]["domainName"]["value"]
        feature_type = results[0]["featureType"]["value"]
        self.logger.info(f"For {env_data_iri}, domainName={domain_name}, featureType={feature_type}")
        return domain_name, feature_type

    # ---- NEW OR MODIFIED ----
    # => Here we use absolute-time-filter.sparql
    def fetch_env_data(self, env_data_iri: str, endpoint_url: Optional[str] = None,
                       reference_time: Optional[str] = None) -> Tuple[pd.DataFrame, str]:
        if endpoint_url is None:
            endpoint_url = self.ENV_DATA_ENDPOINT_URL

        domain_label, feature_type = self.get_domain_and_featuretype(env_data_iri, endpoint_url)
        dl_lower = domain_label.strip().lower()

        self.logger.info(f"[fetch_env_data] Processing env_data_iri={env_data_iri}, domain_label={domain_label}, feature_type={feature_type}")

        if env_data_iri in self.env_data_cache and reference_time is None:
            self.logger.info(f"[fetch_env_data] Cache hit for env_data_iri: {env_data_iri}")
            return self.env_data_cache[env_data_iri]

        if feature_type.upper() == "POINT" and dl_lower.startswith("food hygiene rating") and reference_time is not None:
            self.logger.info(f"[fetch_env_data] Using absolute-time-filter for {env_data_iri} at {reference_time}")
            template_file = "absolute_time_filter.sparql"  # You might have named it absolute-time-filter.sparql
            sparql_query = self.load_template(template_file).format(given_time=reference_time)
            headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
            resp = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
            resp.raise_for_status()
            json_res = resp.json()
            data = []
            for b in json_res["results"]["bindings"]:
                lat = b.get("lat", {}).get("value")
                lon = b.get("long", {}).get("value")
                data.append({
                    "ID": b.get("be", {}).get("value", "N/A"),
                    "Latitude": lat if lat else "N/A",
                    "Longitude": lon if lon else "N/A",
                    "WKT": b.get("wkt", {}).get("value", "N/A"),
                })
            df = pd.DataFrame(data)
            self.logger.info(f"[POINT - TimeFilter] Extracted DataFrame with {len(df)} rows for {env_data_iri}")
            domain_label_final = domain_label  # Keep original domain label

        elif feature_type.upper() == "POINT":
            if dl_lower.startswith("food hygiene rating"):
                template_file = "get_food_hygiene_rating.sparql"
            else:
                template_file = "get_point_generic.sparql"
            sparql_query = self.load_template(template_file).format(env_data_iri=env_data_iri)
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
            self.logger.info(f"[POINT] Extracted DataFrame with {len(df)} rows for {env_data_iri}")
            domain_label_final = domain_label

        elif feature_type.upper() == "AREA":
            if dl_lower.startswith("greenspace"):
                template_file = "get_greenspace_area.sparql"
            else:
                template_file = "get_area_generic.sparql"
            sparql_query = self.load_template(template_file).format(env_data_iri=env_data_iri)
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
            self.logger.info(f"[AREA] Extracted DataFrame with {len(df)} rows for {env_data_iri}")
            domain_label_final = domain_label
        else:
            self.logger.warning(f"featureType={feature_type} unknown for {env_data_iri}, returning empty df.")
            df = pd.DataFrame([])
            domain_label_final = domain_label

        if reference_time is None:
            self.env_data_cache[env_data_iri] = (df, domain_label_final)

        return df, domain_label_final

    def fetch_domain_and_data_sources(self, env_data_iri: str) -> List[Dict[str, str]]:
        template = self.load_template("get_domain_and_data_sources.sparql")
        query = template.format(env_data_iri=env_data_iri)
        headers = {"Content-Type": "application/sparql-query", "Accept": "application/json"}
        resp = requests.post(self.ENV_DATA_ENDPOINT_URL, data=query, headers=headers, timeout=30)
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
    
    def create_standard_trajectory_table(self, table_name: str, conn):
        """
        Creates a standardized trajectory table only for internal use when importing external data.
        This SQL is defined inline rather than as a template since it's a utility function
        for internal data standardization rather than a query template.
        """
        create_sql = f'''
        CREATE TABLE IF NOT EXISTS "{table_name}" (
            "time" TIMESTAMP,
            "LATITUDE" DOUBLE PRECISION,
            "LONGITUDE" DOUBLE PRECISION
        );
        '''
        self.execute_query(conn, create_sql)
        self.logger.info(f"[Fallback] Created table {table_name} with standard structure.")


    def calculate_exposure_simplified_util(self, data: dict) -> List[Dict]:
        trajectoryIRIs = data.get("trajectoryIRIs", [])
        exposure_radius = data.get("exposure_radius", 100)
        dataIRIs = data.get("DataIRIs", [])
        final_results = []

        try:
            conn = self.connect_to_database(
                host=self.TRAJECTORY_DB_HOST,
                port=self.DB_PORT,
                user=self.DB_USER,
                password=self.TRAJECTORY_DB_PASSWORD,
                database=self.DB_NAME
            )
        except Exception as e:
            raise Exception(f"Failed to connect DB: {str(e)}")

        for trajectory_iri in trajectoryIRIs:
            try:
                table_name = self.get_table_name_for_timeseries(conn, trajectory_iri)
            except Exception as e:
                conn.close()
                raise Exception(f"Failed to get table_name for {trajectory_iri}: {str(e)}")

            for env_data_iri in dataIRIs:
                try:
                    domain_name, feature_type = self.get_domain_and_featuretype(env_data_iri, self.ENV_DATA_ENDPOINT_URL)
                    ds_rows = self.fetch_domain_and_data_sources(env_data_iri)
                    if not ds_rows:
                        final_results.append({
                            "trajectory_iri": trajectory_iri,
                            "env_data_iri": env_data_iri,
                            "error": "No dataSourceName found"
                        })
                        continue

                    dl_lower = domain_name.strip().lower()
                    row_count = 0

                    if feature_type.upper() == "AREA":
                        if dl_lower.startswith("greenspace"):
                            sql_template = self.load_template("simplified_get_area_greenspace.sql")
                            query = sql_template.format(exposure_radius=exposure_radius, table_name=table_name)
                            rows = self.execute_query(conn, query)
                            row_count = len(rows)
                        else:
                            union_parts = []
                            for item in ds_rows:
                                ds_name = item["dataSourceName"]
                                union_parts.append(f'SELECT wkb_geometry FROM public."{ds_name}"')
                            union_sql = "\nUNION ALL\n".join(union_parts)
                            sql_template = self.load_template("simplified_get_area_generic.sql")
                            query = sql_template.format(exposure_radius=exposure_radius, table_name=table_name, union_sql=union_sql)
                            rows = self.execute_query(conn, query)
                            row_count = len(rows)
                    elif feature_type.upper() == "POINT":
                        if dl_lower.startswith("food hygiene rating"):
                            union_parts = []
                            for item in ds_rows:
                                ds_name = item["dataSourceName"]
                                union_parts.append(f'''
                                    SELECT "Name", "Address", geom 
                                    FROM public."{ds_name}"
                                ''')
                            union_sql = "\nUNION ALL\n".join(union_parts)
                            sql_template = self.load_template("simplified_get_point_food_hygiene.sql")
                            query = sql_template.format(exposure_radius=exposure_radius, table_name=table_name, union_sql=union_sql)
                            point_rows = self.execute_query(conn, query)
                            row_count = sum(r[3] for r in point_rows)
                        else:
                            union_parts = []
                            for item in ds_rows:
                                ds_name = item["dataSourceName"]
                                union_parts.append(f'SELECT geom FROM public."{ds_name}"')
                            union_sql = "\nUNION ALL\n".join(union_parts)
                            sql_template = self.load_template("simplified_get_point_generic.sql")
                            query = sql_template.format(exposure_radius=exposure_radius, table_name=table_name, union_sql=union_sql)
                            rows = self.execute_query(conn, query)
                            row_count = len(rows)
                    else:
                        final_results.append({
                            "trajectory_iri": trajectory_iri,
                            "env_data_iri": env_data_iri,
                            "error": f"Unknown featureType={feature_type}"
                        })
                        continue

                    safe_column_name = f"{domain_name}_Count".replace(" ", "_").replace("/", "_")
                    alter_sql = f"""
                    DO $$ BEGIN
                        IF NOT EXISTS (
                            SELECT FROM information_schema.columns
                            WHERE table_name = %s AND column_name = %s
                        ) THEN
                            EXECUTE 'ALTER TABLE "{table_name}" ADD COLUMN "{safe_column_name}" INTEGER';
                        END IF;
                    END $$;
                    """
                    self.execute_query(conn, alter_sql, (table_name, safe_column_name))

                    update_sql = f'''
                    UPDATE "{table_name}"
                    SET "{safe_column_name}" = %s
                    '''
                    self.execute_query(conn, update_sql, (row_count,))
                    self.commit_if_psycopg2(conn)

                    final_results.append({
                        "trajectory_iri": trajectory_iri,
                        "env_data_iri": env_data_iri,
                        "domain_name": domain_name,
                        "feature_type": feature_type,
                        "updated_column": safe_column_name,
                        "row_count": row_count,
                        "table_name": table_name
                    })
                except Exception as e:
                    final_results.append({
                        "trajectory_iri": trajectory_iri,
                        "env_data_iri": env_data_iri,
                        "error": str(e)
                    })
        if conn:
            conn.close()
        return final_results

    def detect_trips_util(self, trajectory_iri: str) -> dict:
        """
        Performs trip detection on a trajectory and updates the database with results.
        Args:
            trajectory_iri: The IRI of the trajectory to analyze
        Returns:
            dict: Status of the operation
        """
        if not trajectory_iri:
            raise ValueError("trajectoryIRI is required")

        try:
            trajectory_df = self.fetch_trajectory_data_from_db(trajectory_iri)
            if 'time' not in trajectory_df.columns:
                raise ValueError("Required column 'time' not found in trajectory data")
            if trajectory_df['time'].isna().all():
                raise ValueError("No valid time data found in trajectory")

            trajectory_df['Timestamp'] = pd.to_datetime(trajectory_df['time'])
            self.logger.info(f"Created Timestamp column from time data. Sample values:")
            self.logger.info(f"First 3 values: {trajectory_df['Timestamp'].head(3).tolist()}")
            self.logger.info(f"Total rows: {len(trajectory_df)}, Valid Timestamp values: {trajectory_df['Timestamp'].notna().sum()}")

            if trajectory_df['Timestamp'].isna().all():
                raise ValueError("Failed to create valid Timestamp column from time data")

            for col in ['LATITUDE', 'LONGITUDE', 'SPEED', 'DISTANCE', 'HEIGHT', 'HEADING']:
                if col not in trajectory_df.columns:
                    raise ValueError(f"Required column '{col}' not found in trajectory data")
                if trajectory_df[col].isna().all():
                    raise ValueError(f"No valid data found in column '{col}'")
                trajectory_df[col] = pd.to_numeric(trajectory_df[col], errors='coerce')
                if col not in ['LATITUDE', 'LONGITUDE']:
                    trajectory_df[col] = trajectory_df[col].fillna(0)

            trajectory_df = trajectory_df.sort_values('Timestamp').reset_index(drop=True)
            trajectory_df['original_index'] = trajectory_df.index

        except Exception as e:
            self.logger.error(f"Failed to prepare trajectory data: {e}")
            raise

        columns_mapping = {
            "utc_date": "Timestamp",
            "lat": "LATITUDE",
            "lon": "LONGITUDE",
            "utm_n": "y_utm_sd",
            "utm_e": "x_utm_sd"
        }

        if "y_utm_sd" in trajectory_df.columns:
            trajectory_df.drop(columns=["y_utm_sd"], inplace=True)
        if "x_utm_sd" in trajectory_df.columns:
            trajectory_df.drop(columns=["x_utm_sd"], inplace=True)

        valid_coords = trajectory_df[trajectory_df['LATITUDE'].notna() & trajectory_df['LONGITUDE'].notna()]
        if valid_coords.empty:
            raise ValueError("No valid coordinates found in trajectory data")

        lat, lon = float(valid_coords.iloc[0]['LATITUDE']), float(valid_coords.iloc[0]['LONGITUDE'])
        utm_code = f"EPSG:{wgs_to_utm_code(lat, lon)}"
        self.logger.info(f"Using projection {utm_code} based on coordinates: lat={lat}, lon={lon}")

        try:
            self.logger.info("Starting trip detection with DataFrame shape: %s", trajectory_df.shape)
            detected_gps, incidents_table, width, height, cell_size = detect_trips(
                trajectory_df.copy(),
                trajectory_iri,
                columns_mapping,
                params=DEFAULT_PARAMS,
                interpolate_helper_func=None,
                code=utm_code
            )
            self.logger.info("Trip detection completed successfully")
            self.logger.info("Results - detected_gps shape: %s", detected_gps.shape if detected_gps is not None else "None")
            self.logger.info("Results - incidents_table shape: %s", incidents_table.shape if incidents_table is not None else "None")

            if detected_gps is not None:
                result_df = pd.DataFrame(index=trajectory_df.index)
                result_df['time'] = trajectory_df['time']

                trip_columns = [
                    ("Points in segment", ""),
                    ("x_utm_sd", None),
                    ("y_utm_sd", None),
                    ("kernel", 0.0),
                    ("zone", -1),
                    ("norm_kernel", 0.0),
                    ("modfied_kernel", 0.0),
                    ("norm_modified_kernel", 0.0),
                    ("snap_to_hs", 0),
                    ("Trip index", -1),
                    ("Visit index", -1),
                    ("Gap", 0)
                ]

                detected_gps = detected_gps.reset_index(drop=True)
                result_df = result_df.reset_index(drop=True)

                if len(detected_gps) != len(result_df):
                    self.logger.warning(f"Length mismatch: detected_gps={len(detected_gps)}, result_df={len(result_df)}")
                    if 'original_index' in detected_gps.columns:
                        detected_gps = detected_gps.set_index('original_index')
                        result_df = result_df.reindex(detected_gps.index)
                        detected_gps = detected_gps.reset_index()
                        result_df = result_df.reset_index()
                    else:
                        min_len = min(len(detected_gps), len(result_df))
                        detected_gps = detected_gps.iloc[:min_len]
                        result_df = result_df.iloc[:min_len]

                for col, default_value in trip_columns:
                    try:
                        result_df[col] = detected_gps[col].values if col in detected_gps.columns else pd.Series([default_value] * len(result_df))
                    except Exception as e:
                        self.logger.warning(f"Error copying column {col}: {e}")
                        result_df[col] = pd.Series([default_value] * len(result_df))

                detected_gps = result_df

        except Exception as e:
            self.logger.error(f"Trip detection failed: {e}")
            self.logger.error("DataFrame state at failure:")
            self.logger.error(f"Columns: {trajectory_df.columns.tolist()}")
            self.logger.error(f"Data types: {trajectory_df.dtypes.to_dict()}")
            raise

        conn = None
        try:
            conn = self.connect_to_database(
                self.TRAJECTORY_DB_HOST,
                self.DB_PORT,
                self.DB_USER,
                self.TRAJECTORY_DB_PASSWORD,
                self.DB_NAME
            )
            table_name = self.get_table_name_for_timeseries(conn, trajectory_iri)

            add_template = self.load_template("add_trip_detection_columns.sql")
            for col_name, col_type in [
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
                ("Gap", "INTEGER"),
                ("utm_zone", "TEXT")
            ]:
                alter_sql = add_template.format(table_name=table_name, column_name=col_name, col_type=col_type)
                self.execute_query(conn, alter_sql)

            update_sql_template = self.load_template("update_trip_detection_results.sql")
            update_sql = update_sql_template.format(table_name=table_name)

            for _, row in detected_gps.iterrows():
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
                self.execute_query(conn, update_sql, values)

            self.commit_if_psycopg2(conn)
            return {
                "message": "Trip detection applied and table updated successfully.",
                "projection_used": utm_code,
                "processed_rows": len(detected_gps)
            }

        except Exception as e:
            self.logger.error(f"Database operation failed: {e}")
            raise
        finally:
            if conn:
                conn.close()


