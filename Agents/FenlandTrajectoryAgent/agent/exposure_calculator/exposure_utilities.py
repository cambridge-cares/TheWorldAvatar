import os
import sys
import time
import logging
import configparser
import binascii
from typing import Optional, Tuple, List, Dict

import pandas as pd
import requests
import psycopg2

from shapely.geometry import Point, LineString
from shapely.ops import unary_union
from shapely.wkb import loads as wkb_loads
from pyproj import CRS, Transformer


class ExposureUtils:
    def __init__(self):
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(self.__class__.__name__)

        config_file = os.path.join(os.path.dirname(__file__), "config.properties")
        if not os.path.exists(config_file):
            self.logger.error(f"Config file not found at {config_file}. Please provide one.")
            sys.exit(1)
        self.config = configparser.ConfigParser()
        self.config.read(config_file)
        try:
            self.ENV_DATA_ENDPOINT_URL = self.config.get("DEFAULT", "ENV_DATA_ENDPOINT_URL")
            self.TRAJECTORY_DB_HOST = self.config.get("DEFAULT", "TRAJECTORY_DB_HOST")
            self.TRAJECTORY_DB_PASSWORD = self.config.get("DEFAULT", "TRAJECTORY_DB_PASSWORD")
        except (configparser.NoOptionError, configparser.NoSectionError) as e:
            self.logger.error(f"Config error: {e}")
            sys.exit(1)

        self.DB_PORT = 5432
        self.DB_USER = "postgres"
        self.DB_NAME = "postgres"

        self.env_data_cache = {}

        # Base directory for SQL and SPARQL templates
        self.template_dir = os.path.join(os.path.dirname(__file__), "count")

    def load_template(self, filename: str) -> str:
        filepath = os.path.join(self.template_dir, filename)
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()
        return content

    def connect_to_database(self, host: str, port: int, user: str, password: str, database: str) -> psycopg2.extensions.connection:
        try:
            self.logger.info(f"Connecting to database {database} at {host}:{port}...")
            conn = psycopg2.connect(
                host=host,
                port=port,
                user=user,
                password=password,
                database=database,
                connect_timeout=10
            )
            self.logger.info("Database connection successful.")
            return conn
        except psycopg2.Error as e:
            self.logger.error(f"Database connection failed: {e}")
            raise e

    def execute_query(self, connection, query: str, params: Optional[tuple] = None):
        try:
            do_fetch = False
            first = query.strip().upper()
            if first.startswith("WITH") or first.startswith("SELECT"):
                do_fetch = True

            start_time = time.time()
            with connection.cursor() as cursor:
                self.logger.debug(f"Executing query: {query}, params={params}")
                cursor.execute(query, params)
                elapsed = time.time() - start_time
                if do_fetch:
                    rows = cursor.fetchall()
                    self.logger.info(f"Query returned {len(rows)} rows in {elapsed:.3f}s.")
                    return rows
                else:
                    self.logger.info(f"Query executed (no fetch) in {elapsed:.3f}s.")
                    return None
        except psycopg2.Error as e:
            self.logger.error(f"Query execution error: {e}")
            raise e

    def get_table_name_for_timeseries(self, conn, timeseriesIRI: str) -> str:
        query_template = self.load_template("get_table_name_for_timeseries.sql")
        rows = self.execute_query(conn, query_template, (timeseriesIRI,))
        if not rows:
            raise ValueError(f"No tableName found for timeseriesIRI: {timeseriesIRI}")
        return rows[0][0]

    def get_timeseries_data(self, conn, table_name: str) -> pd.DataFrame:
        query_template = self.load_template("get_timeseries_data.sql")
        query = query_template.format(table_name=table_name)
        rows = self.execute_query(conn, query)
        df = pd.DataFrame(rows, columns=["time", "column1", "column2", "column3", "column4", "column5", "column6", "column7"])
        return df

    def fetch_trajectory_data_from_db(self, trajectory_iri: str) -> pd.DataFrame:
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
            df = df.sort_values('time')
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

    def fetch_env_data(self, env_data_iri: str, endpoint_url: Optional[str] = None) -> Tuple[pd.DataFrame, str]:
        if endpoint_url is None:
            endpoint_url = self.ENV_DATA_ENDPOINT_URL

        domain_label, feature_type = self.get_domain_and_featuretype(env_data_iri, endpoint_url)
        dl_lower = domain_label.strip().lower()

        self.logger.info(f"[fetch_env_data] Processing env_data_iri={env_data_iri}, domain_label={domain_label}, feature_type={feature_type}")

        if env_data_iri in self.env_data_cache:
            self.logger.info(f"[fetch_env_data] Cache hit for env_data_iri: {env_data_iri}")
            return self.env_data_cache[env_data_iri]

        if feature_type.upper() == "POINT":
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
            self.logger.info(f"[POINT] Extracted DataFrame for {env_data_iri}:")
            self.logger.info(f"First 3 rows:\n{df.head(3)}")
            self.logger.info(f"Last 3 rows:\n{df.tail(3)}")
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
            self.logger.info(f"[AREA] Extracted DataFrame for {env_data_iri}:")
            self.logger.info(f"First 3 rows:\n{df.head(3)}")
            self.logger.info(f"Last 3 rows:\n{df.tail(3)}")
        else:
            self.logger.warning(f"featureType={feature_type} unknown for {env_data_iri}, returning empty df.")
            df = pd.DataFrame([])

        self.env_data_cache[env_data_iri] = (df, domain_label)
        self.logger.info(f"[fetch_env_data] Data cached for env_data_iri: {env_data_iri}")
        return df, domain_label

    def exposure_calculation(
        self,
        trajectory_df: pd.DataFrame,
        env_df: pd.DataFrame,
        domain_label: str,
        feature_type: str,
        exposure_radius: float,
        trajectory_iri: str
    ) -> dict:
        self.logger.info(f"[exposure_calculation] Start, domain_label={domain_label}, feature_type={feature_type}, radius={exposure_radius}")
        self.logger.info(f"[exposure_calculation] Trajectory DF shape={trajectory_df.shape}, Env DF shape={env_df.shape}")

        coords_ll = []
        for _, row in trajectory_df.iterrows():
            lat = float(row["LATITUDE"])
            lon = float(row["LONGITUDE"])
            coords_ll.append((lon, lat))
        self.logger.info(f"[exposure_calculation] Collected {len(coords_ll)} trajectory points, e.g. first 5: {coords_ll[:5]}")

        if len(coords_ll) < 2:
            self.logger.warning("[exposure_calculation] Trajectory has fewer than 2 points, falling back to a single point buffer.")
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

        crs_wgs84 = CRS.from_epsg(4326)
        crs_27700 = CRS.from_epsg(27700)
        transformer = Transformer.from_crs(crs_wgs84, crs_27700, always_xy=True)
        def lonlat_to_27700(lon, lat):
            x, y = transformer.transform(lon, lat)
            return (x, y)
        coords_27700 = [lonlat_to_27700(lon, lat) for lon, lat in coords_ll]
        self.logger.info(f"[exposure_calculation] Transformed trajectory to EPSG:27700, first 5 coords: {coords_27700[:5]}")

        buffers = []
        for i in range(len(coords_27700) - 1):
            segment = LineString([coords_27700[i], coords_27700[i + 1]])
            segment_buffer = segment.buffer(exposure_radius)
            buffers.append(segment_buffer)
        buffer_geom = unary_union(buffers)
        self.logger.info(f"[exposure_calculation] Created segmented buffer, area={buffer_geom.area:.2f}, bounds={buffer_geom.bounds}")

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
                    self.logger.debug(f"[POINT] Skipping row idx={idx} due to error: {e}")
                    continue
            point_count = len(visited_ids)
            self.logger.info(f"[POINT] Total count={point_count}, visited_ids={list(visited_ids)[:10]}...")
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
                    self.logger.debug(f"[AREA] Skipping row idx={idx} due to error: {e}")
                    continue
            self.logger.info(f"[AREA] Final count={area_count}, total_area={total_area:.2f}")

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
            point_column = f"{safe_label}_point_count"
            area_column = f"{safe_label}_area_count"
            total_area_column = f"{safe_label}_total_area"
            with conn.cursor() as cursor:
                # UPDATE and ALTER queries remain hardcoded
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
                cursor.execute(f"""
                UPDATE "{table_name}"
                SET "{point_column}" = %s, "{area_column}" = %s, "{total_area_column}" = %s;
                """, (point_count, area_count, total_area))
                conn.commit()
                self.logger.info(f"Database table {table_name} updated successfully.")
        except Exception as e:
            self.logger.error(f"Failed to update the database: {e}")
        finally:
            if conn:
                conn.close()

        return {
            "envFeatureName": domain_label,
            "type": feature_type,
            "count": point_count if feature_type.upper() == "POINT" else area_count,
            "totalArea": total_area
        }

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
                    conn.commit()

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
