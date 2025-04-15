"""
This script demonstrates an exposure calculation pipeline with PROJECTION:
1) Read a trajectory from CSV (time in ms, lat/lon) and reproject to EPSG:27700
2) Slice the trajectory by a given time interval (in seconds)
3) For each slice, create a buffer geometry (in meters, thanks to EPSG:27700)
4) Construct a SPARQL query to Blazegraph, which in turn federates to Ontop
5) Filter the returned WKT by intersection with the buffer
6) Collect unique business establishments as exposure results

Now the 100m buffer is truly 100 meters, because we do a reproject.

Additionally, we reproject each returned geometry from EPSG:4326 to EPSG:27700
before intersection, ensuring coordinate compatibility.

Author: Jiying Chen(jc2341@cam.ac.uk) March 2025
"""

import csv
import time
import datetime
import requests
import json
import pandas as pd
from shapely.geometry import Point, LineString
from shapely import wkt
from shapely.ops import transform
from pyproj import Transformer

##############################
# 1) Reprojection Setup
##############################
# If your data is in WGS84 (EPSG:4326), and you want meters in the UK,
# you can reproject to EPSG:27700.

project_4326_to_27700 = Transformer.from_crs("EPSG:4326", "EPSG:27700", always_xy=True).transform


def read_trajectory(csv_path):
    """
    Read trajectory CSV with columns:
    - 'time': Unix ms timestamp
    - 'LATITUDE', 'LONGITUDE'
    Then reproject each point from lat/lon in EPSG:4326 to EPSG:27700.
    Return a list of (datetime, (x_meters, y_meters)) sorted by time.
    """
    points = []
    df = pd.read_csv(csv_path)
    for _, row in df.iterrows():
        ts_ms = int(row['time'])
        ts = datetime.datetime.utcfromtimestamp(ts_ms / 1000)
        lat = float(row['LATITUDE'])
        lon = float(row['LONGITUDE'])
        # Reproject to EPSG:27700
        pt_4326 = Point(lon, lat)
        pt_27700 = transform(project_4326_to_27700, pt_4326)

        # Now we store x,y from EPSG:27700 which is in meters
        points.append((ts, (pt_27700.x, pt_27700.y)))
    points.sort(key=lambda x: x[0])
    print(f"[INFO] Loaded {len(points)} trajectory points from {csv_path}, reprojected to EPSG:27700.")
    return points


def slice_trajectory(points, delta_seconds):
    """
    Slice the trajectory by time intervals (delta_seconds).
    Each slice is a list of (timestamp, (x_meters, y_meters)).
    The mid_time of that slice is used for SPARQL query.
    """
    slices = []
    if not points:
        return slices

    slice_start = points[0][0]
    current_slice = []

    for ts, coord in points:
        if (ts - slice_start).total_seconds() <= delta_seconds:
            current_slice.append((ts, coord))
        else:
            if current_slice:
                mid_time = current_slice[len(current_slice)//2][0]
                slices.append((current_slice, mid_time))
                print(f"[DEBUG] Created slice at {mid_time.isoformat()} with {len(current_slice)} points.")
            slice_start = ts
            current_slice = [(ts, coord)]

    # final flush
    if current_slice:
        mid_time = current_slice[len(current_slice)//2][0]
        slices.append((current_slice, mid_time))
        print(f"[DEBUG] Created final slice at {mid_time.isoformat()} with {len(current_slice)} points.")

    return slices


def compute_buffer(slice_points, buffer_radius):
    """
    Now that slice_points are in meters, buffer_radius is truly in meters.
    If there's only 1 point, create a Point buffer; otherwise a LineString.
    """
    coords = [coord for (ts, coord) in slice_points]
    if len(coords) == 1:
        geom = Point(coords[0])
    else:
        geom = LineString(coords)
    return geom.buffer(buffer_radius)


def query_sparql(given_time, blazegraph_endpoint):
    """
    Construct & POST a SPARQL query to blazegraph_endpoint.
    The SPARQL internally references Ontop (via SERVICE <http://host.docker.internal:5007/ontop/ui/sparql>).

    Double-escape the regex.
    """

    pattern_python = r'(?:-[A-Za-z]{2})?\s+([0-9]{2}:[0-9]{2})-([0-9]{2}:[0-9]{2}).*'
    # double-escape
    pattern_for_sparql = pattern_python.replace('\\','\\\\')

    weekday_map = {"Mon": "Mo", "Tue": "Tu", "Wed": "We", "Thu": "Th", "Fri": "Fr", "Sat": "Sa", "Sun": "Su"}
    weekday_str = weekday_map[given_time.strftime("%a")]
    time_str = given_time.strftime("%H:%M")
    givenTime_str = given_time.strftime("%Y-%m-%dT%H:%M:%SZ")

    query = f"""
PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
PREFIX ies4: <http://ies.data.gov.uk/ontology/ies4#>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX wgs: <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX osmkey: <https://www.openstreetmap.org/wiki/Key:>

SELECT DISTINCT ?be ?geometry ?wkt ?openingHours
WHERE {{
  BIND("{weekday_str}" AS ?weekday)
  BIND("{time_str}" AS ?timeStr)

  BIND(CONCAT(".*", ?weekday, "{pattern_for_sparql}") AS ?pattern)

  SERVICE <http://host.docker.internal:5007/ontop/ui/sparql> {{
    SELECT DISTINCT ?be ?geometry ?wkt
    WHERE {{
      BIND("{givenTime_str}"^^xsd:dateTimeStamp AS ?givenTime)
      BIND(SUBSTR(STR(?givenTime), 1, 4) AS ?timeYear)
      ?be a fh:BusinessEstablishment ; geo:hasGeometry ?geometry .
      FILTER(CONTAINS(STR(?geometry), CONCAT("/", ?timeYear)))
      ?geometry geo:asWKT ?wkt .

      ?startEvent ies4:isStartOf ?be ; ies4:inPeriod ?startPeriod .
      ?startPeriod ies4:iso8601PeriodRepresentation ?startTime .
      ?endEvent ies4:isEndOf ?be ; ies4:inPeriod ?endPeriod .
      ?endPeriod ies4:iso8601PeriodRepresentation ?endTime .
      FILTER(?startTime <= ?givenTime && ?endTime >= ?givenTime)
    }}
  }}

  OPTIONAL {{ ?be osmkey:opening_hours ?openingHours . }}

  FILTER (
    (!BOUND(?openingHours)) ||
    (
      regex(?openingHours, ?pattern) &&
      (?timeStr >= REPLACE(?openingHours, ?pattern, "$1")) &&
      (?timeStr <= REPLACE(?openingHours, ?pattern, "$2"))
    )
  )
}}
"""

    headers = {"Accept": "application/sparql-results+json"}
    response = requests.post(blazegraph_endpoint, data={"query": query}, headers=headers)
    if response.status_code == 200:
        return response.json()
    else:
        print("[ERROR] SPARQL query failed:", response.status_code)
        print("[ERROR] SPARQL query text:\n" + query)
        print(response.text)
        return None


def filter_by_buffer(results_json, buffer_poly):
    """
    Inspect each ?wkt, parse geometry, reproject from EPSG:4326 to EPSG:27700,
    then check intersection with buffer_poly.
    Return a set of ?be URIs.
    """
    valid_be = set()
    if results_json is None:
        return valid_be

    bindings = results_json.get("results", {}).get("bindings", [])

    # We'll create a local reproject function for geometry from 4326 to 27700
    reproject_4326_to_27700_local = Transformer.from_crs("EPSG:4326", "EPSG:27700", always_xy=True).transform

    for binding in bindings:
        wkt_str = binding.get("wkt", {}).get("value")
        if wkt_str:
            try:
                geom_4326 = wkt.loads(wkt_str)
                # Reproject the DB geometry from EPSG:4326 to EPSG:27700
                geom_27700 = transform(reproject_4326_to_27700_local, geom_4326)

                if geom_27700.intersects(buffer_poly):
                    be_uri = binding["be"]["value"]
                    valid_be.add(be_uri)
            except Exception as e:
                print("[WARN] WKT parsing error:", e)

    return valid_be


def main_experiment(csv_file, delta_t_seconds, buffer_radius, blazegraph_endpoint):

    points = read_trajectory(csv_file)
    slices = slice_trajectory(points, delta_t_seconds)

    union_exposures = set()
    total_query_time = 0.0

    for i, (slice_data, mid_time) in enumerate(slices):
        print(f"\n[INFO] Processing slice {i+1}/{len(slices)} at {mid_time.isoformat()}")
        buffer_poly = compute_buffer(slice_data, buffer_radius)

        start_query = time.time()
        results_json = query_sparql(mid_time, blazegraph_endpoint)
        elapsed = time.time() - start_query
        total_query_time += elapsed

        if results_json:
            cnt = len(results_json["results"]["bindings"])
            print(f"[DEBUG] SPARQL returned {cnt} records in {elapsed:.2f} s.")
        else:
            print("[DEBUG] No SPARQL results. Possibly 0 records or query error.")

        valid_be = filter_by_buffer(results_json, buffer_poly)
        print(f"[DEBUG] Found {len(valid_be)} establishments intersecting with buffer.")
        union_exposures.update(valid_be)

    print(f"\n[RESULT] Final exposure count: {len(union_exposures)}")
    print(f"[RESULT] Total SPARQL query time: {total_query_time:.2f} seconds.")

    return union_exposures, total_query_time