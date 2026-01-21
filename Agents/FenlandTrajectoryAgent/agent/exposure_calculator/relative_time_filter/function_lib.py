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
from shapely.ops import transform, nearest_points
from pyproj import Transformer

##############################
# Coordinate Projection Settings
##############################
# If the raw data is in WGS84 (EPSG:4326) and you need distances in meters for the UK,
# then project the points to EPSG:27700.
project_4326_to_27700 = Transformer.from_crs("EPSG:4326", "EPSG:27700", always_xy=True).transform
project_27700_to_4326 = Transformer.from_crs("EPSG:27700", "EPSG:4326", always_xy=True).transform

def read_trajectory(csv_path):
    """
    Reads trajectory data from a CSV file. The CSV must contain the following columns:
      - 'time': Unix timestamp in milliseconds
      - 'LATITUDE' and 'LONGITUDE'
    Converts each point to a datetime and projects it to EPSG:27700 (meters).
    Returns a time-sorted list, where each element is (timestamp, (x, y)).
    """
    points = []
    df = pd.read_csv(csv_path)
    for _, row in df.iterrows():
        ts_ms = int(row['time'])
        ts = datetime.datetime.utcfromtimestamp(ts_ms / 1000)
        lat = float(row['LATITUDE'])
        lon = float(row['LONGITUDE'])
        pt_4326 = Point(lon, lat)
        pt_27700 = transform(project_4326_to_27700, pt_4326)
        points.append((ts, (pt_27700.x, pt_27700.y)))
    points.sort(key=lambda x: x[0])
    print(f"[INFO] Loaded {len(points)} trajectory points from {csv_path} and projected them to EPSG:27700.")
    return points

def get_interpolated_point(points, target_time):
    """
    For a given sorted list of trajectory points (timestamp, (x, y)) and a target time,
    returns the point if the target time exactly exists in the sample;
    otherwise, computes the coordinate for the target time using linear interpolation.
    """
    if target_time <= points[0][0]:
        return points[0]
    if target_time >= points[-1][0]:
        return points[-1]
    for i in range(len(points) - 1):
        t1, coord1 = points[i]
        t2, coord2 = points[i + 1]
        if t1 <= target_time <= t2:
            if target_time == t1:
                return points[i]
            if target_time == t2:
                return points[i + 1]
            total_seconds = (t2 - t1).total_seconds()
            elapsed = (target_time - t1).total_seconds()
            fraction = elapsed / total_seconds
            x = coord1[0] + fraction * (coord2[0] - coord1[0])
            y = coord1[1] + fraction * (coord2[1] - coord1[1])
            return (target_time, (x, y))
    return points[-1]

def slice_trajectory_with_interpolation(points, delta_seconds):
    """
    Splits the trajectory into segments based on the specified time interval (in seconds).
    For each segment, if the start or end time is not exactly a sample, calculate the corresponding point using interpolation.
    Returns a list where each element is (slice_points, mid_time), with slice_points including the interpolated start point,
    all in-between sample points, and the interpolated end point; mid_time represents the midpoint time of the segment.
    """
    slices = []
    if not points:
        return slices
    current_start = points[0][0]
    last_time = points[-1][0]
    while current_start < last_time:
        current_end = current_start + datetime.timedelta(seconds=delta_seconds)
        if current_end > last_time:
            current_end = last_time
        slice_points = []
        start_point = get_interpolated_point(points, current_start)
        slice_points.append(start_point)
        for pt in points:
            if pt[0] > current_start and pt[0] < current_end:
                slice_points.append(pt)
        end_point = get_interpolated_point(points, current_end)
        slice_points.append(end_point)
        duration = current_end - current_start
        mid_time = current_start + duration / 2
        print(f"[DEBUG] Created slice: start {current_start.isoformat()}, end {current_end.isoformat()}, midpoint {mid_time.isoformat()}, containing {len(slice_points)} points")
        slices.append((slice_points, mid_time))
        current_start = current_end
    return slices

def create_full_buffer(points, buffer_radius):
    """
    Generates a buffer for the entire trajectory.
    If the trajectory has only one point, apply the buffer directly to that point; otherwise, construct a LineString and buffer it.
    """
    coords = [coord for (_, coord) in points]
    if len(coords) == 1:
        geom = Point(coords[0])
    else:
        geom = LineString(coords)
    return geom.buffer(buffer_radius)

def compute_buffer(slice_points, buffer_radius):
    """
    Generates a buffer based on all points in a slice (projected to EPSG:27700).
    If the slice has only one point, return a circular buffer; otherwise, generate a buffer from the LineString.
    """
    coords = [coord for (_, coord) in slice_points]
    if len(coords) == 1:
        geom = Point(coords[0])
    else:
        geom = LineString(coords)
    return geom.buffer(buffer_radius)

def format_timestamp(dt):
    """
    Formats a datetime object into a literal string allowed by xsd:dateTimeStamp.
    If microseconds are present, truncate to milliseconds and append "Z" to indicate UTC.
    Format examples: YYYY-MM-DDTHH:MM:SS.mmmZ or YYYY-MM-DDTHH:MM:SSZ.
    """
    if dt.microsecond == 0:
        return dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    else:
        ms = int(dt.microsecond / 1000)
        return dt.strftime("%Y-%m-%dT%H:%M:%S") + f".{ms:03d}Z"

###############################################################################
# SPARQL Query Construction (Dynamic Time Binding and Regex)
###############################################################################
def query_sparql(dt_start, dt_end, blazegraph_endpoint):
    """
    Dynamically constructs a SPARQL query using the start and end times of a slice.
    Extracts the weekday and time string (HH:MM) from dt_start and dt_end to build a regular expression filter for opening_hours,
    while binding the slice's start and end times as xsd:dateTimeStamp.
    
    Args:
      dt_start (datetime): The slice's start time.
      dt_end (datetime): The slice's end time.
      blazegraph_endpoint (str): The Blazegraph SPARQL endpoint URL.
      
    Returns:
      The JSON data of the query results; returns None if an error occurs.
    """
    # Format as a string allowed by xsd:dateTimeStamp
    formatted_start = format_timestamp(dt_start)
    formatted_end = format_timestamp(dt_end)
    
    # Dynamically extract weekday and HH:MM strings
    weekday_map = {"Mon": "Mo", "Tue": "Tu", "Wed": "We", "Thu": "Th", "Fri": "Fr", "Sat": "Sa", "Sun": "Su"}
    weekday_start = weekday_map[dt_start.strftime("%a")]
    time_str_start = dt_start.strftime("%H:%M")
    weekday_end = weekday_map[dt_end.strftime("%a")]
    time_str_end = dt_end.strftime("%H:%M")
    
    # Construct regex patterns; note that four backslashes represent two backslashes when passed to SPARQL
    query = f"""
PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
PREFIX ies4: <http://ies.data.gov.uk/ontology/ies4#>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX wgs: <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX osmkey: <https://www.openstreetmap.org/wiki/Key:>

SELECT DISTINCT ?be ?geometry ?wkt ?lat ?long ?openingHours
WHERE {{
  # Bind the dynamically extracted weekday and time (HH:MM)
  BIND("{weekday_start}" AS ?weekdayStart)
  BIND("{time_str_start}" AS ?timeStrStart)
  BIND("{weekday_end}" AS ?weekdayEnd)
  BIND("{time_str_end}" AS ?timeStrEnd)
  
  # Construct regex patterns to extract operating hours from opening_hours
  BIND(CONCAT(".*", ?weekdayStart, "(?:-[A-Za-z]{{2}})?\\\\s+([0-9]{{2}}:[0-9]{{2}})-([0-9]{{2}}:[0-9]{{2}}).*") AS ?patternStart)
  BIND(CONCAT(".*", ?weekdayEnd, "(?:-[A-Za-z]{{2}})?\\\\s+([0-9]{{2}}:[0-9]{{2}})-([0-9]{{2}}:[0-9]{{2}}).*") AS ?patternEnd)
  
  ##################################################################
  ## Federated query from Ontop: Get valid BusinessEstablishment (absolute filter)
  ##################################################################
  SERVICE <http://host.docker.internal:5007/ontop/ui/sparql> {{
    SELECT DISTINCT ?be ?geometry ?wkt ?lat ?long
    WHERE {{
      # Bind the slice's start and end time
      BIND("{formatted_start}"^^xsd:dateTimeStamp AS ?givenTimeStart)
      BIND("{formatted_end}"^^xsd:dateTimeStamp AS ?givenTimeEnd)
      # Extract year from start time for geometry filtering
      BIND(YEAR(?givenTimeStart) AS ?timeYear)
      ?be a fh:BusinessEstablishment ;
          geo:hasGeometry ?geometry .
      # Filter geometry by year: geometry IRI format is geo:Geometry/{FHRSID}/{year}
      # This ensures we only get geometries matching the year of the time slice
      FILTER(CONTAINS(STR(?geometry), CONCAT("/", STR(?timeYear))))
      ?geometry geo:asWKT ?wkt .
      OPTIONAL {{ ?geometry wgs:lat ?lat . }}
      OPTIONAL {{ ?geometry wgs:long ?long . }}
      ?startEvent ies4:isStartOf ?be ;
                  ies4:inPeriod ?startPeriod .
      ?startPeriod ies4:iso8601PeriodRepresentation ?startTime .
      ?endEvent ies4:isEndOf ?be ;
                ies4:inPeriod ?endPeriod .
      ?endPeriod ies4:iso8601PeriodRepresentation ?endTime .
      # Ensure the establishment's operating period overlaps with the slice's time interval
      FILTER(?startTime <= ?givenTimeEnd && ?endTime >= ?givenTimeStart)
    }}
  }}
  
  ##################################################################
  OPTIONAL {{ ?be osmkey:opening_hours ?openingHours . }}
  
  ##################################################################
  # Operating hours filtering: Require that the extracted opening time from opening_hours
  # is not later than the slice's start time, and the closing time is not earlier than the slice's end time.
  # If opening_hours is not provided, keep the establishment by default.
  FILTER (
    (!BOUND(?openingHours))
    ||
    (
      regex(?openingHours, ?patternStart) &&
      regex(?openingHours, ?patternEnd) &&
      (?timeStrStart >= REPLACE(?openingHours, ?patternStart, "$1")) &&
      (?timeStrEnd <= REPLACE(?openingHours, ?patternEnd, "$2"))
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
        print("[ERROR] Query:\n" + query)
        print(response.text)
        return None

###############################################################################
# Approach A: Trip-span Intersecting
###############################################################################
def approach_a(points, buffer_radius, blazegraph_endpoint):
    """
    Approach A:
      1) For the entire CSV trajectory, extract the overall start (trip_start) and end times (trip_end).
      2) Build a full buffer using these points.
      3) Query business establishments within the buffer.
      4) For establishments with opening_hours, check if their operating hours intersect with [trip_start, trip_end];
         if opening_hours is not provided, keep the establishment by default.
    """
    trip_start = points[0][0]
    trip_end = points[-1][0]
    full_buffer = create_full_buffer(points, buffer_radius)
    
    # Use a simplified SPARQL query (here calling naive_sparql_query which queries using the entire time interval)
    results_json = naive_sparql_query(trip_start, trip_end, blazegraph_endpoint)
    valid_uris = filter_by_buffer(results_json, full_buffer)
    
    final_uris = set()
    for binding in results_json.get("results", {}).get("bindings", []):
        be_uri = binding["be"]["value"]
        if be_uri not in valid_uris:
            continue
        opening_hours = binding.get("openingHours", {}).get("value")
        if not opening_hours:
            final_uris.add(be_uri)
        else:
            # Call the check function to determine if [trip_start, trip_end] overlaps with the establishment's operating hours
            if check_overlap_naive(trip_start, trip_end, binding):
                final_uris.add(be_uri)
    return final_uris

###############################################################################
# Approach B: Nearest-Point Filtering
###############################################################################
def approach_b(points, buffer_radius, blazegraph_endpoint):
    """
    Approach B:
      1) Construct a buffer for the entire trajectory.
      2) Query business establishments within the buffer.
      3) For each business establishment, find the sensor point closest to it.
         - If the establishment has no opening_hours, keep it directly;
         - Otherwise, check if the time of the nearest sensor point falls within the establishment's operating hours.
           If yes, keep it; otherwise, filter it out.
    """
    full_buffer = create_full_buffer(points, buffer_radius)
    trip_start = points[0][0]
    trip_end = points[-1][0]
    results_json = naive_sparql_query(trip_start, trip_end, blazegraph_endpoint)
    valid_uris = filter_by_buffer(results_json, full_buffer)
    
    final_uris = set()
    for binding in results_json.get("results", {}).get("bindings", []):
        be_uri = binding["be"]["value"]
        if be_uri not in valid_uris:
            continue
        opening_hours = binding.get("openingHours", {}).get("value")
        if not opening_hours:
            final_uris.add(be_uri)
            continue
        wkt_str = binding.get("wkt", {}).get("value")
        if not wkt_str:
            continue
        try:
            geom_4326 = wkt.loads(wkt_str)
            geom_27700 = transform(project_4326_to_27700, geom_4326)
            # Find the nearest sensor point (traverse all points and return the closest one along with its time)
            nearest_pt, nearest_ts = find_nearest_point_in_space(geom_27700, points)
            if check_time_in_opening_hours(nearest_ts, opening_hours):
                final_uris.add(be_uri)
        except Exception as e:
            print("[WARN] Nearest point error:", e)
    return final_uris

###############################################################################
# Approach C: Time-Sliced Matching (Model C)
###############################################################################
def approach_c(points, delta_t_seconds, buffer_radius, blazegraph_endpoint):
    """
    Approach C:
      Use a time-slicing method: split the trajectory into slices based on Δt,
      dynamically construct a SPARQL query for each slice, and merge the results.
    """
    slices = slice_trajectory_with_interpolation(points, delta_t_seconds)
    union_exposures = set()
    total_query_time = 0.0
    for i, (slice_data, mid_time) in enumerate(slices):
        dt_start = slice_data[0][0]
        dt_end   = slice_data[-1][0]
        buffer_poly = compute_buffer(slice_data, buffer_radius)
        t0 = time.time()
        results_json = query_sparql(dt_start, dt_end, blazegraph_endpoint)
        elapsed = time.time() - t0
        total_query_time += elapsed
        valid_be = filter_by_buffer(results_json, buffer_poly)
        union_exposures.update(valid_be)
    print(f"[DEBUG] Time-sliced total query time: {total_query_time:.2f} s.")
    return union_exposures

###############################################################################
# The following are helper functions (placeholder examples, extendable as needed)
###############################################################################
def naive_sparql_query(dt_start, dt_end, blazegraph_endpoint):
    """
    A simple SPARQL query function that directly calls query_sparql using the entire time interval.
    In Approaches A and B, the query can be adjusted as needed.
    """
    return query_sparql(dt_start, dt_end, blazegraph_endpoint)

def filter_by_buffer(results_json, buffer_poly):
    """
    Iterates through all business establishment data (with geometry in WKT) returned by the SPARQL query,
    parses and projects them to EPSG:27700,
    and if they intersect with the given buffer_poly, records their URI.
    Returns a set of the intersecting URIs.
    """
    valid_be = set()
    if results_json is None:
        return valid_be
    bindings = results_json.get("results", {}).get("bindings", [])
    reproj = Transformer.from_crs("EPSG:4326", "EPSG:27700", always_xy=True).transform
    for binding in bindings:
        wkt_str = binding.get("wkt", {}).get("value")
        if not wkt_str:
            continue
        try:
            geom_4326 = wkt.loads(wkt_str)
            geom_27700 = transform(reproj, geom_4326)
            if geom_27700.intersects(buffer_poly):
                valid_be.add(binding["be"]["value"])
        except Exception as e:
            print("[WARN] WKT parsing error:", e)
    return valid_be

def check_overlap_naive(tstart, tend, binding):
    """
    Placeholder extension point for additional overlap validation.

    This function is called in Approaches A and C to perform a secondary
    check of whether the time slice [tstart, tend] overlaps with the
    business’s opening hours. In the current implementation, the
    opening-hours filtering is already handled within the SPARQL query
    via regular expressions, so this function simply returns True.

    You can replace this placeholder with custom logic—for example,
    parsing binding["openingHours"]["value"] (e.g. "09:00-17:00")
    and returning False if the slice lies entirely outside that range.
    """
    return True


def check_time_in_opening_hours(sensor_time, opening_hours_string):
    """
    Placeholder extension point for time-in-hours validation.

    This function is called in Approach B to perform an additional
    check on a single representative timestamp (sensor_time) against
    the opening_hours_string. The SPARQL query already applies a regex-
    based filter on opening hours, so this function currently always
    returns True.

    You can extend it to parse opening_hours_string (e.g. "09:00-17:00")
    and return True only if sensor_time.time() falls within that interval,
    handling cases like overnight hours if needed.
    """
    return True


def find_nearest_point_in_space(business_geom, points):
    """
    Given the geometry of a business establishment (projected to EPSG:27700) and a list of sensor points (EPSG:27700),
    finds the point closest to business_geom using an O(n) traversal,
    returning (nearest point coordinates, nearest point time).
    For large datasets, consider using an R-tree to improve efficiency.
    """
    from math import inf
    min_dist = inf
    nearest_pt = None
    nearest_ts = None
    for (ts, (x, y)) in points:
        pt = Point(x, y)
        d = business_geom.distance(pt)
        if d < min_dist:
            min_dist = d
            nearest_pt = pt
            nearest_ts = ts
    return nearest_pt, nearest_ts

###############################################################################
# Main Experiment Function
###############################################################################
def main_experiment(csv_file, delta_t_seconds, buffer_radius, blazegraph_endpoint, approach='c'):
    """
    Main Process:
      The parameter 'approach' controls which model to use:
         'a' -> Approach A: Trip-span intersecting
         'b' -> Approach B: Nearest-point filtering
         'c' -> Approach C: Time-sliced matching (default)
    """
    points = read_trajectory(csv_file)
    if approach == 'a':
        print("[INFO] Using Approach A: Full time interval filtering")
        exposures = approach_a(points, buffer_radius, blazegraph_endpoint)
    elif approach == 'b':
        print("[INFO] Using Approach B: Nearest sensor point filtering")
        exposures = approach_b(points, buffer_radius, blazegraph_endpoint)
    else:
        print("[INFO] Using Approach C: Time-sliced matching")
        exposures = approach_c(points, delta_t_seconds, buffer_radius, blazegraph_endpoint)
    
    print(f"[RESULT] Approach '{approach}' found {len(exposures)} business establishments.")
    return exposures