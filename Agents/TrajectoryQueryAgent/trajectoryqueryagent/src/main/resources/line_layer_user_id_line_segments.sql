WITH distinct_devices AS (
    SELECT 
        array_agg(device_id::text) as device_list
    FROM 
        (SELECT DISTINCT device_id FROM devices) AS distinct_devices
),

timeseries AS (
    SELECT
        timeseries.time AS time,
        timeseries.speed AS speed,
        timeseries.altitude AS altitude,
        timeseries.geom AS geom,
        timeseries.bearing AS bearing,
        timeseries.user_id AS user_id
    FROM
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY time
),

line AS (
    SELECT 
        ts.time as time,
        LAG(ts.geom) OVER (ORDER BY ts.time) AS prev_geom,
        ST_MakeLine(LAG(ts.geom) OVER (ORDER BY ts.time), ts.geom) AS geom,
        ts.speed AS speed,
        ts.altitude AS altitude,
        ts.bearing AS bearing,
        ts.user_id AS user_id,
        LAG(ts.user_id) OVER (ORDER BY ts.time) AS prev_user_id,
        CONCAT('https://w3id.org/MON/person.owl#person_', ts.user_id) AS iri
    FROM 
        timeseries ts
)

SELECT 
    time / 1000 AS time, geom, speed, altitude, bearing, iri
FROM 
    line
WHERE
    line.prev_geom IS NOT NULL
    AND ('%user_id%' = '' OR (user_id = '%user_id%' AND prev_user_id = '%user_id%'))