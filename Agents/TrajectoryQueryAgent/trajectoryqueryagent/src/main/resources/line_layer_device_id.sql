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
        timeseries.session_id AS session_id,
        timeseries.device_id AS device_id
    FROM
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY time
),

line AS (
    SELECT 
        ts.time as time,
        LAG(ts.geom) OVER (PARTITION BY device_id, session_id ORDER BY ts.time) AS prev_geom,
        ST_MakeLine(LAG(ts.geom) OVER (PARTITION BY device_id, session_id ORDER BY ts.time), ts.geom) AS geom,
        ts.speed AS speed,
        ts.altitude AS altitude,
        ts.bearing AS bearing,
        ts.device_id AS device_id,
        get_device_iri(ts.device_id) AS iri
    FROM 
        timeseries ts
)

SELECT 
    time, geom, speed, altitude, bearing, iri
FROM 
    line
WHERE
    line.prev_geom IS NOT NULL
    AND ('%device_id%' = '' OR device_id = '%device_id%')