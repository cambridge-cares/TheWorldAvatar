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
        ST_MakeLine(timeseries.geom) AS geom 
    FROM 
        timeseries
),

offset_timeseries AS (
    SELECT
        LEAD(timeseries.time) OVER (ORDER BY timeseries.time) AS time,
        LEAD(timeseries.speed) OVER (ORDER BY timeseries.time) AS speed,
        LEAD(timeseries.altitude) OVER (ORDER BY timeseries.time) AS altitude,
        LEAD(timeseries.geom) OVER (ORDER BY timeseries.time) AS geom,
        LEAD(timeseries.bearing) OVER (ORDER BY timeseries.time) AS bearing,
        LEAD(timeseries.user_id) OVER (ORDER BY timeseries.time) AS user_id
    FROM
        timeseries
),

split_segments AS (
    SELECT 
        offset_timeseries.time as time,
        (ST_Dump(ST_Split(line.geom, offset_timeseries.geom))).geom AS geom,
        offset_timeseries.speed AS speed,
        offset_timeseries.altitude AS altitude,
        offset_timeseries.bearing AS bearing,
        CONCAT('https://w3id.org/MON/person.owl#person_', offset_timeseries.user_id) AS iri,
        offset_timeseries.user_id AS user_id
    FROM 
        line, offset_timeseries
)

SELECT 
    *
FROM 
    split_segments