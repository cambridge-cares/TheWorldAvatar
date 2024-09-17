WITH distinct_devices AS (
    SELECT 
        array_agg(device_id::text) AS device_list
    FROM (
        SELECT DISTINCT device_id 
        FROM devices
    ) AS distinct_devices
),

timeseries AS (
    SELECT
        timeseries.time AS time,
        timeseries.speed AS speed,
        timeseries.altitude AS altitude,
        ST_Transform(timeseries.geom, 24500) AS geom,
        timeseries.bearing AS bearing,
        timeseries.device_id AS device_id
    FROM 
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY 
        time
),

buffered_line AS (
    SELECT 
        ST_Buffer(ST_MakeLine(ts.geom), 100) AS geom
    FROM 
        timeseries ts
)
SELECT 
    iri 
FROM 
    buildings_layer b, 
    buffered_line l
WHERE 
    ST_Intersects(b.geom, l.geom)