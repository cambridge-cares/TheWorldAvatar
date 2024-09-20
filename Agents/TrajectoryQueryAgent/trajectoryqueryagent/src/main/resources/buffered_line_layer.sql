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
        timeseries.geom AS geom,
        timeseries.bearing AS bearing,
        timeseries.device_id AS device_id
    FROM 
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY 
        time
)

SELECT 
    ST_Buffer(ST_Transform(ST_MakeLine(ts.geom), 3857), 100) AS geom
FROM 
    timeseries ts