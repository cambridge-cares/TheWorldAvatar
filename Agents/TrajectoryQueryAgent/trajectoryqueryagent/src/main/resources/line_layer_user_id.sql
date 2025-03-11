/* Used by timeline app, produces a single line rather than line segments */
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
        timeseries.user_id AS user_id
    FROM
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY time
)

SELECT 
    MIN(time) AS start_time,  
    MAX(time) AS end_time,
    ST_MakeLine(geom) as geom, 
    CONCAT('https://w3id.org/MON/person.owl#person_', '%user_id%') AS iri,
    session_id AS session_id,
    1 AS id
    ST_Length(ST_Transform(ST_MakeLine(ARRAY_AGG(geom ORDER BY time)), 3857))::INTEGER AS distance_traveled
FROM 
    timeseries ts
WHERE
    ('%user_id%' = '' OR user_id = '%user_id%')
    AND (%lowerbound% = 0 OR time > %lowerbound%)
    AND (%upperbound% = 0 OR time < %upperbound%)
GROUP BY
    ts.session_id
    ORDER BY start_time;
