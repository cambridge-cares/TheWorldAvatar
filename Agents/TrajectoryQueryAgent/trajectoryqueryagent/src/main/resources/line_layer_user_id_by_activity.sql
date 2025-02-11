/* Used by timeline app, produces a single line rather than line segments */
WITH distinct_devices AS (
    SELECT 
        array_agg(device_id::text) AS device_list
    FROM 
        (SELECT DISTINCT device_id 
         FROM devices 
         WHERE sensor_class = 'Activity') AS distinct_devices
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
),

activity_data AS (
    SELECT
        activity_data.time AS time,
        activity_data.user_id AS user_id,
        activity_data.activity_type AS activity_type,
        activity_data.confidence_level AS confidence_level
    FROM
        public.get_activity_table((SELECT device_list FROM distinct_devices)) AS activity_data
    ORDER BY time
),

joined_data AS (
    SELECT 
        t.time, 
        t.speed, 
        t.altitude, 
        t.geom, 
        t.bearing, 
        t.session_id, 
        t.user_id, 
        a.activity_type,
        a.confidence_level,
        LAG(a.activity_type) OVER (PARTITION BY t.user_id ORDER BY t.time) AS prev_activity_type
    FROM 
        timeseries t
    LEFT JOIN 
        activity_data a 
    ON 
        t.user_id = a.user_id 
        AND ABS(t.time - a.time) <= 5000 -- Allow a 5-second difference
    ORDER BY t.time
),

fixed_activity_data AS (
    SELECT 
        time,
        speed,
        altitude,
        geom,
        bearing,
        session_id,
        user_id,
        COALESCE(NULLIF(activity_type, 'Others'), prev_activity_type) AS activity_type,
        confidence_level
    FROM joined_data
),

change_marked AS (
    SELECT 
        *,
        CASE 
            WHEN 
                LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time) IS DISTINCT FROM activity_type 
                AND activity_type <> 'Others'  -- Ignore "Others" as a change
            THEN 1 
            ELSE 0 
        END AS change_flag
    FROM fixed_activity_data
),

numbered_activity_data AS (
    SELECT 
        *,
        SUM(change_flag) OVER (PARTITION BY user_id ORDER BY time ROWS UNBOUNDED PRECEDING) + 1 AS id
    FROM change_marked
)

SELECT
    na.id,
    na.activity_type,
    ST_MakeLine(ARRAY_AGG(na.geom ORDER BY na.time)) as geom, 
    CONCAT('https://w3id.org/MON/person.owl#person_', '%user_id%') AS iri
FROM
    numbered_activity_data AS na 
WHERE  
    ('%user_id%' = '' OR user_id = '%user_id%')
    AND ('%lowerbound%' = '0' OR time > '%lowerbound%'::BIGINT)
    AND ('%upperbound%' = '0' OR time < '%upperbound%'::BIGINT)
GROUP BY
    na.id, na.activity_type, na.user_id




