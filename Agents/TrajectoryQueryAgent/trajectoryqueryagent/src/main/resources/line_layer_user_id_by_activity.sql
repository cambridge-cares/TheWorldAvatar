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
        a.confidence_level
    FROM 
        timeseries t
    JOIN 
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
        activity_type,
        confidence_level
    FROM (
        SELECT 
            time,
            speed,
            altitude,
            geom,
            bearing,
            session_id,
            user_id,
            activity_type,
            confidence_level,
            LAG(time) OVER (PARTITION BY user_id ORDER BY time) AS prev_time
        FROM joined_data
    ) AS subquery
    WHERE time <> prev_time  
),

change_marked AS (
    SELECT 
        *,
        CASE
            WHEN 
                LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time) IS NULL
                THEN 0  
            WHEN 
                LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time) IS DISTINCT FROM activity_type 
            THEN 1 
            ELSE 0 
        END AS change_flag
    FROM fixed_activity_data
),

change_marked_union AS (
    SELECT 
        time,
        speed,
        altitude,
        geom,
        bearing,
        session_id,
        user_id,
        activity_type,
        confidence_level,
        change_flag
    FROM change_marked

    UNION ALL  
    SELECT 
        time,
        speed,
        altitude,
        geom,
        bearing,
        session_id,
        user_id,
        COALESCE(LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time), 'others') as activity_type,  
        confidence_level,
        0 AS change_flag  
    FROM change_marked
    WHERE change_flag = 1   
    ORDER BY time, change_flag 
),

numbered_activity_data AS (
    SELECT 
        *,
        SUM(change_flag) OVER (PARTITION BY user_id ORDER BY time ROWS UNBOUNDED PRECEDING) + 1 AS id
    FROM change_marked_union cm
    ORDER BY cm.time, cm.change_flag
),

filled_activity_data AS (
    SELECT 
        *,
        COALESCE(
            NULLIF(activity_type, 'others'),
            LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time),
            FIRST_VALUE(activity_type) OVER (PARTITION BY user_id ORDER BY time ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
        ) AS filled_activity_type
    FROM numbered_activity_data
)

SELECT
    MIN(na.time) AS start_time,  
    MAX(na.time) AS end_time,
    na.id,
    na.filled_activity_type AS activity_type,
    na.session_id,
    ST_MakeLine(ARRAY_AGG(na.geom ORDER BY na.time)) AS geom, 
    ST_Length(ST_Transform(ST_MakeLine(ARRAY_AGG(na.geom ORDER BY na.time)), 3857))::INTEGER AS distance_traveled,
    CONCAT('https://w3id.org/MON/person.owl#person_', na.user_id) AS iri
FROM
    filled_activity_data AS na 
WHERE  
    ('%user_id%' = '' OR na.user_id = '%user_id%')
    AND ('%lowerbound%' = '0' OR na.time > '%lowerbound%'::BIGINT)
    AND ('%upperbound%' = '0' OR na.time < '%upperbound%'::BIGINT)
GROUP BY
    na.id, na.filled_activity_type, na.user_id, na.session_id
