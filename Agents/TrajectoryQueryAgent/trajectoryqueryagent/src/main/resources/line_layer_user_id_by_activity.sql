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
        *
    FROM
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY time
),

activity_data AS (
    SELECT
        *
    FROM
        public.get_activity_table((SELECT device_list FROM distinct_devices)) AS activity_data
    ORDER BY time
),

joined_data AS (
    SELECT 
        t.*, 
        a.activity_type,
        a.confidence_level,
        LAG(t.time) OVER (PARTITION BY t.user_id ORDER BY t.time) AS prev_time
    FROM 
        timeseries t
    LEFT JOIN 
        activity_data a 
    ON 
        ABS(t.time - a.time) <= 5000 
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
        confidence_level,
        COALESCE(activity_type, 'others') AS activity_type
    FROM joined_data
    WHERE time <> prev_time  
),

temp_activity_table AS (
    SELECT * FROM fill_activity_types(
        (SELECT array_agg(activity_type ORDER BY time) FROM fixed_activity_data),  
        (SELECT array_agg(time ORDER BY time) FROM fixed_activity_data) 
    )
),

filled_activity_data AS (
    SELECT
        fixed.time AS time,
        fixed.speed AS speed,
        fixed.altitude AS altitude,
        fixed.geom AS geom,
        fixed.bearing AS bearing,
        fixed.session_id AS session_id,
        fixed.user_id AS user_id,
        temp.activity_type AS activity_type,
        fixed.confidence_level AS confidence_level
    FROM fixed_activity_data AS fixed
    JOIN temp_activity_table AS temp 
    ON fixed.time = temp.time
    ORDER BY time
),

change_marked AS (
    SELECT
        *,
        CASE
            WHEN
                LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time) IS DISTINCT FROM activity_type
            THEN 1
            ELSE 0 
        END AS change_flag
    FROM filled_activity_data
),

change_marked_union AS (
    SELECT * FROM change_marked
    UNION ALL
    SELECT time, speed, altitude, geom, bearing, session_id, user_id,
        LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time), confidence_level, 0
    FROM change_marked WHERE change_flag = 1
    ORDER BY time, change_flag
),

numbered_activity_data AS (
    SELECT 
        *,
        SUM(change_flag) OVER (PARTITION BY user_id ORDER BY time ROWS UNBOUNDED PRECEDING) AS id
    FROM change_marked_union cm
    WHERE activity_type IS NOT NULL
    ORDER BY cm.time, cm.change_flag
)

SELECT
    id,
    MIN(na.time) AS start_time,
    MAX(na.time) AS end_time,
    user_id,
    activity_type,
    session_id,
    ST_MakeLine(geom) as geom,
    ST_Length(ST_Transform(ST_MakeLine(geom), 3857))::INTEGER AS distance_traveled,
    CONCAT('https://w3id.org/MON/person.owl#person_', user_id) AS iri
FROM
    numbered_activity_data na
WHERE
('%user_id%' = '' OR user_id = '%user_id%')
AND ('%lowerbound%' = '0' OR na.time > '%lowerbound%'::BIGINT)
AND ('%upperbound%' = '0' OR na.time < '%upperbound%'::BIGINT)
GROUP BY
    na.id, na.activity_type, na.user_id, na.session_id
ORDER BY start_time