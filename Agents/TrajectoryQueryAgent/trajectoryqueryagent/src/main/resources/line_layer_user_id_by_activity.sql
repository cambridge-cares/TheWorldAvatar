--only get devices that allow for activity tracking
WITH distinct_devices AS (
    SELECT 
        array_agg(device_id::text) AS device_list
    FROM 
        (SELECT DISTINCT device_id 
         FROM devices 
         WHERE sensor_class = 'Activity') AS distinct_devices
),

--get location data without activity
timeseries AS (
    SELECT
        *
    FROM
        public.get_location_table((SELECT device_list FROM distinct_devices)) AS timeseries
    ORDER BY time
),

--get activity data
activity_data AS (
    SELECT
        *
    FROM
        public.get_activity_table((SELECT device_list FROM distinct_devices)) AS activity_data
    ORDER BY time
),

--join data so all location points are accounted for and allows for time to be off by 5 seconds
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
        ABS(t.time - a.time) <= 5000 -- Allow a 5-second difference
    ORDER BY t.time
),

--ensure no repeat timestamps
--make all instances of null in activity_type = "others"
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

--call function to get a table that contains all activity_types and there associated times
--but all instances of activity_types = 'others' is the closest previous type or the first non-'others' type if no previous type
temp_activity_table AS (
    SELECT * FROM fill_activity_types(
        (SELECT array_agg(activity_type ORDER BY time) FROM fixed_activity_data),  -- Aggregating activity_type by time order
        (SELECT array_agg(time ORDER BY time) FROM fixed_activity_data)  -- Aggregating time by order
    )
),

--join original table and table with filled activities so all activity types are valid
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

--add field for to mark each time an activity type changes
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

--add duplicate row at each change with the previous activity type to ensure smooth line segments and timing
change_marked_union AS (
    SELECT * FROM change_marked
    UNION ALL  
    SELECT time, speed, altitude, geom, bearing, session_id, user_id, 
        LAG(activity_type) OVER (PARTITION BY user_id ORDER BY time), confidence_level, 0
    FROM change_marked WHERE change_flag = 1
    ORDER BY time, change_flag
),

--add column id to have a unique id for each segment of the trajectory by adding all change_flag up to that point
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
    MIN(time) AS start_time,  
    MAX(time) AS end_time,
    user_id,
    activity_type,
    session_id,
    ST_MakeLine(ARRAY_AGG(geom ORDER BY time)) AS geom, 
    ST_Length(ST_Transform(ST_MakeLine(ARRAY_AGG(geom ORDER BY time)), 3857))::INTEGER AS distance_traveled,
    CONCAT('https://w3id.org/MON/person.owl#person_', user_id) AS iri
FROM
    numbered_activity_data
WHERE  
    ('%user_id%' = '' OR user_id = '%user_id%')
    AND ('%lowerbound%' = '0' OR time > '%lowerbound%'::BIGINT)
    AND ('%upperbound%' = '0' OR time < '%upperbound%'::BIGINT)
GROUP BY
    id, activity_type, user_id, session_id
ORDER BY start_time