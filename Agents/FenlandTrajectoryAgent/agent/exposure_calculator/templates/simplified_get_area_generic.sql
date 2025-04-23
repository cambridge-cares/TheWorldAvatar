WITH Trajectory AS (
    SELECT ST_Transform(
        ST_Buffer(
            ST_MakeLine("column7"::geometry ORDER BY "time")::geography, 
            {exposure_radius}
        )::geometry, 
        27700
    ) AS buffered_trajectory
    FROM "{table_name}"
),
combined_area AS (
    {union_sql}
)
SELECT ca.wkb_geometry
FROM combined_area ca, Trajectory t
WHERE ST_Intersects(ca.wkb_geometry, t.buffered_trajectory);
