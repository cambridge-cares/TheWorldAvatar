WITH BufferedLine AS (
    SELECT 
        ST_Buffer(
            ST_MakeLine(gps."column7"::geometry ORDER BY gps."time")::geography, 
            {exposure_radius}
        ) AS buffered_geom
    FROM 
        "{table_name}" gps
),
combined_pts AS (
    {union_sql}
)
SELECT combined_pts.geom
FROM 
    BufferedLine bl
JOIN 
    combined_pts
ON 
    ST_Intersects(bl.buffered_geom, combined_pts.geom::geography);
