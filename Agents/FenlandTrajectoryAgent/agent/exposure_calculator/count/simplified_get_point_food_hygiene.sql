WITH BufferedLine AS (
    SELECT 
        ST_Buffer(
            ST_MakeLine(gps."column7"::geometry ORDER BY gps."time")::geography, 
            {exposure_radius}
        ) AS buffered_geom
    FROM 
        "{table_name}" gps
),
combined_frs AS (
    {union_sql}
)
SELECT 
    frs."Name" AS entity_name, 
    frs."Address" AS address,
    ST_AsText(frs.geom) AS entity_geom,
    COUNT(frs."Name") AS no_of_entities
FROM 
    BufferedLine bl
JOIN 
    combined_frs frs
ON 
    ST_Intersects(bl.buffered_geom, frs.geom::geography)
GROUP BY
    frs."Name", frs."Address", frs.geom;
