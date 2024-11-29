WITH BufferedLine AS (
    SELECT 
        ST_Buffer(ST_MakeLine(gps.geom::geometry ORDER BY gps."UTC TIME")::geography, 100) AS buffered_geom -- Buffer the line in geography units (meters)
    FROM 
        public.gps_data AS gps
),
-- Combine supermarkets and takeaways into one common table
combined_frs AS (
    SELECT "Name", "Address", geom
    FROM public.supermarket
    UNION ALL
    SELECT "Name", "Address", geom
    FROM public.takeaways
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
