WITH Line AS (
    SELECT 
        gps."UTC DATE", 
        gps."UTC TIME",
        ST_MakeLine(gps.geom::geometry) AS line_geom -- Creates a line from geometry points
    FROM 
        public.gps_data AS gps
    GROUP BY
        gps."UTC DATE", gps."UTC TIME"
),
BufferedLine AS (
    SELECT
        "UTC DATE",
        "UTC TIME",
        ST_Buffer(line_geom::geography, 100) AS buffered_geom -- Buffer the line in geography units (meters)
    FROM Line
),
-- Combine supermarkets and takeaways into one common table
combined_frs AS (
    SELECT "Name", "Address", geom
    FROM public.supermarket
    UNION ALL
    SELECT "Name", "Address", geom
    FROM public.takeaways
)
SELECT bl."UTC DATE" AS date, 
       bl."UTC TIME" AS time, 
       frs."Name" AS entity_name,
       ST_AsText(frs.geom) AS entity_geom, 
       frs."Address" AS address, 
       COUNT(frs."Name") AS no_of_entities
FROM BufferedLine bl
JOIN combined_frs frs
ON ST_Intersects(bl.buffered_geom, frs.geom::geography)
GROUP BY bl."UTC DATE", bl."UTC TIME", frs."Name", frs."Address", frs.geom
ORDER BY bl."UTC DATE", bl."UTC TIME";
