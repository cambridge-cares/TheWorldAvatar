WITH BufferedLine AS (
    SELECT 
        ST_Buffer(
            ST_MakeLine(gps."column7"::geometry ORDER BY gps."time")::geography, 
            100
        ) AS buffered_geom -- create static buffer
    FROM 
        "35cb3043-9bfc-4cc7-b995-fe6290d8c86b" AS gps -- example table name
),
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