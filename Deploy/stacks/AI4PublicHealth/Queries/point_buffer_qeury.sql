SELECT gps."UTC DATE" AS date, 
       gps."UTC TIME" AS time, 
       frs."Name" AS entity_name, 
       frs."Address" AS address,
       ST_AsText(frs.geom) AS entity_geom, 
       COUNT(frs."Name") AS no_of_entities
FROM public.gps_data AS gps
JOIN (
    SELECT "Name", "Address", geom
    FROM public.supermarket
    UNION ALL
    SELECT "Name", "Address", geom
    FROM public.takeaways
) AS frs
ON ST_Intersects(ST_Buffer(geography(gps.geom), 100), geography(frs.geom))
GROUP BY gps."UTC DATE", gps."UTC TIME", frs."Name", frs."Address", frs.geom
ORDER BY gps."UTC DATE", gps."UTC TIME"
