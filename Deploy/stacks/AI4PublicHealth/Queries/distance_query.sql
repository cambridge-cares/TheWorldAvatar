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
ON ST_DWithin(geography(gps.geom), geography(frs.geom), 100)
GROUP BY gps."UTC DATE", gps."UTC TIME", frs.geom, frs."Name", frs."Address"