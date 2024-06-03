WITH slr AS (
    SELECT geom
    FROM "sealevelprojections"
    WHERE uuid='7a1ad181-7dce-4871-b548-cfd3065a76b7'
    ORDER BY sealevelriseinmeters DESC
),
     routing_ways AS (
         SELECT osm_id, name,length_m, the_geom
         FROM routing_ways
     )
SELECT DISTINCT routing_ways.osm_id, routing_ways.name, routing_ways.length_m
FROM slr,routing_ways
WHERE ST_INTERSECTS(slr.geom, routing_ways."the_geom")