ALTER TABLE routing_ways_vertices_pgr ADD COLUMN elevation double precision;
UPDATE routing_ways_vertices_pgr
SET elevation = ST_Value(elevation.rast, 1,  routing_ways_vertices_pgr.the_geom)
FROM elevation
WHERE ST_Intersects(elevation.rast, routing_ways_vertices_pgr.the_geom);

ALTER TABLE routing_ways ADD COLUMN source_elevation double precision;
ALTER TABLE routing_ways ADD COLUMN target_elevation double precision;

UPDATE routing_ways w
SET source_elevation = v1.elevation,
    target_elevation = v2.elevation
FROM routing_ways_vertices_pgr v1, routing_ways_vertices_pgr v2
WHERE w.source = v1.id
AND w.target = v2.id;

ALTER TABLE routing_ways ADD COLUMN slope FLOAT;

UPDATE routing_ways SET slope = ((target_elevation - source_elevation) / source_elevation) * 100;


