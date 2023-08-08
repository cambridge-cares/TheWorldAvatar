--Importing grid_primary_site--
ALTER TABLE grid_primary_site
ADD COLUMN geom geometry(Point, 4326);

UPDATE grid_primary_site
SET geom = 
    CASE
        WHEN "Spatial Coordinates" ~ '^-?[0-9]+(\.[0-9]+)?,\s*-?[0-9]+(\.[0-9]+)?$' THEN
            ST_SetSRID(ST_MakePoint(
                CAST(split_part("Spatial Coordinates", ',', 2) AS double precision),
                CAST(split_part("Spatial Coordinates", ',', 1) AS double precision)
            ), 4326)
        ELSE
            NULL
    END;

--Remove out of bounds site--
DELETE FROM grid_primary_site
WHERE geom IS NULL OR NOT ST_Within(geom, ST_MakeEnvelope(0.334832,52.732131,0.455535,52.777448, 4326));


--Add closest edge to site--
ALTER TABLE grid_primary_site ADD COLUMN closest_edge INTEGER;

UPDATE grid_primary_site SET closest_edge = (
  SELECT edge_id FROM pgr_findCloseEdges(
    $$SELECT gid as id, the_geom as geom FROM public.routing_ways$$,
    (SELECT grid_primary_site.geom),
    0.5, partial => false)
  LIMIT 1
);

-- Alter the "grid_primary_site" table to add the "source" and "target" columns
ALTER TABLE grid_primary_site
ADD COLUMN source BIGINT,
ADD COLUMN target BIGINT;


-- Update the "source" and "target" columns based on the matching "closest_edge" with "id" in the "routing_ways" table
UPDATE grid_primary_site
SET source = routing_ways.source, target = routing_ways.target
FROM routing_ways
WHERE grid_primary_site.closest_edge = routing_ways.gid;

ALTER TABLE grid_primary_site
ADD COLUMN closest_node INT;

ALTER TABLE grid_primary_site ADD COLUMN source_distance numeric;
UPDATE grid_primary_site AS p
SET source_distance = (
    SELECT ST_DISTANCE(p.geom, w.the_geom)
    FROM routing_ways_vertices_pgr AS w
    WHERE p.source = w.id
);

ALTER TABLE grid_primary_site ADD COLUMN target_distance numeric;
UPDATE grid_primary_site AS p
SET target_distance = (
    SELECT ST_DISTANCE(p.geom, w.the_geom)
    FROM routing_ways_vertices_pgr AS w
    WHERE p.target = w.id
);

UPDATE grid_primary_site
SET closest_node = CASE
    WHEN source_distance < target_distance THEN source
    ELSE target
END;


--Create SQL view for Traveeling Salesman--
CREATE OR REPLACE VIEW tsp AS 
SELECT *
FROM pgr_TSP(
    $$SELECT * FROM pgr_dijkstraCostMatrix(
        'SELECT gid as id, source, target, cost_s as cost FROM routing_ways',
        (
            SELECT array_agg(id)
            FROM routing_ways_vertices_pgr
            WHERE id IN (SELECT closest_node FROM grid_primary_site, flood_polygon_single WHERE ST_Intersects(grid_primary_site.geom, flood_polygon_single.geom) OR ST_DISTANCE (grid_primary_site.geom, flood_polygon_single.geom) <0.005)
        ),
        false
    )$$, 
    (
		SELECT MAX(closest_node)
FROM grid_primary_site, flood_polygon_single WHERE ST_Intersects(grid_primary_site.geom, flood_polygon_single.geom) OR ST_DISTANCE (grid_primary_site.geom, flood_polygon_single.geom) <0.005
    ), 
    (
		SELECT MAX(closest_node)
FROM grid_primary_site, flood_polygon_single WHERE ST_Intersects(grid_primary_site.geom, flood_polygon_single.geom) OR ST_DISTANCE (grid_primary_site.geom, flood_polygon_single.geom) <0.005
    )
);