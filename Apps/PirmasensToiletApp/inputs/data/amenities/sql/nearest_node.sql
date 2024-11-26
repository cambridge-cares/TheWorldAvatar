ALTER TABLE points ADD COLUMN IF NOT EXISTS nearest_node_id BIGINT;

WITH nearest_nodes_table AS (
select (
    SELECT id
    FROM routing_ways_vertices_pgr AS r
    ORDER BY ST_Distance(r.the_geom, ST_Centroid(p."geometryProperty"))
    LIMIT 1
), p.osm_id
FROM points AS p
WHERE p.amenity = 'toilets' OR p.toilets_position IS NOT NULL OR p.toilets_handwashing IS NOT NULL
)

UPDATE points
SET nearest_node_id = nearest_nodes_table.id 
FROM nearest_nodes_table 
WHERE points.osm_id = nearest_nodes_table.osm_id