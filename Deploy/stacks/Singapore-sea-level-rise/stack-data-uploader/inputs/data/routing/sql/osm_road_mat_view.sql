DROP MATERIALIZED VIEW IF EXISTS osm_road_mat_view;
CREATE MATERIALIZED VIEW osm_road_mat_view AS
SELECT 
w.osm_id,
w.name, 
ROUND(CAST(SUM(w.length_m) AS numeric), 2) AS total_length_m,
INITCAP(c.tag_value) AS road_type, 
w.oneway, 
w.maxspeed_forward, 
w.maxspeed_backward, 
ST_Collect(w.the_geom) AS geom
FROM 
routing_ways w
JOIN 
configuration c 
ON 
w.tag_id = c.tag_id
GROUP BY 
w.osm_id,
w.name, 
c.tag_value, 
w.oneway, 
w.maxspeed_forward, 
w.maxspeed_backward;