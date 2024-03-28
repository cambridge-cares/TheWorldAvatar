SELECT v.id, v.the_geom 
FROM routing_ways_vertices_pgr AS v, routing_ways AS e 
WHERE v.id = (
    SELECT nearest_node_id 
    FROM points 
    ORDER BY "geometryProperty" <-> ST_SetSRID(ST_MakePoint(%lon%, %lat%), 4326) 
    LIMIT 1
    ) 
AND (e.source = v.id OR e.target = v.id) 
GROUP BY v.id, v.the_geom
