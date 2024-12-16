SELECT w.name, w.length_m, c.tag_value AS road_type, w.oneway, w.maxspeed_forward, w.maxspeed_backward,  w.the_geom
FROM routing_ways w
JOIN configuration c ON w.tag_id = c.tag_id