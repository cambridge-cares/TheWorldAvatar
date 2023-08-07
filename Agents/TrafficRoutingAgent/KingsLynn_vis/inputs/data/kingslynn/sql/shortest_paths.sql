SELECT
min(r.seq) AS seq,
e.gid AS id,
sum(e.cost) AS cost,
ST_Collect(e.the_geom) AS geom 
FROM pgr_dijkstra('SELECT gid as id, source, target, cost_s as cost, reverse_cost_s   as reverse_cost FROM routing_ways',%source%,%target%,false) AS r,routing_ways AS e WHERE r.edge=e.gid GROUP BY e.gid