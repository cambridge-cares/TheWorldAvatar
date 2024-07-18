SELECT routing_ways.the_geom
FROM (
    SELECT ROW_NUMBER() OVER() as seq, tsp.node
    FROM tsp
) n1
JOIN (
    SELECT ROW_NUMBER() OVER() as seq, tsp.node
    FROM tsp
) n2 ON n1.seq + 1 = n2.seq
JOIN pgr_dijkstra(
    'SELECT id, source, target, cost_s as cost, reverse_cost_s as reverse_cost FROM flood_cost_10cm',
    n1.node,
    n2.node
) AS di ON true
JOIN routing_ways ON di.edge = routing_ways.gid
ORDER BY n1.seq