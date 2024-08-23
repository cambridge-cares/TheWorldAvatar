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
    'SELECT gid as id, source, target, cost, reverse_cost FROM routing_ways',
    n1.node,
    n2.node
) AS di ON true
JOIN routing_ways ON di.edge = routing_ways.gid
ORDER BY n1.seq