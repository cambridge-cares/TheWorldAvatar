SELECT
    e.the_geom AS geom
FROM
    pgr_dijkstra(
        'SELECT id, source, target, cost_s as cost, reverse_cost_s as reverse_cost FROM flood_cost_90cm',
        %source%,
        %target%,
        false
    ) AS r,
    routing_ways AS e
WHERE
    r.edge = e.gid