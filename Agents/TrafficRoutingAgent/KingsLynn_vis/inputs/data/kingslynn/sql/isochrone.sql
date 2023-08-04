SELECT
    minute_limit * 2 AS minute,
    ST_ConvexHull(ST_Collect(subquery.the_geom)) AS isochrone_polygon
FROM
    generate_series(1, 5) AS minute_limit
CROSS JOIN LATERAL (
    SELECT
        id,
        the_geom
    FROM
        pgr_drivingDistance(
            'SELECT gid as id, source, target, cost_s as cost FROM routing_ways WHERE routing_ways.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)',
            7536,
            minute_limit * 120,
            false
        ) AS dd
    JOIN
        routing_ways_vertices_pgr AS v ON dd.node = v.id
) AS subquery
GROUP BY minute_limit