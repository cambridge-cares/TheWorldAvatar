

-- This returns the most used roads
-- CREATE VIEW trips_centrality AS (
-- SELECT
--   b.gid,
--   b.name,
--   b.the_geom AS geom,
--   count(b.the_geom) AS count
-- FROM
-- 	routing_ways AS t,
--   pgr_dijkstra(
--       'SELECT
--           g.gid AS id,
--           g.source,z
--           g.target,
--           g.cost_s AS cost
--           FROM routing_ways AS g', 
--           7536, t.target,
--           directed := FALSE) AS j
-- JOIN routing_ways AS b 
--   ON j.edge = b.gid 
-- GROUP BY b.gid, b.the_geom
-- ); 


--Create Function
CREATE OR REPLACE FUNCTION get_nearest_vertex(lon numeric, lat numeric) RETURNS TABLE (id bigint) AS $$
BEGIN
    RETURN QUERY
    SELECT
      v.id
    FROM
      routing_ways_vertices_pgr AS v,
      routing_ways AS e
    WHERE
      v.id = (SELECT
                routing_ways_vertices_pgr.id
              FROM routing_ways_vertices_pgr
              ORDER BY routing_ways_vertices_pgr.the_geom <-> ST_SetSRID(ST_MakePoint(get_nearest_vertex.lon, get_nearest_vertex.lat), 4326) LIMIT 1)
      AND (e.source = v.id OR e.target = v.id)
    GROUP BY v.id, v.the_geom;
END;
$$ LANGUAGE plpgsql;


--Isochrone from Hospital Unflooded--
CREATE OR REPLACE VIEW isochrone_hospital_unflooded AS 
SELECT
    minute_limit * 2 AS minute,
    ST_OptimalAlphaShape(ST_Collect(subquery.the_geom)) AS isochrone_polygon
FROM
    get_nearest_vertex(0.44596810988223834, 52.75684401611818) as hospital_id,
    generate_series(1, 5) AS minute_limit
    
CROSS JOIN LATERAL (
    SELECT
        id,
        the_geom
    FROM
        pgr_drivingDistance(
            'SELECT gid as id, source, target, cost_s as cost FROM routing_ways',
            hospital_id,
            minute_limit * 120,
            false
        ) AS dd
    JOIN
        routing_ways_vertices_pgr AS v ON dd.node = v.id
) AS subquery
GROUP BY minute_limit
ORDER BY minute_limit;


--Isochrone from Hospital Flooded--
CREATE OR REPLACE VIEW isochrone_hospital_flooded AS 
SELECT
    minute_limit * 2 AS minute,
    ST_OptimalAlphaShape(ST_Collect(subquery.the_geom)) AS isochrone_polygon
FROM
    get_nearest_vertex(0.44596810988223834, 52.75684401611818) as hospital_id,
    generate_series(1, 5) AS minute_limit
    
CROSS JOIN LATERAL (
    SELECT
        id,
        the_geom
    FROM
        pgr_drivingDistance(
            'SELECT id, source, target, cost_s as cost, reverse_cost_s as reverse_cost FROM flood_cost_10cm',
            hospital_id,
            minute_limit * 120,
            false
        ) AS dd
    JOIN
        routing_ways_vertices_pgr AS v ON dd.node = v.id
) AS subquery
GROUP BY minute_limit
ORDER BY minute_limit;

--Unreachable population--
CREATE OR REPLACE VIEW unreachable_population AS
SELECT (
    SELECT ST_Difference(u.isochrone_polygon, i.isochrone_polygon) as geom
    FROM isochrone_hospital_flooded AS i, isochrone_hospital_unflooded AS u
    WHERE i.minute = 8 AND u.minute = 8
),ROUND(SUM((ST_SummaryStats(ST_Clip(population.rast, ST_Transform((
    SELECT ST_Difference(u.isochrone_polygon, i.isochrone_polygon) as geom
    FROM isochrone_hospital_flooded AS i, isochrone_hospital_unflooded AS u
    WHERE i.minute = 8 AND u.minute = 8
), ST_SRID(population.rast)), TRUE))).sum)) AS "Unreachable Population In Less than 8 Minutes"
FROM population