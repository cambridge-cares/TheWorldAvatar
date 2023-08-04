CREATE TABLE trips_centrality AS (
SELECT
  b.gid,
  b.the_geom AS geom,
  count(b.the_geom) AS count
FROM
	routing_ways AS t,
  pgr_dijkstra(
      'SELECT
          g.gid AS id,
          g.source,
          g.target,
          g.cost_s AS cost
          FROM routing_ways AS g WHERE
                g.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)', 
          7536, t.target,
          directed := FALSE) AS j
JOIN routing_ways AS b 
  ON j.edge = b.gid 
GROUP BY b.gid, b.the_geom
); 