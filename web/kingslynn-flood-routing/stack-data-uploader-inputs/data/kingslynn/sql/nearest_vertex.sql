SELECT
  v.id,
  v.the_geom
FROM
  routing_ways_vertices_pgr AS v
WHERE
  v.id = (
    SELECT
      id
    FROM
      routing_ways_vertices_pgr
    ORDER BY
      the_geom <-> ST_SetSRID(ST_MakePoint(%lon%, %lat%), 4326)
    LIMIT
      1
  )
  AND EXISTS (
    SELECT
      1
    FROM
      routing_ways AS e
    WHERE
      e.source = v.id
      OR e.target = v.id
  )