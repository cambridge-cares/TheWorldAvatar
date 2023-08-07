CREATE OR REPLACE VIEW flood_polygon AS
SELECT (ST_DumpAsPolygons(rast, 1, true)).*
FROM public.flood;

CREATE TABLE flood_polygon_single AS
(
    SELECT ST_UNION(geom_selected) AS geom
    FROM
    (
        SELECT ST_Transform(geom, 4326) AS geom_selected
        FROM flood_polygon
        WHERE val >= 0.3
    ) AS subquery
);

CREATE OR REPLACE VIEW flood_cost AS
SELECT rw.gid AS id,
rw.tag_id as tag_id,
    rw.source,
    rw.target,
        CASE
            WHEN (EXISTS ( SELECT 1
               FROM flood_polygon_single
              WHERE st_intersects(rw.the_geom, flood_polygon_single.geom))) THEN (- abs(rw.cost_s))
            ELSE rw.cost_s
        END AS cost_s,
        CASE
            WHEN (EXISTS ( SELECT 1
               FROM flood_polygon_single
              WHERE st_intersects(rw.the_geom, flood_polygon_single.geom))) THEN (- abs(rw.reverse_cost_s))
            ELSE rw.reverse_cost_s
        END AS reverse_cost_s
   FROM routing_ways rw;

