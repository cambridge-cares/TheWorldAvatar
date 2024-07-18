Create EXTENSION IF NOT EXISTS postgis_sfcgal;

CREATE
OR REPLACE VIEW flood_polygon AS
SELECT
    (ST_DumpAsPolygons(rast, 1, true)).*
FROM
    public.flood;

--- Wading depth of a car set to 10cm
--- Wading depth of a range rover set to 90cm
--- Ambulance, Fire truck at 30cm
CREATE TABLE IF NOT EXISTS flood_polygon_single_10cm AS (
    SELECT
        ST_UNION(geom_selected) AS geom
    FROM
        (
            SELECT
                ST_Transform(geom, 4326) AS geom_selected
            FROM
                flood_polygon
            WHERE
                val >= 0.1
        ) AS subquery
);

CREATE TABLE IF NOT EXISTS flood_polygon_single_30cm AS (
    SELECT
        ST_UNION(geom_selected) AS geom
    FROM
        (
            SELECT
                ST_Transform(geom, 4326) AS geom_selected
            FROM
                flood_polygon
            WHERE
                val >= 0.3
        ) AS subquery
);

CREATE TABLE IF NOT EXISTS flood_polygon_single_90cm AS (
    SELECT
        ST_UNION(geom_selected) AS geom
    FROM
        (
            SELECT
                ST_Transform(geom, 4326) AS geom_selected
            FROM
                flood_polygon
            WHERE
                val >= 0.9
        ) AS subquery
);

CREATE
 MATERIALIZED VIEW IF NOT EXISTS flood_cost_10cm AS
SELECT
    rw.gid AS id,
    rw.tag_id as tag_id,
    rw.source,
    rw.target,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_10cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_10cm.geom)
            )
        ) THEN (- abs(rw.cost_s))
        ELSE rw.cost_s
    END AS cost_s,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_10cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_10cm.geom)
            )
        ) THEN (- abs(rw.reverse_cost_s))
        ELSE rw.reverse_cost_s
    END AS reverse_cost_s
FROM
    routing_ways rw;

CREATE
 MATERIALIZED VIEW IF NOT EXISTS flood_cost_30cm AS
SELECT
    rw.gid AS id,
    rw.tag_id as tag_id,
    rw.source,
    rw.target,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_30cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_30cm.geom)
            )
        ) THEN (- abs(rw.cost_s))
        ELSE rw.cost_s
    END AS cost_s,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_30cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_30cm.geom)
            )
        ) THEN (- abs(rw.reverse_cost_s))
        ELSE rw.reverse_cost_s
    END AS reverse_cost_s
FROM
    routing_ways rw;

CREATE
MATERIALIZED VIEW IF NOT EXISTS flood_cost_90cm AS
SELECT
    rw.gid AS id,
    rw.tag_id as tag_id,
    rw.source,
    rw.target,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_90cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_90cm.geom)
            )
        ) THEN (- abs(rw.cost_s))
        ELSE rw.cost_s
    END AS cost_s,
    CASE
        WHEN (
            EXISTS (
                SELECT
                    1
                FROM
                    flood_polygon_single_90cm
                WHERE
                    st_intersects(rw.the_geom, flood_polygon_single_90cm.geom)
            )
        ) THEN (- abs(rw.reverse_cost_s))
        ELSE rw.reverse_cost_s
    END AS reverse_cost_s
FROM
    routing_ways rw;