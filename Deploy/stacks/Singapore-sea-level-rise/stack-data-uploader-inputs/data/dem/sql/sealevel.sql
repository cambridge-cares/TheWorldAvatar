CREATE OR REPLACE VIEW dem_polygon AS
SELECT
    (ST_DumpAsPolygons(rast, 1, true)).*
FROM
    public.dem;

CREATE TABLE IF NOT EXISTS sealevel_rise_5m AS (
    SELECT
        ST_UNION(geom_selected) AS geom
    FROM
        (
            SELECT
                ST_Transform(geom, 4326) AS geom_selected
            FROM
                dem_polygon
            WHERE
                val <= 5.0
        ) AS subquery
);