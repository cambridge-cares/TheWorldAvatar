--Dump DEM raster into polygons
CREATE MATERIALIZED VIEW dem_polygon AS
SELECT
    (ST_DumpAsPolygons(rast, 1, true)).*
FROM
    public.dem;

-- 
UPDATE sealevelprojections
SET geom = subquery.geom_union
FROM (
    SELECT
        sealevelprojections.ogc_fid,
        ST_Union(ST_Transform(dem_polygon.geom, 4326)) AS geom_union
    FROM
        dem_polygon,
        sealevelprojections
    WHERE
        dem_polygon.val <= sealevelprojections.sealevelriseinmeters
    GROUP BY
        sealevelprojections.ogc_fid
) AS subquery
WHERE sealevelprojections.ogc_fid = subquery.ogc_fid;