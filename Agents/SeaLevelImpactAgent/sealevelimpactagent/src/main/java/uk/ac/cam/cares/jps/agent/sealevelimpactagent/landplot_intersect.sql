INSERT INTO slr_landplot (slr_uuid, landplot_uuid, affectedarea)
WITH slr AS (
    SELECT uuid, geom
    FROM sealevelprojections
    WHERE uuid='4eae3ab2-6980-4028-aa2a-64076890d4c7'
    ORDER BY sealevelriseinmeters DESC
)
SELECT
    slr.uuid AS slr_uuid,
    lp.ogc_fid AS lp_uuid,
    ROUND(ST_AREA(ST_TRANSFORM(ST_INTERSECTION(slr.geom, ST_MAKEVALID(lp."lod1Geometry")), 3857))::numeric, 2) AS affectedarea
FROM slr
JOIN landplot lp ON ST_INTERSECTS(slr.geom, lp."lod1Geometry");