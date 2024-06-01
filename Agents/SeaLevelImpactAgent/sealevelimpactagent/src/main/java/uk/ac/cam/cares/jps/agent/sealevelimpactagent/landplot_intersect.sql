WITH slr AS (
    SELECT geom
    FROM "sealevelprojections"
    WHERE uuid='7a1ad181-7dce-4871-b548-cfd3065a76b7'
    ORDER BY sealevelriseinmeters DESC
),
     landplot AS (
         SELECT ogc_fid, "lod1Geometry"
         FROM landplot
     )
SELECT landplot.ogc_fid
FROM slr,landplot
WHERE ST_INTERSECTS(slr.geom, landplot."lod1Geometry")