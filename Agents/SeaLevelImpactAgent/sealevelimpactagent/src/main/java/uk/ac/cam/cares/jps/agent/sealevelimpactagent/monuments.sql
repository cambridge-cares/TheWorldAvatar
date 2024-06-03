WITH slr AS (
    SELECT geom, uuid
    FROM "sealevelprojections"
    WHERE uuid='7a1ad181-7dce-4871-b548-cfd3065a76b7'
    ORDER BY sealevelriseinmeters DESC
),
     monuments AS (
         SELECT ogc_fid, wkb_geometry
         FROM monuments
     )
SELECT slr.uuid as slr_uuid, monuments.ogc_fid as monuments_ogc_fid
FROM slr,monuments
WHERE ST_INTERSECTS(slr.geom, monuments.wkb_geometry)