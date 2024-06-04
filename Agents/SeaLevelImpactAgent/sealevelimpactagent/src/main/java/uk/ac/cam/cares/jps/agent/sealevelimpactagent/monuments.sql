INSERT INTO slr_monuments (slr_uuid, monuments_uuid)
WITH slr AS (
    SELECT geom, uuid
    FROM "sealevelprojections"
    WHERE uuid='4eae3ab2-6980-4028-aa2a-64076890d4c7'
),
     monuments AS (
         SELECT ogc_fid, wkb_geometry
         FROM monuments
     )
SELECT slr.uuid as slr_uuid, monuments.uuid as monuments_uuid
FROM slr,monuments
WHERE ST_INTERSECTS(slr.geom, monuments.wkb_geometry)