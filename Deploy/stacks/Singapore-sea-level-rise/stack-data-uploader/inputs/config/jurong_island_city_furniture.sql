CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

ALTER TABLE jurong_island_city_furniture
    DROP COLUMN IF EXISTS "company_uuid",
    DROP COLUMN IF EXISTS "city_furniture_uuid";

ALTER TABLE jurong_island_city_furniture 
    ADD COLUMN company_uuid TEXT,
    ADD COLUMN "city_furniture_uuid" TEXT;

WITH cityfurniture_footprint AS (
    SELECT cityobject_genericattrib.cityobject_id,
    cityobject_genericattrib.geomval
    FROM cityobject_genericattrib
    WHERE ((cityobject_genericattrib.attrname)::text = 'footprint'::text)
)

UPDATE jurong_island_city_furniture 
SET "city_furniture_uuid" = match.uuid::uuid
FROM (
    SELECT ogc_fid, uuid, dist 
    FROM (
        SELECT ogc_fid, public.ST_TRANSFORM(wkb_geometry, 24500) AS geometry 
        FROM jurong_island_city_furniture
    ) r1
    LEFT JOIN LATERAL (
        SELECT strval AS uuid, r1.geometry <-> cityfurniture_footprint.geomval AS dist
        FROM cityfurniture_footprint, citydb.cityobject_genericattrib
        WHERE citydb.cityobject_genericattrib.cityobject_id = cityfurniture_footprint.cityobject_id
        AND citydb.cityobject_genericattrib.attrname = 'uuid'
        ORDER BY dist
        LIMIT 1
    ) r2 ON true
) match
WHERE jurong_island_city_furniture.ogc_fid = match.ogc_fid 
AND match.dist < 250;

UPDATE jurong_island_city_furniture 
SET company_uuid = uuid_generate_v5('2b448aee-4e3a-4952-be44-3136508f9d64', company);
