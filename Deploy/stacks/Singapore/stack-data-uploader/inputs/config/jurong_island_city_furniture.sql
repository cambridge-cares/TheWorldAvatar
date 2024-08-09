CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table jurong_island_city_furniture
drop column if exists "company_uuid",
drop column if exists "city_furniture_uuid";
alter table jurong_island_city_furniture 
add column company_uuid UUID ,
add column "city_furniture_uuid" UUID ;
Update jurong_island_city_furniture set "city_furniture_uuid" = match.uuid::uuid from (
SELECT ogc_fid, uuid, dist FROM
(SELECT ogc_fid, public.ST_TRANSFORM(wkb_geometry, 24500) AS geometry FROM jurong_island_city_furniture) r1
LEFT JOIN LATERAL (
Select strval AS uuid, r1.geometry <-> citydb.cityfurniture_footprint.geomval AS dist
from citydb.cityfurniture_footprint, citydb.cityobject_genericattrib
where citydb.cityobject_genericattrib.cityobject_id = citydb.cityfurniture_footprint.cityobject_id
and citydb.cityobject_genericattrib.attrname = 'uuid'
order by dist
limit 1
) r2 on true
) match
where jurong_island_city_furniture.ogc_fid = match.ogc_fid and match.dist < 250;
UPDATE jurong_island_city_furniture 
set company_uuid = uuid_generate_v5('2b448aee-4e3a-4952-be44-3136508f9d64', company);
