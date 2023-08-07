WITH "toid_table" AS (SELECT "strval" AS "rawID", "cityobject_id" FROM "cityobject_genericattrib" WHERE "attrname"='rawID'),
"uuid_table" AS (SELECT "strval" AS "uuid", "cityobject_id" FROM "cityobject_genericattrib" WHERE "attrname"='uuid'),
"iri_table" AS (SELECT "urival" AS "iri", "cityobject_id" FROM "cityobject_genericattrib" WHERE "attrname"='iri')
SELECT "building_id","measured_height" AS "building_height",ST_FlipCoordinates("geometry") AS geometry,"rawID","uuid","iri" FROM
"thematic_surface" JOIN "building" ON "thematic_surface"."building_id"="building"."id"
JOIN "surface_geometry" ON "surface_geometry"."parent_id"="thematic_surface"."lod2_multi_surface_id"
JOIN "toid_table" ON "building"."id"="toid_table"."cityobject_id"
JOIN "uuid_table" ON "building"."id"="uuid_table"."cityobject_id"
JOIN "iri_table" ON "building"."id"="iri_table"."cityobject_id"
WHERE "thematic_surface"."objectclass_id" = '35'