SET search_path TO public,citydb;
UPDATE "building"
SET "measured_height" = ST_ZMax(Box3D("cityobject"."envelope")) - ST_ZMin(Box3D("cityobject"."envelope"))
FROM "cityobject" WHERE
"building"."id" = "cityobject"."id" AND "building"."measured_height" IS NULL;