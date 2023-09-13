SET search_path TO public,citydb;
-- copy all surfaces with geometry that should be identified (belong to building, but not footprint/roofprint/solid) and identified them into a temporary table
DROP TABLE IF EXISTS "public"."true_surface_1d9b061a";
CREATE TABLE "public"."true_surface_1d9b061a" AS (
SELECT "id","gmlid",box2envelope(Box3D("geometry")) AS "envelope", "parent_id", "root_id", "cityobject_id", CASE
    WHEN public.ST_3DArea(public.ST_MakeValid(geometry)) != 0 AND public.ST_Orientation(geometry) = 1 AND public.ST_Area(geometry)/public.ST_3DArea(public.ST_MakeValid(geometry))>0.8 THEN 35
    ELSE CASE 
        WHEN public.ST_3DArea(public.ST_MakeValid(geometry)) != 0 AND public.ST_Orientation(geometry) = -1 AND public.ST_Area(geometry)/public.ST_3DArea(public.ST_MakeValid(geometry))>0.8 THEN 33
        ELSE 34 END
END AS "class"
FROM "surface_geometry"
WHERE "geometry" IS NOT NULL AND "cityobject_id" IN (SELECT "id" FROM "building") AND 
(("root_id" IN (SELECT "lod2_multi_surface_id" FROM "building")) OR ("root_id" IN (SELECT "lod3_multi_surface_id" FROM "building")))
);
-- remove their parent and root from building
UPDATE "citydb"."building"
SET "lod2_multi_surface_id" = NULL WHERE 
("lod2_multi_surface_id" IN (SELECT DISTINCT("parent_id") FROM "public"."true_surface_1d9b061a")) OR ("lod2_multi_surface_id" IN (SELECT DISTINCT("root_id") FROM "public"."true_surface_1d9b061a"));
UPDATE "citydb"."building"
SET "lod3_multi_surface_id" = NULL WHERE 
("lod3_multi_surface_id" IN (SELECT DISTINCT("parent_id") FROM "public"."true_surface_1d9b061a")) OR ("lod3_multi_surface_id" IN (SELECT DISTINCT("root_id") FROM "public"."true_surface_1d9b061a"));
-- unlink with parent and root in surface_geometry
UPDATE "citydb"."surface_geometry"
SET "parent_id" = NULL, "root_id" = NULL FROM "public"."true_surface_1d9b061a"
WHERE "citydb"."surface_geometry"."id" = "public"."true_surface_1d9b061a"."id";
-- their parent, and root are no longer needed
DELETE FROM "citydb"."surface_geometry" WHERE
("id" IN (SELECT DISTINCT("parent_id") FROM "public"."true_surface_1d9b061a")) OR ("id" IN (SELECT DISTINCT("root_id") FROM "public"."true_surface_1d9b061a"));
-- create city objects, update cityobject_id of existing surface geometries
WITH "true_surface_with_row_number" AS (
            SELECT row_number() OVER (), "id", "class",
                "gmlid" AS "surface_gmlid",
                "envelope" AS "surface_envelope","cityobject_id" AS "bid"
            FROM "public"."true_surface_1d9b061a"
        ),
"co" AS (
    INSERT INTO "citydb"."cityobject" (
            "objectclass_id",
            "gmlid",
            "envelope",
            "creation_date",
            "last_modification_date",
            "updating_person",
			"lineage"
        ) (SELECT "class",
            "surface_gmlid" || '_cityobject',
            "surface_envelope",
            CURRENT_TIMESTAMP,
            CURRENT_TIMESTAMP,
            user,
			"cityobject"."lineage"
            FROM "true_surface_with_row_number" JOIN "cityobject" ON "true_surface_with_row_number"."bid" = "cityobject"."id")
    RETURNING "gmlid",
        "id" AS "coid"
),
"id_to_coid" AS (
SELECT "true_surface_with_row_number"."id", "t2"."coid"
FROM "true_surface_with_row_number" JOIN (SELECT row_number() OVER (), * FROM "co") AS "t2" USING (row_number)
)
UPDATE "surface_geometry"
SET "cityobject_id"="id_to_coid"."coid" FROM "id_to_coid" WHERE
"surface_geometry"."id" = "id_to_coid"."id";
-- create parents in surface_geometry (has no geometry)
INSERT INTO "citydb"."surface_geometry" (
            "gmlid",
            "is_solid",
            "is_composite",
            "is_triangulated",
            "is_xlink",
            "is_reverse",
            "geometry",
            "cityobject_id")
            (
            SELECT "surface_geometry"."gmlid" || '_parent',
                0,
                0,
                0,
                0,
                0,
                NULL,
                "surface_geometry"."cityobject_id"
                FROM "surface_geometry", "public"."true_surface_1d9b061a" WHERE
                "surface_geometry"."id" = "public"."true_surface_1d9b061a"."id"
            );
-- let children know their parents
WITH "intermediate" AS (SELECT * FROM "surface_geometry" WHERE "geometry" IS NULL),
     "actual" AS (SELECT * FROM "surface_geometry" WHERE "geometry" IS NOT NULL),
"intermediate_actual" AS (
SELECT "cityobject_id","actual"."id" AS "actual_id","intermediate"."id" AS "intermediate_id"
FROM "intermediate" JOIN "actual" using ("cityobject_id"))
UPDATE "surface_geometry"
SET "parent_id" = "intermediate_id", "root_id" = "intermediate_id" FROM "intermediate_actual"
WHERE "surface_geometry"."id" = "intermediate_actual"."actual_id" AND (("surface_geometry"."parent_id" IS NULL) OR ("surface_geometry"."root_id" IS NULL));
-- make parents self-aware
UPDATE "surface_geometry"
SET "root_id" = "id" WHERE "root_id" IS NULL;
-- update thematic surface
INSERT INTO "thematic_surface" ("id","objectclass_id","building_id","lod2_multi_surface_id")
(SELECT "surface_geometry"."cityobject_id","true_surface_1d9b061a"."class","true_surface_1d9b061a"."cityobject_id","surface_geometry"."root_id"
FROM "true_surface_1d9b061a" JOIN "surface_geometry" ON "surface_geometry"."id" = "true_surface_1d9b061a"."id");
-- check if the average lowest point of roof surfaces is above the average highest point of ground surfaces
-- if this is not satisfied by half of the buildings, then swap grounds and roofs around
UPDATE "thematic_surface"
SET "objectclass_id" = CASE WHEN ("objectclass_id" = 33 and "condition"."correct_percentage"<0.5) THEN 35
ELSE CASE WHEN ("objectclass_id" = 35 and "condition"."correct_percentage"<0.5) THEN 33
ELSE "objectclass_id" END END
FROM (SELECT SUM("correct?"::int)::real/COUNT(*) AS "correct_percentage" FROM
(SELECT "zg"."building_id","avg_zmin_roof">="avg_zmax_ground" AS "correct?" FROM (SELECT "building_id",Avg(public.ST_ZMax("geometry")) AS "avg_zmax_ground" FROM
"thematic_surface" JOIN "building" ON "thematic_surface"."building_id"="building"."id"
JOIN "surface_geometry" ON "surface_geometry"."parent_id"="thematic_surface"."lod2_multi_surface_id"
WHERE "thematic_surface"."objectclass_id" = '35' GROUP BY "building_id") AS "zg" JOIN
(SELECT "building_id",Avg(public.ST_ZMin("geometry")) AS "avg_zmin_roof" FROM
"thematic_surface" JOIN "building" ON "thematic_surface"."building_id"="building"."id"
JOIN "surface_geometry" ON "surface_geometry"."parent_id"="thematic_surface"."lod2_multi_surface_id"
WHERE "thematic_surface"."objectclass_id" = '33' GROUP BY "building_id") AS "zr" 
ON "zg"."building_id" = "zr"."building_id") AS "zz") AS "condition";