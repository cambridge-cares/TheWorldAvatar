SET search_path TO public,citydb;
-- For each building, insert a new lod0_footprint_node, then link new node to building
WITH "fp_parent" AS (
    INSERT INTO "citydb"."surface_geometry" (
            "gmlid",
            "is_solid",
            "is_composite",
            "is_triangulated",
            "is_xlink",
            "is_reverse",
            "geometry",
            "cityobject_id"
        ) (SELECT "cityobject"."gmlid" || '_footprint',
                0,
                0,
                0,
                0,
                0,
                NULL,
                "building"."id"
            FROM "building" JOIN "cityobject" ON "building"."id" = "cityobject"."id" WHERE "building"."lod0_footprint_id" IS NULL)
    RETURNING "id" AS "footprint_id", "cityobject_id" AS "bid","gmlid"
),
"anon" AS (UPDATE "citydb"."building"
SET "lod0_footprint_id" = "fp_parent"."footprint_id" FROM "fp_parent" WHERE "building"."id" = "fp_parent"."bid" AND "building"."lod0_footprint_id" IS NULL),
"fg" AS (SELECT "bid","footprint_id","gmlid",("dumpling").geom AS "geometry"
FROM (SELECT "building_id",public.ST_Dump(public.ST_Union("geometry")) AS "dumpling" FROM "surface_geometry"
        JOIN "thematic_surface" ON "surface_geometry"."root_id" = "thematic_surface"."lod2_multi_surface_id"
        WHERE "geometry" IS NOT NULL AND "objectclass_id" = 35
        GROUP BY "building_id") AS "t1" JOIN "fp_parent" ON "fp_parent"."bid" = "t1"."building_id")
-- add footprint geometry
INSERT INTO "citydb"."surface_geometry" (
        "gmlid",
        "parent_id",
        "root_id",
        "is_solid",
        "is_composite",
        "is_triangulated",
        "is_xlink",
        "is_reverse",
        "geometry",
        "cityobject_id"
    ) (
        SELECT "fg"."gmlid" || '_child',
            "fg"."footprint_id",
            "fg"."footprint_id",
            0,
            0,
            0,
            0,
            0,
            "fg"."geometry",
            "fg"."bid"
        FROM "fg");
-- make parents self-aware
UPDATE "surface_geometry"
SET "root_id" = "id" WHERE "root_id" IS NULL;