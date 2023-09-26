SET search_path TO public,citydb;
-- Find buildings without footprints
-- use ground surfaces to build footprints if they exists
-- use any geometries that can be found to build approximate footprints
-- force all footprints to have the minimum Z
WITH "building_without_footprint" AS (SELECT "building"."id", "cityobject"."gmlid", public.ST_Zmin("cityobject"."envelope") as "zmin"  FROM "building" JOIN "cityobject" On "building"."id" = "cityobject"."id" WHERE "building"."lod0_footprint_id" IS NULL),
"precise_footprint" AS (SELECT "building_id",public.ST_UnaryUnion(public.ST_Collect(public.ST_MakeValid(public.ST_Force2D("geometry")))) AS "U" FROM "surface_geometry"
JOIN "thematic_surface" ON "surface_geometry"."root_id" = COALESCE("thematic_surface"."lod4_multi_surface_id","thematic_surface"."lod3_multi_surface_id","thematic_surface"."lod2_multi_surface_id")
WHERE "geometry" IS NOT NULL AND "objectclass_id" = 35 GROUP BY "building_id"),
"rough_footprint" AS (SELECT COALESCE("building"."id","thematic_surface"."building_id") AS "building_id",public.ST_UnaryUnion(public.ST_Collect(public.ST_MakeValid(public.ST_Force2D("geometry")))) AS "U" FROM "surface_geometry"
FULL JOIN "building" ON "surface_geometry"."root_id" = COALESCE("building"."lod4_multi_surface_id","building"."lod3_multi_surface_id","building"."lod2_multi_surface_id","building"."lod1_multi_surface_id","building"."lod4_solid_id","building"."lod3_solid_id","building"."lod2_solid_id","building"."lod1_solid_id")
FULL JOIN "thematic_surface" ON "surface_geometry"."root_id" = COALESCE("thematic_surface"."lod4_multi_surface_id","thematic_surface"."lod3_multi_surface_id","thematic_surface"."lod2_multi_surface_id")
WHERE "geometry" IS NOT NULL AND public.ST_Area("geometry")>0.0 GROUP BY "building"."id", "thematic_surface"."building_id"),
"footprint_soup" AS (SELECT "id","gmlid",public.ST_Force3D("geometry","zmin") AS "geometry" FROM
(SELECT "id","gmlid",("dumpling").geom AS "geometry", "zmin" FROM
(SELECT "id","gmlid",public.ST_Dump(COALESCE("precise_footprint"."U","rough_footprint"."U")) AS "dumpling", "zmin" FROM "building_without_footprint"
LEFT JOIN "precise_footprint" ON "building_without_footprint"."id" = "precise_footprint"."building_id"
LEFT JOIN "rough_footprint" ON "building_without_footprint"."id" = "rough_footprint"."building_id") AS "all_footprint") AS "expanded"),
"fp_parent" AS (
    INSERT INTO "citydb"."surface_geometry" (
            "gmlid",
            "is_solid",
            "is_composite",
            "is_triangulated",
            "is_xlink",
            "is_reverse",
            "geometry",
            "cityobject_id"
        ) (SELECT "building_without_footprint"."gmlid" || '_footprint',
                0,
                0,
                0,
                0,
                0,
                NULL,
                "building_without_footprint"."id"
            FROM "building_without_footprint")
    RETURNING "id" AS "footprint_id", "cityobject_id" AS "bid","gmlid"
),
"anon" AS (UPDATE "citydb"."building"
SET "lod0_footprint_id" = "fp_parent"."footprint_id" FROM "fp_parent" WHERE "building"."id" = "fp_parent"."bid" AND "building"."lod0_footprint_id" IS NULL
RETURNING "id","lod0_footprint_id")
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
        SELECT "footprint_soup"."gmlid" || '_footprint_child',
            "fp_parent"."footprint_id",
            "fp_parent"."footprint_id",
            0,
            0,
            0,
            0,
            0,
            "footprint_soup"."geometry",
            "footprint_soup"."id"
        FROM "footprint_soup" JOIN "fp_parent" ON "footprint_soup"."id" = "fp_parent"."bid" WHERE public.GeometryType("geometry") = 'POLYGON'
		);
UPDATE "surface_geometry"
SET "root_id" = "id" WHERE "root_id" IS NULL;