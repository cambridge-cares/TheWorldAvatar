SET search_path TO public,
    citydb;
DROP TABLE IF EXISTS "public"."building_without_footprint_CityDB";
DROP TABLE IF EXISTS "public"."precise_footprint_CityDB";
DROP TABLE IF EXISTS "public"."rough_footprint_CityDB";
DROP TABLE IF EXISTS "public"."footprint_soup_CityDB";
CREATE TABLE "public"."building_without_footprint_CityDB" AS (
    SELECT "building"."id",
        "cityobject"."gmlid",
        public.ST_Zmin("cityobject"."envelope") AS "zmin"
    FROM "building"
        JOIN "cityobject" ON "building"."id" = "cityobject"."id"
    WHERE "building"."lod0_footprint_id" IS NULL
);
CREATE TABLE "public"."precise_footprint_CityDB" AS (
    SELECT "building_id",
        public.ST_UnaryUnion(
            public.ST_Collect(
                public.ST_MakeValid(public.ST_Force2D("geometry"))
            ),
            0.00000001
        ) AS "U"
    FROM "surface_geometry"
        JOIN "thematic_surface" ON "surface_geometry"."root_id" = COALESCE(
            "thematic_surface"."lod4_multi_surface_id",
            "thematic_surface"."lod3_multi_surface_id",
            "thematic_surface"."lod2_multi_surface_id"
        )
        JOIN "public"."building_without_footprint_CityDB" ON "building_without_footprint_CityDB"."id" = "thematic_surface"."building_id"
    WHERE "geometry" IS NOT NULL
        AND "objectclass_id" = 35
    GROUP BY "building_id"
);
CREATE TABLE "public"."rough_footprint_CityDB" AS (
    SELECT COALESCE(
            "building"."id",
            "thematic_surface"."building_id"
        ) AS "building_id",
        public.ST_UnaryUnion(
            public.ST_Collect(
                public.ST_MakeValid(public.ST_Force2D("geometry"))
            ),
            0.00000001
        ) AS "U"
    FROM "surface_geometry"
        FULL JOIN "building" ON "surface_geometry"."root_id" = COALESCE(
            "building"."lod4_multi_surface_id",
            "building"."lod3_multi_surface_id",
            "building"."lod2_multi_surface_id",
            "building"."lod1_multi_surface_id",
            "building"."lod4_solid_id",
            "building"."lod3_solid_id",
            "building"."lod2_solid_id",
            "building"."lod1_solid_id"
        )
        FULL JOIN "thematic_surface" ON "surface_geometry"."root_id" = COALESCE(
            "thematic_surface"."lod4_multi_surface_id",
            "thematic_surface"."lod3_multi_surface_id",
            "thematic_surface"."lod2_multi_surface_id"
        )
    WHERE "geometry" IS NOT NULL
        AND public.ST_Area("geometry") > 0.0
        AND (
            COALESCE(
                "building"."id",
                "thematic_surface"."building_id"
            ) NOT IN (
                SELECT "building_id"
                FROM "public"."precise_footprint_CityDB"
            )
        )
    GROUP BY "building"."id",
        "thematic_surface"."building_id"
);
CREATE TABLE "public"."footprint_soup_CityDB" AS (
    SELECT "id",
        "gmlid",
        public.ST_Force3D("geometry", "zmin") AS "geometry"
    FROM (
            SELECT "id",
                "gmlid",
                ("dumpling").geom AS "geometry",
                "zmin"
            FROM (
                    SELECT "id",
                        "gmlid",
                        public.ST_Dump(
                            COALESCE(
                                "precise_footprint_CityDB"."U",
                                "rough_footprint_CityDB"."U"
                            )
                        ) AS "dumpling",
                        "zmin"
                    FROM "building_without_footprint_CityDB"
                        LEFT JOIN "precise_footprint_CityDB" ON "building_without_footprint_CityDB"."id" = "precise_footprint_CityDB"."building_id"
                        LEFT JOIN "rough_footprint_CityDB" ON "building_without_footprint_CityDB"."id" = "rough_footprint_CityDB"."building_id"
                ) AS "all_footprint"
        ) AS "expanded"
    WHERE public.GeometryType("geometry") = 'POLYGON'
);
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
        ) (
            SELECT "building_without_footprint_CityDB"."gmlid" || '_footprint',
                0,
                0,
                0,
                0,
                0,
                NULL,
                "building_without_footprint_CityDB"."id"
            FROM "building_without_footprint_CityDB"
        )
    RETURNING "id" AS "footprint_id",
        "cityobject_id" AS "bid",
        "gmlid"
),
"anon" AS (
    UPDATE "citydb"."building"
    SET "lod0_footprint_id" = "fp_parent"."footprint_id"
    FROM "fp_parent"
    WHERE "building"."id" = "fp_parent"."bid"
        AND "building"."lod0_footprint_id" IS NULL
    RETURNING "id",
        "lod0_footprint_id"
)
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
        SELECT "footprint_soup_CityDB"."gmlid" || '_footprint_child',
            "fp_parent"."footprint_id",
            "fp_parent"."footprint_id",
            0,
            0,
            0,
            0,
            0,
            "footprint_soup_CityDB"."geometry",
            "footprint_soup_CityDB"."id"
        FROM "footprint_soup_CityDB"
            JOIN "fp_parent" ON "footprint_soup_CityDB"."id" = "fp_parent"."bid"
    );
UPDATE "surface_geometry"
SET "root_id" = "id"
WHERE "root_id" IS NULL;
DROP TABLE IF EXISTS "public"."building_without_footprint_CityDB";
DROP TABLE IF EXISTS "public"."precise_footprint_CityDB";
DROP TABLE IF EXISTS "public"."rough_footprint_CityDB";
DROP TABLE IF EXISTS "public"."footprint_soup_CityDB";