SET
    search_path TO public,
    citydb;

SET
    session_replication_role = replica;

-- copy all surfaces with geometry that should be identified (belong to building, but not footprint/roofprint/solid) and identified them into a temporary table
DROP TABLE IF EXISTS "public"."true_surface_CityDB";

CREATE SEQUENCE true_surface_seq;

ALTER SEQUENCE true_surface_seq RESTART WITH {seqStart};

CREATE TABLE "public"."true_surface_CityDB" AS (
    SELECT
        0 AS rownum,
        sg."id",
        sg."gmlid",
        NULL AS "envelope",
        sg."parent_id",
        sg."root_id",
        sg."cityobject_id",
        sg."geometry",
        public.ST_Orientation(sg.geometry) AS "orientation",
        public.ST_Area(sg.geometry) AS "a2d",
        public.ST_3DArea(public.ST_MakeValid(sg.geometry)) AS "a3d",
        0 AS "class"
    FROM
        "surface_geometry" AS sg
        JOIN "building" AS b ON sg."cityobject_id" = b."id"
    WHERE
        sg."geometry" IS NOT NULL
        AND (
            sg."root_id" = b."lod2_multi_surface_id"
            OR sg."root_id" = b."lod3_multi_surface_id"
        )
);

UPDATE
    "public"."true_surface_CityDB"
SET
    "envelope" = box2envelope(Box3D("geometry"));

UPDATE
    "public"."true_surface_CityDB"
SET
    "rownum" = nextval('true_surface_seq');

UPDATE
    "public"."true_surface_CityDB"
SET
    "class" = CASE
        WHEN "a3d" != 0
        AND "orientation" = 1
        AND "a2d" / "a3d" > {critAreaRatio} THEN 35
        ELSE CASE
            WHEN "a3d" != 0
            AND "orientation" = -1
            AND "a2d" / "a3d" > {critAreaRatio} THEN 33
            ELSE 34
        END
    END;

ALTER TABLE
    "public"."true_surface_CityDB"
ADD
    PRIMARY KEY (id);

-- remove their parent and root from building
UPDATE
    "citydb"."building"
SET
    "lod2_multi_surface_id" = NULL
WHERE
    (
        "lod2_multi_surface_id" IN (
            SELECT
                DISTINCT("parent_id")
            FROM
                "public"."true_surface_CityDB"
        )
    )
    OR (
        "lod2_multi_surface_id" IN (
            SELECT
                DISTINCT("root_id")
            FROM
                "public"."true_surface_CityDB"
        )
    );

UPDATE
    "citydb"."building"
SET
    "lod3_multi_surface_id" = NULL
WHERE
    (
        "lod3_multi_surface_id" IN (
            SELECT
                DISTINCT("parent_id")
            FROM
                "public"."true_surface_CityDB"
        )
    )
    OR (
        "lod3_multi_surface_id" IN (
            SELECT
                DISTINCT("root_id")
            FROM
                "public"."true_surface_CityDB"
        )
    );

-- unlink with parent and root in surface_geometry
UPDATE
    "citydb"."surface_geometry"
SET
    "parent_id" = NULL,
    "root_id" = NULL
FROM
    "public"."true_surface_CityDB"
WHERE
    "citydb"."surface_geometry"."id" = "public"."true_surface_CityDB"."id";

-- their parent, and root are no longer needed
DELETE FROM
    "citydb"."surface_geometry"
WHERE
    (
        "id" IN (
            SELECT
                DISTINCT("parent_id")
            FROM
                "public"."true_surface_CityDB"
        )
    )
    OR (
        "id" IN (
            SELECT
                DISTINCT("root_id")
            FROM
                "public"."true_surface_CityDB"
        )
    );

-- create city objects, update cityobject_id of existing surface geometries
INSERT INTO
    "citydb"."cityobject" (
        "objectclass_id",
        "gmlid",
        "envelope",
        "creation_date",
        "last_modification_date",
        "updating_person",
        "lineage"
    ) (
        SELECT
            "class",
            "true_surface_CityDB"."gmlid" || '_cityobject',
            "true_surface_CityDB"."envelope",
            CURRENT_TIMESTAMP,
            CURRENT_TIMESTAMP,
            user,
            "cityobject"."lineage"
        FROM
            "true_surface_CityDB"
            JOIN "cityobject" ON "true_surface_CityDB"."cityobject_id" = "cityobject"."id"
    );

UPDATE
    "surface_geometry"
SET
    "cityobject_id" = "true_surface_CityDB"."rownum"
FROM
    "true_surface_CityDB"
WHERE
    "surface_geometry"."id" = "true_surface_CityDB"."id";

-- create parents in surface_geometry (has no geometry)
INSERT INTO
    "citydb"."surface_geometry" (
        "gmlid",
        "is_solid",
        "is_composite",
        "is_triangulated",
        "is_xlink",
        "is_reverse",
        "geometry",
        "cityobject_id"
    ) (
        SELECT
            "surface_geometry"."gmlid" || '_parent',
            0,
            0,
            0,
            0,
            0,
            NULL,
            "surface_geometry"."cityobject_id"
        FROM
            "surface_geometry",
            "public"."true_surface_CityDB"
        WHERE
            "surface_geometry"."id" = "public"."true_surface_CityDB"."id"
    );

-- let children know their parents
UPDATE
    "surface_geometry" AS sg
SET
    "parent_id" = ia."intermediate_id",
    "root_id" = ia."intermediate_id"
FROM
    (
        SELECT
            "cityobject_id",
            "a"."id" AS "actual_id",
            "i"."id" AS "intermediate_id"
        FROM
            (
                SELECT
                    *
                FROM
                    "surface_geometry"
                WHERE
                    "geometry" IS NULL
            ) AS i
            JOIN (
                SELECT
                    *
                FROM
                    "surface_geometry"
                WHERE
                    "geometry" IS NOT NULL
            ) AS a USING ("cityobject_id")
    ) AS ia
WHERE
    sg."id" = ia."actual_id"
    AND (
        (sg."parent_id" IS NULL)
        OR (sg."root_id" IS NULL)
    );

-- make parents self-aware
UPDATE
    "surface_geometry"
SET
    "root_id" = "id"
WHERE
    "root_id" IS NULL;

-- update thematic surface
INSERT INTO
    "thematic_surface" (
        "id",
        "objectclass_id",
        "building_id",
        "lod2_multi_surface_id"
    ) (
        SELECT
            "surface_geometry"."cityobject_id",
            "true_surface_CityDB"."class",
            "true_surface_CityDB"."cityobject_id",
            "surface_geometry"."root_id"
        FROM
            "true_surface_CityDB"
            JOIN "surface_geometry" ON "surface_geometry"."id" = "true_surface_CityDB"."id"
    );

DROP TABLE IF EXISTS "public"."true_surface_CityDB";

DROP SEQUENCE true_surface_seq;

-- check if the average lowest point of roof surfaces is above the average highest point of ground surfaces
-- if this is not satisfied by half of the buildings, then swap grounds and roofs around
UPDATE
    "thematic_surface"
SET
    "objectclass_id" = CASE
        WHEN (
            "objectclass_id" = 33
            and "condition"."correct_percentage" < 0.5
        ) THEN 35
        ELSE CASE
            WHEN (
                "objectclass_id" = 35
                and "condition"."correct_percentage" < 0.5
            ) THEN 33
            ELSE "objectclass_id"
        END
    END
FROM
    (
        SELECT
            SUM("correct?" :: int) :: real / COUNT(*) AS "correct_percentage"
        FROM
            (
                SELECT
                    "zg"."building_id",
                    "avg_zmin_roof" >= "avg_zmax_ground" AS "correct?"
                FROM
                    (
                        SELECT
                            "building_id",
                            Avg(public.ST_ZMax("geometry")) AS "avg_zmax_ground"
                        FROM
                            "thematic_surface"
                            JOIN "building" ON "thematic_surface"."building_id" = "building"."id"
                            JOIN "surface_geometry" ON "surface_geometry"."parent_id" = "thematic_surface"."lod2_multi_surface_id"
                        WHERE
                            "thematic_surface"."objectclass_id" = '35'
                        GROUP BY
                            "building_id"
                    ) AS "zg"
                    JOIN (
                        SELECT
                            "building_id",
                            Avg(public.ST_ZMin("geometry")) AS "avg_zmin_roof"
                        FROM
                            "thematic_surface"
                            JOIN "building" ON "thematic_surface"."building_id" = "building"."id"
                            JOIN "surface_geometry" ON "surface_geometry"."parent_id" = "thematic_surface"."lod2_multi_surface_id"
                        WHERE
                            "thematic_surface"."objectclass_id" = '33'
                        GROUP BY
                            "building_id"
                    ) AS "zr" ON "zg"."building_id" = "zr"."building_id"
            ) AS "zz"
    ) AS "condition";

SET
    session_replication_role = DEFAULT;