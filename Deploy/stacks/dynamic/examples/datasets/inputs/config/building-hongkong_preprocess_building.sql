SET search_path TO public,
    citydb;
--UPDATE "public"."raw_gdal" SET height = '0.1' WHERE height IS NULL;
CREATE TABLE "public"."raw_data" AS (
    SELECT parent,
        average_height,
        50.AS elevation,
        ST_MakeValid(geometry) AS geometry
    FROM (
            SELECT parent,
                avg(height) AS average_height,
                ST_Union(wkb_geometry) AS geometry
            FROM (
                    SELECT id,
                        TO_NUMBER(height, '99G999D9S') AS height,
                        CASE
                            WHEN rel IS NOT NULL THEN rel
                            ELSE id
                        END AS parent,
                        ST_FlipCoordinates(wkb_geometry) AS wkb_geometry
                    FROM (
                            SELECT split_part(id, '/', 2) AS id,
                                height,
                                CAST(raw_gdal."@relations"->0->'rel' AS TEXT) AS rel,
                                wkb_geometry
                            FROM "public"."raw_gdal"
                            WHERE GeometryType(wkb_geometry) = 'POLYGON'
                                OR GeometryType(wkb_geometry) = 'MULTIPOLYGON'
                        ) AS tmp
                    WHERE rel IS NOT NULL
                        OR height IS NOT NULL
                        OR id IN (
                            SELECT CAST(raw_gdal."@relations"->0->'rel' AS TEXT)
                            FROM raw_gdal
                        )
                ) AS tmp2
            GROUP BY parent
        ) AS tmp3
    WHERE average_height IS NOT NULL
);
CREATE TABLE "public"."raw_building" AS (
    SELECT "parent",
        'building_' || gen_random_uuid() AS "gmlid",
        ST_Translate(
            ST_Extrude("geometry", 0, 0, "average_height"),
            0,
            0,
            "elevation"
        ) AS "geom",
        "average_height" AS "mh"
    FROM "public"."raw_data"
    WHERE "average_height" IS NOT NULL
);
CREATE TABLE "public"."raw_surface" AS (
    SELECT "building_gmlid",
        'surface_' || gen_random_uuid() AS "gmlid",
        "class",
        ST_Reverse("geom") AS geom
    FROM (
            SELECT "building_gmlid",
                "geom",
                CASE
                    WHEN ST_Zmin("geom") = ST_Zmax("geom") THEN CASE
                        WHEN ST_Zmin("geom") = "bzl" THEN 35
                        ELSE 33
                    END
                    ELSE 34
                END AS "class"
            FROM (
                    SELECT "gmlid" AS "building_gmlid",
                        ST_Zmin("geom") AS "bzl",
                        (ST_Dump(ST_CollectionExtract("geom"))).geom AS "geom"
                    FROM "public"."raw_building"
                ) AS "table1"
        ) AS "table2"
);