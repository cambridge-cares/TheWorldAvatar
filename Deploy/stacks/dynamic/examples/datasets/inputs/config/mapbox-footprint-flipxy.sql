WITH "uuid_table" AS (
    SELECT
        "strval" AS "uuid",
        "cityobject_id"
    FROM
        "citydb"."cityobject_genericattrib"
    WHERE
        "attrname" = 'uuid'
),
"iri_table" AS (
    SELECT
        "urival" AS "iri",
        "cityobject_id"
    FROM
        "citydb"."cityobject_genericattrib"
    WHERE
        "attrname" = 'iri'
)
SELECT
    "building"."id" AS "building_id",
    COALESCE("measured_height", 100.0) AS "building_height",
    public.ST_FlipCoordinates(geometry) AS "geometry",
    "uuid",
    "iri"
FROM
    "citydb"."building"
    JOIN "citydb"."surface_geometry" ON "citydb"."surface_geometry"."root_id" = "citydb"."building"."lod0_footprint_id"
    JOIN "uuid_table" ON "citydb"."building"."id" = "uuid_table"."cityobject_id"
    JOIN "iri_table" ON "citydb"."building"."id" = "iri_table"."cityobject_id"
WHERE
    "citydb"."surface_geometry"."geometry" IS NOT NULL