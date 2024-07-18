WITH "uuid_table" AS (
    SELECT
        "strval" AS "uuid",
        "cityobject_id"
    FROM
        "cityobject_genericattrib"
    WHERE
        "attrname" = 'uuid'
),
"iri_table" AS (
    SELECT
        "urival" AS "iri",
        "cityobject_id"
    FROM
        "cityobject_genericattrib"
    WHERE
        "attrname" = 'iri'
)
SELECT
    "building"."id" AS "building_id",
    COALESCE("measured_height", 100.0) AS "building_height",
    "geometry",
    "uuid",
    "iri"
FROM
    "building"
    JOIN "surface_geometry" ON "surface_geometry"."root_id" = "building"."lod0_footprint_id"
    JOIN "uuid_table" ON "building"."id" = "uuid_table"."cityobject_id"
    JOIN "iri_table" ON "building"."id" = "iri_table"."cityobject_id"
WHERE
    "surface_geometry"."geometry" IS NOT NULL