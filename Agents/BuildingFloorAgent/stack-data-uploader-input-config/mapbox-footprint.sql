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
),
"GFA_table" AS (
    SELECT
        "realval" AS "GFA",
        "cityobject_id"
    FROM
        "citydb"."cityobject_genericattrib"
    WHERE
        "attrname" = 'GFA'
),
"refGFA_table" AS (
    SELECT "mb"."building_iri" AS "uuid", 
        CASE WHEN "GPR" ~ '^\d+(\.\d+)?$' 
        THEN ("GPR"::double precision * public.ST_Area("lod1Geometry", true))
        END AS "reference GFA"
    FROM "public"."landplot" AS "pl", "public"."matched_buildings" AS "mb"
    WHERE "pl"."ogc_fid" = "mb"."public_landplot_ogc_fid"           
)
SELECT
    "building"."id" AS "building_id",
    COALESCE("measured_height", 100.0) AS "building_height",
    "geometry",
    "uuid_table"."uuid",
    "iri",
    "GFA",
    "reference GFA"
FROM
    "citydb"."building"
    JOIN "citydb"."surface_geometry" ON "citydb"."surface_geometry"."root_id" = "citydb"."building"."lod0_footprint_id"
    JOIN "uuid_table" ON "citydb"."building"."id" = "uuid_table"."cityobject_id"
    JOIN "iri_table" ON "citydb"."building"."id" = "iri_table"."cityobject_id"
    FULL OUTER JOIN "GFA_table" ON "citydb"."building"."id" = "GFA_table"."cityobject_id"
    FULL OUTER JOIN "refGFA_table" ON "uuid_table"."uuid"= "refGFA_table"."uuid"
WHERE
    "citydb"."surface_geometry"."geometry" IS NOT NULL
