-- Drop the materialized view if it exists
DROP MATERIALIZED VIEW IF EXISTS usage.buildingusage_geoserver;

CREATE MATERIALIZED VIEW usage.buildingusage_geoserver AS
WITH uuid_table AS (
    SELECT strval AS uuid, cityobject_id
    FROM citydb.cityobject_genericattrib
    WHERE attrname = 'uuid'
), iri_table AS (
    SELECT urival AS iri, cityobject_id
    FROM citydb.cityobject_genericattrib
    WHERE attrname = 'iri'
), usageTable AS (
    SELECT building_iri AS iri, propertyusage_iri, ontobuilt, usageshare
    FROM usage.usage
), pointsTable AS (
    SELECT building_iri AS iri, name
    FROM osm.points
), polygonsTable AS (
    SELECT building_iri AS iri, name
    FROM osm.polygons
), cityfurniture_footprint AS (
    SELECT
        "cityobject_id",
        "geomval"
    FROM "citydb"."cityobject_genericattrib"
    WHERE attrname = 'footprint'
), cityfurniture_height AS (
    SELECT
        "cityobject_id",
        "realval"
    FROM "citydb"."cityobject_genericattrib"
    WHERE attrname = 'height'
)

SELECT DISTINCT
    CASE
        WHEN COALESCE(pointsTable.name, polygonsTable.name) IS NOT NULL
        THEN COALESCE(pointsTable.name, polygonsTable.name)
        ELSE CONCAT('Building ', uuid_table.cityobject_id)
    END AS name,
    COALESCE(measured_height, 100.0) AS building_height,
    ST_Transform(geometry, 4326) AS geom,
    iri_table.iri,
    ontobuilt,
	heat_emissions,
	citydb.objectclass.classname AS objectclass
FROM citydb.building b
JOIN citydb.surface_geometry sg ON sg.root_id = b.lod0_footprint_id
JOIN uuid_table ON b.id = uuid_table.cityobject_id
JOIN iri_table ON b.id = iri_table.cityobject_id
JOIN citydb.objectclass ON b.objectclass_id = citydb.objectclass.id
LEFT JOIN pointsTable ON uuid_table.uuid = pointsTable.iri
LEFT JOIN polygonsTable ON uuid_table.uuid = polygonsTable.iri
LEFT JOIN usageTable ON uuid_table.uuid = usageTable.iri
LEFT JOIN company ON uuid_table.uuid = company.building_iri
WHERE sg.geometry IS NOT NULL AND COALESCE(measured_height, 100.0) != '0'

UNION ALL

SELECT
    CONCAT('City Furniture ', uuid_table.cityobject_id) as name,
	COALESCE(cityfurniture_height.realval, 100.0) AS building_height,
	ST_Transform(cityfurniture_footprint.geomval, 4326) AS geom,
	iri_table.iri,
	null as ontobuilt,
	null as heat_emissions,
	citydb.objectclass.classname AS objectclass
FROM 
    "citydb"."city_furniture", "uuid_table", "iri_table", "cityfurniture_height", "cityfurniture_footprint", "citydb"."objectclass"
WHERE "citydb"."city_furniture"."objectclass_id" = "citydb"."objectclass"."id"
    AND "citydb"."city_furniture"."id" = "cityfurniture_height"."cityobject_id"
    AND "citydb"."city_furniture"."id" = "cityfurniture_footprint"."cityobject_id"
    AND "citydb"."city_furniture"."id" = "uuid_table"."cityobject_id"
    AND "citydb"."city_furniture"."id" = "iri_table"."cityobject_id"   
    AND "cityfurniture_footprint"."geomval" IS NOT NULL;

CREATE INDEX usage_index ON usage.buildingusage_geoserver (ontobuilt);
CREATE INDEX geometry_index ON usage.buildingusage_geoserver USING GIST (geom);
CREATE INDEX classname_index ON usage.buildingusage_geoserver (objectclass);