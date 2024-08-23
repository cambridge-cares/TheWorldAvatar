-- Drop the materialized view if it exists
DROP MATERIALIZED VIEW IF EXISTS usage.buildingusage_geoserver_sg;

CREATE MATERIALIZED VIEW usage.buildingusage_geoserver_sg AS
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
), factory_data_combined AS (
    SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'factories' as infrastructure_type
	FROM factories
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'data_centres' as infrastructure_type
	FROM data_centres
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'precision_engineering' as infrastructure_type
	FROM precision_engineering
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'printing' as infrastructure_type
	FROM printing
	GROUP BY building_uuid
), gfa_table AS (
    SELECT realval AS calc_gfa, cityobject_id
	FROM citydb.cityobject_genericattrib
	WHERE attrname = 'GFA'
), ref_gfa_table AS (
    SELECT mb.building_uuid AS uuid, 
        CASE WHEN "GPR" ~ '^\d+(\.\d+)?$' 
        THEN ("GPR"::double precision * public.ST_Area("lod1Geometry", true))
		ELSE 0
        END AS ref_gfa
    FROM public.landplot AS pl, public.landplot_buildings AS mb
    WHERE pl.ogc_fid = mb.ogc_fid 
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
	citydb.objectclass.classname AS objectclass,
	infrastructure_type,
	calc_gfa,
	ref_gfa
FROM citydb.building b
JOIN citydb.surface_geometry sg ON sg.root_id = b.lod0_footprint_id
JOIN uuid_table ON b.id = uuid_table.cityobject_id
JOIN iri_table ON b.id = iri_table.cityobject_id
JOIN citydb.objectclass ON b.objectclass_id = citydb.objectclass.id
LEFT JOIN pointsTable ON uuid_table.uuid = pointsTable.iri
LEFT JOIN polygonsTable ON uuid_table.uuid = polygonsTable.iri
LEFT JOIN usageTable ON uuid_table.uuid = usageTable.iri
LEFT JOIN factory_data_combined ON uuid_table.uuid = factory_data_combined.building_uuid
LEFT JOIN gfa_table ON b.id = gfa_table.cityobject_id
LEFT JOIN ref_gfa_table ON uuid_table.uuid = ref_gfa_table.uuid
WHERE sg.geometry IS NOT NULL AND COALESCE(measured_height, 100.0) != '0'

UNION ALL

SELECT
    CONCAT('City Furniture ', uuid_table.cityobject_id) as name,
    COALESCE(cityfurniture_height.realval, 100.0) AS building_height,
    ST_Transform(cityfurniture_footprint.geomval, 4326) AS geom,
    iri_table.iri,
    null as ontobuilt,
    heat_emissions,
    objectclass.classname AS objectclass,
	null as infrastructure_type,
	0 as calc_gfa,
	0 as ref_gfa
FROM 
    citydb.city_furniture
JOIN 
    uuid_table ON city_furniture.id = uuid_table.cityobject_id
JOIN 
    iri_table ON city_furniture.id = iri_table.cityobject_id
JOIN 
    cityfurniture_height ON city_furniture.id = cityfurniture_height.cityobject_id
JOIN 
    cityfurniture_footprint ON city_furniture.id = cityfurniture_footprint.cityobject_id
JOIN 
    citydb.objectclass ON city_furniture.objectclass_id = objectclass.id
LEFT JOIN 
    jurong_island_city_furniture ON uuid_table.uuid = jurong_island_city_furniture.city_furniture_uuid::varchar
WHERE 
    cityfurniture_footprint.geomval IS NOT NULL;

CREATE INDEX usage_index ON usage.buildingusage_geoserver_sg (ontobuilt);
CREATE INDEX geometry_index ON usage.buildingusage_geoserver_sg USING GIST (geom);
CREATE INDEX classname_index ON usage.buildingusage_geoserver_sg (objectclass);
CREATE INDEX infra_index ON usage.buildingusage_geoserver_sg (infrastructure_type);
CREATE INDEX heat_index ON usage.buildingusage_geoserver_sg (heat_emissions);
CREATE INDEX gfa_index ON usage.buildingusage_geoserver_sg (calc_gfa);
CREATE INDEX refgfa_index ON usage.buildingusage_geoserver_sg (ref_gfa);

DROP MATERIALIZED VIEW IF EXISTS usage.buildings_with_cea;
CREATE MATERIALIZED VIEW usage.buildings_with_cea AS (
SELECT bg.*, 'http://sg-blazegraph:8080/blazegraph/namespace/cea/sparql' as endpoint FROM usage.buildingusage_geoserver_sg AS bg
JOIN cea.cea AS c ON c.iri = bg.iri);
CREATE INDEX geometry_cea_index ON usage.buildings_with_cea USING GIST (geom);