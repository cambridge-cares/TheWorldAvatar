-- Drop the materialized view if it exists
DROP MATERIALIZED VIEW IF EXISTS buildings_layer;

CREATE MATERIALIZED VIEW buildings_layer AS
WITH uuid_table AS (
    SELECT strval AS uuid, cityobject_id
    FROM citydb.cityobject_genericattrib
    WHERE attrname = 'uuid'
), iri_table AS (
    SELECT urival AS iri, cityobject_id
    FROM citydb.cityobject_genericattrib
    WHERE attrname = 'iri'
), usagetable AS (
    SELECT building_iri AS uuid, propertyusage_iri, ontobuilt, usageshare, name
    FROM buildinginfo.usage_area
), factory_data_combined AS (
    SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'chemicals' as infrastructure_type
	FROM chemicals
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'data_centres' as infrastructure_type
	FROM data_centres
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'precision_engineering' as infrastructure_type
	FROM precision_engineering
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
	UNION
	SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'printing' as infrastructure_type
	FROM printing
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
    UNION
    SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'food_beverages' as infrastructure_type
	FROM food_beverages
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
    UNION
    SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'semiconductors' as infrastructure_type
	FROM semiconductors
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
    UNION
    SELECT building_uuid, sum(heat_emissions) as heat_emissions, 'pharmaceuticals' as infrastructure_type
	FROM pharmaceuticals
    WHERE building_uuid IS NOT NULL
	GROUP BY building_uuid
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
        WHEN u.name IS NOT NULL
        THEN u.name
        ELSE CONCAT('Building ', uuid_table.cityobject_id)
    END AS name,
    COALESCE(measured_height, 100.0) AS building_height,
    geometry AS geom,
    iri_table.iri,
    u.ontobuilt,
    'http://sea-level-blazegraph:8080/blazegraph/namespace/kb/sparql' as endpoint,
    c.cost,
    heat_emissions,
    citydb.objectclass.classname AS objectclass,
    infrastructure_type
FROM citydb.building b
JOIN citydb.surface_geometry sg ON sg.root_id = b.lod0_footprint_id
JOIN uuid_table ON b.id = uuid_table.cityobject_id
JOIN iri_table ON b.id = iri_table.cityobject_id
JOIN citydb.objectclass ON b.objectclass_id = citydb.objectclass.id
LEFT JOIN usagetable u ON uuid_table.uuid = u.uuid
LEFT JOIN gfa_floors.cost c ON c.building_uuid = uuid_table.uuid
LEFT JOIN factory_data_combined ON uuid_table.uuid = factory_data_combined.building_uuid
WHERE sg.geometry IS NOT NULL AND COALESCE(measured_height, 100.0) != '0'

UNION ALL

SELECT
    CONCAT('City Furniture ', uuid_table.cityobject_id) as name,
    COALESCE(cityfurniture_height.realval, 100.0) AS building_height,
    cityfurniture_footprint.geomval AS geom,
    iri_table.iri,
    null as ontobuilt,
    'http://sea-level-blazegraph:8080/blazegraph/namespace/kb/sparql' as endpoint,
    null as cost,
    heat_emissions,
    objectclass.classname AS objectclass,
	null as infrastructure_type
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

CREATE INDEX usage_index ON buildings_layer (ontobuilt);
CREATE INDEX geometry_index ON buildings_layer USING GIST (geom);
CREATE INDEX classname_index ON buildings_layer (objectclass);
CREATE INDEX infra_index ON buildings_layer (infrastructure_type);
CREATE INDEX heat_index ON buildings_layer (heat_emissions);

DROP MATERIALIZED VIEW IF EXISTS buildings_with_cea;
CREATE MATERIALIZED VIEW buildings_with_cea AS (
SELECT bg.iri, bg.building_height, bg.name, bg.geom, 'http://sea-level-blazegraph:8080/blazegraph/namespace/cea/sparql' as endpoint FROM buildings_layer AS bg
JOIN cea.cea AS c ON c.iri = bg.iri);
CREATE INDEX geometry_cea_index ON buildings_with_cea USING GIST (geom);