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
    c.cost
FROM citydb.building b
JOIN citydb.surface_geometry sg ON sg.root_id = b.lod0_footprint_id
JOIN uuid_table ON b.id = uuid_table.cityobject_id
JOIN iri_table ON b.id = iri_table.cityobject_id
JOIN citydb.objectclass ON b.objectclass_id = citydb.objectclass.id
LEFT JOIN usagetable u ON uuid_table.uuid = u.uuid
LEFT JOIN gfa_floors.cost c ON c.building_uuid = uuid_table.uuid
WHERE sg.geometry IS NOT NULL AND COALESCE(measured_height, 100.0) != '0';

CREATE INDEX usage_index ON buildings_layer (ontobuilt);
CREATE INDEX geometry_index ON buildings_layer USING GIST (geom);