--THIS WILL BE REPLACED BY ISOCHRONEAGENT JAVA CODE

DROP TABLE IF EXISTS buildings;

CREATE TABLE buildings AS
SELECT p.name as name, cga.urival AS urival, p.ontobuilt, ST_Transform(sg.geometry, 4326) AS geometry
FROM citydb.building b
INNER JOIN citydb.cityobject_genericattrib cga ON b.id = cga.cityobject_id
INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.id
LEFT JOIN (
    SELECT u.building_iri, u.ontobuilt, u.propertyusage_iri, u.usageshare,
       COALESCE(p.name, o.name) AS name
    FROM usage.usage AS u
    LEFT JOIN osm.points AS p ON u.building_iri = p.building_iri
    LEFT JOIN osm.polygons AS o ON u.building_iri = o.building_iri
) p ON cga.urival = p.building_iri
WHERE cga.attrname = 'iri' AND sg.geometry IS NOT NULL;

ALTER TABLE buildings ADD COLUMN nearest_node integer;

UPDATE buildings AS s
SET nearest_node = (
    SELECT id
    FROM routing_ways_segment_vertices_pgr
    ORDER BY s.geometry <-> the_geom
    LIMIT 1
);