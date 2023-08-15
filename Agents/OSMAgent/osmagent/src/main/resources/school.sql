UPDATE planet_osm_point
SET ontobuilt = 'Education'
WHERE amenity = 'school' OR building = 'school';

UPDATE planet_osm_polygon
SET ontobuilt = 'Education'
WHERE amenity = 'school' OR building = 'school';

-- SELECT Statement
SELECT name, propertyusage_iri,building_iri, ontobuilt, way FROM planet_osm_polygon
WHERE ontobuilt::text = 'Education'
UNION
SELECT name, propertyusage_iri,building_iri, ontobuilt, way FROM planet_osm_point
WHERE ontobuilt::text = 'Education';