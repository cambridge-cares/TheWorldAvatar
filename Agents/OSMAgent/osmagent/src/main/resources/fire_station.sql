UPDATE planet_osm_point
SET ontobuilt = 'FireStation'
WHERE amenity = 'fire_station' OR building = 'fire_station';

UPDATE planet_osm_polygon
SET ontobuilt = 'FireStation'
WHERE amenity = 'fire_station' OR building = 'fire_station';

-- SELECT Statement
SELECT name, propertyusage_iri,building_iri, ontobuilt, way FROM planet_osm_polygon
WHERE ontobuilt::text = 'FireStation'
UNION
SELECT name, propertyusage_iri,building_iri, ontobuilt, way FROM planet_osm_point
WHERE ontobuilt::text = 'FireStation';