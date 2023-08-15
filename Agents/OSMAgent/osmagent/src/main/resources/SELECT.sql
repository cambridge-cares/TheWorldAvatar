SELECT name, osm_id, building, amenity, way FROM planet_osm_point WHERE amenity ='school' UNION 
SELECT name, osm_id, building, amenity, way FROM planet_osm_point WHERE amenity ='school' UNION 
SELECT name, osm_id, building, amenity, way FROM planet_osm_polygon WHERE building ='school' UNION 
SELECT name, osm_id, building, amenity, way FROM planet_osm_polygon WHERE building ='school'  