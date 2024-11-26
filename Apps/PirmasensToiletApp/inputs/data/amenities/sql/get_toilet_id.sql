SELECT osm_id
FROM points AS p
WHERE p.amenity = 'toilets' OR p.toilets_position IS NOT NULL OR p.toilets_handwashing IS NOT NULL
ORDER BY "geometryProperty" <-> ST_SetSRID(ST_MakePoint(%lon%, %lat%), 4326) LIMIT 1
