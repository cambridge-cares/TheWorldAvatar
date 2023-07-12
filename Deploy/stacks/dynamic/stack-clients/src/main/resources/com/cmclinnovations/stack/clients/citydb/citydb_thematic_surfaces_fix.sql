INSERT INTO citydb.thematic_surface(id, objectclass_id, building_id, lod2_multi_surface_id, lod3_multi_surface_id, lod4_multi_surface_id)
SELECT id, objectclass_id, id AS building_id, COALESCE(lod2_multi_surface_id, lod3_multi_surface_id) AS lod2_multi_surface_id, lod3_multi_surface_id, lod4_multi_surface_id
FROM citydb.cityobject JOIN citydb.building USING (id, objectclass_id)
WHERE citydb.cityobject.id NOT IN (SELECT id FROM citydb.thematic_surface) ;