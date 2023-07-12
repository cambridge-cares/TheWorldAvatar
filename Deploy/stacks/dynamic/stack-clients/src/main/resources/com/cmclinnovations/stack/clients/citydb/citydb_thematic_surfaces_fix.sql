INSERT INTO citydb.thematic_surface(id, objectclass_id, building_id, lod2_multi_surface_id, lod3_multi_surface_id, lod4_multi_surface_id)
SELECT co.id, b.objectclass_id, co.id AS building_id, COALESCE(b.lod2_multi_surface_id, b.lod3_multi_surface_id) AS lod2_multi_surface_id, b.lod3_multi_surface_id, b.lod4_multi_surface_id
FROM citydb.cityobject co JOIN citydb.building b USING (id, objectclass_id)
WHERE NOT EXISTS(SELECT 1 FROM citydb.thematic_surface ts WHERE co.id = ts.id);