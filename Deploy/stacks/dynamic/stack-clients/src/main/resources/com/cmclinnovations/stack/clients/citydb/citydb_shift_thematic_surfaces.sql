UPDATE citydb.thematic_surface
SET lod2_multi_surface_id = COALESCE(lod4_multi_surface_id,lod3_multi_surface_id) WHERE COALESCE(lod4_multi_surface_id,lod3_multi_surface_id) IS NOT NULL AND lod2_multi_surface_id IS NULL
RETURNING id;