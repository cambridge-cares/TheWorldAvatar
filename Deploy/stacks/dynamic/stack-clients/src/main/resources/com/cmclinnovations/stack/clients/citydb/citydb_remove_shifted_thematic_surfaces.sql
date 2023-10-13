UPDATE
    citydb.thematic_surface
SET
    lod2_multi_surface_id = NULL
WHERE
    id IN ('{idList}')