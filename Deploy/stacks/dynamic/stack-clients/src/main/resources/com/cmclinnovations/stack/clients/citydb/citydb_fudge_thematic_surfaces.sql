INSERT INTO citydb.thematic_surface(
        id,
        objectclass_id,
        building_id,
        lod2_multi_surface_id
    )
SELECT b.id,
    b.objectclass_id,
    b.id AS building_id,
    COALESCE(
        b.lod2_multi_surface_id,
        b.lod3_multi_surface_id,
        b.lod4_multi_surface_id,
        b.lod1_multi_surface_id,
        b.lod2_solid_id,
        b.lod3_solid_id,
        b.lod4_solid_id,
        b.lod1_solid_id
    ) AS lod2_multi_surface_id
FROM citydb.building b
WHERE NOT EXISTS(
        SELECT 1
        FROM citydb.thematic_surface ts
        WHERE b.id = ts.id
    )
    AND COALESCE(
        b.lod2_multi_surface_id,
        b.lod3_multi_surface_id,
        b.lod4_multi_surface_id,
        b.lod1_multi_surface_id,
        b.lod2_solid_id,
        b.lod3_solid_id,
        b.lod4_solid_id,
        b.lod1_solid_id
    ) IS NOT NULL
RETURNING id;