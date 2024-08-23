SELECT 
    SUM(st_area(surface_geometry.geometry)) AS total_area
FROM 
    citydb.building
JOIN 
    citydb.surface_geometry 
ON 
    building.lod0_footprint_id = surface_geometry.parent_id
WHERE 
    building.id = (
        SELECT
            cityobject_id
        FROM
            citydb.cityobject_genericattrib
        WHERE
            attrname = 'iri' AND
            urival = '%s'
    )
GROUP BY 
    building.id;
