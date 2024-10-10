-- Drop the table if it exists 
DROP TABLE IF EXISTS isochrone_building_ref;

-- Create the table  
CREATE TABLE isochrone_building_ref AS
SELECT
    DISTINCT ia.iri,
    pno.poi_iri,
    pno.poi_type
FROM
    isochrone_aggregated AS ia
    LEFT JOIN poi_nearest_node AS pno ON ia.poi_type = pno.poi_type
ORDER BY
    ia.iri;