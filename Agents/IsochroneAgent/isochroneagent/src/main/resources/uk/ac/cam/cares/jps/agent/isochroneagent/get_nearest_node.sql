SELECT
    DISTINCT pnn.poi_type
FROM
    poi_nearest_node AS pnn
    LEFT JOIN isochrone_aggregated AS ia ON pnn.poi_type = ia.poi_type
WHERE
    ia.poi_type IS NULL