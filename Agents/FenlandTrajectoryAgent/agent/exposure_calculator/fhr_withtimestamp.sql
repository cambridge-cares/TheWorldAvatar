SELECT 
    gps."UTC DATE" AS date, 
    gps."UTC TIME" AS time, 
    fhr."BusinessName" AS entity_name, 
    ST_AsText(ST_MakePoint(fhr."Geocode/Longitude", fhr."Geocode/Latitude")) AS entity_geom,
    fhr."RatingValue" AS rating_value, 
    COUNT(fhr."BusinessName") AS no_of_entities
FROM 
    public.gps_data AS gps
JOIN (
    SELECT 
        "BusinessName", 
        "RatingValue", 
        "Geocode/Longitude", 
        "Geocode/Latitude" 
    FROM 
        public.fhr_cambridge
) AS fhr
ON ST_DWithin(
    geography(gps.geom), 
    geography(ST_MakePoint(fhr."Geocode/Longitude", fhr."Geocode/Latitude")), 
    100
)
GROUP BY 
    gps."UTC DATE", 
    gps."UTC TIME", 
    fhr."BusinessName", 
    fhr."RatingValue", 
    fhr."Geocode/Longitude", 
    fhr."Geocode/Latitude"
ORDER BY 
    gps."UTC DATE", 
    gps."UTC TIME"