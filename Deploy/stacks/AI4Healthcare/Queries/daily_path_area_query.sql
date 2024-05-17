WITH BufferedLine AS (
    SELECT 
    ST_Buffer(ST_MakeLine(gps.geom::geometry ORDER BY gps."UTC TIME")::geography, 100) AS buffered_geom -- Buffer the line in geography units (meters)
FROM 
    public.gps_data AS gps
)
SELECT frs."Name" as "BusinessName", frs."Address", frs."Latitude" as "FRS_LATITUDE", frs."Longitude" as "FRS.LONGITUDE", COUNT(frs."Name") as "N_of_Retailers"
FROM 
    BufferedLine bl
JOIN 
    public.fr_fenland AS frs
ON 
    ST_Intersects(bl.buffered_geom, frs.geom::geography)
GROUP BY
    frs."Name", frs."Latitude", frs."Longitude", frs."Address"
