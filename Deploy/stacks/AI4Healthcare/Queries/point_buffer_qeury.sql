SELECT gps."UTC DATE" as "Date", gps."UTC TIME" as "Time", frs."Name" as "BusinessName", frs."Address", frs."Latitude" as "FRS_LATITUDE", frs."Longitude" as "FRS.LONGITUDE", COUNT(frs."Name") as "N_of_Retailers"
FROM 
  public.gps_data AS gps
JOIN 
  public.fr_fenland AS frs
ON 
ST_Intersects(ST_Buffer(geography(gps.geom), 100), geography(frs.geom))
GROUP BY
    gps."UTC DATE", gps."UTC TIME", frs."Name", frs."Latitude", frs."Longitude", frs."Address"
ORDER BY
  gps."UTC DATE", gps."UTC TIME"
