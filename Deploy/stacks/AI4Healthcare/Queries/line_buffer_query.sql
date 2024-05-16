WITH Line AS (
    SELECT 
        gps."UTC DATE", 
        gps."UTC TIME",
		gps."LATITUDE",
		gps."LONGITUDE",
        ST_MakeLine(gps.geom::geometry) AS line_geom -- Creates a line from geometry points
    FROM 
        public.gps_data AS gps
    GROUP BY
        gps."UTC DATE", gps."UTC TIME", gps."LATITUDE", gps."LONGITUDE"
),
BufferedLine AS (
    SELECT
        "UTC DATE",
        "UTC TIME",
		"LATITUDE",
		"LONGITUDE",
        ST_Buffer(line_geom::geography, 100) AS buffered_geom -- Buffer the line in geography units (meters)
    FROM Line
)
SELECT bl."UTC DATE" as "Date", bl."UTC TIME" as "Time", frs."Name" as "BusinessName", frs."Address", frs."Latitude" as "FRS_LATITUDE", frs."Longitude" as "FRS.LONGITUDE", COUNT(frs."Name") as "N_of_Retailers"
FROM 
    BufferedLine bl
JOIN 
    public.fr_fenland AS frs
ON 
    ST_Intersects(bl.buffered_geom, frs.geom::geography)
GROUP BY
    bl."UTC DATE", bl."UTC TIME", frs."Name", frs."Latitude", frs."Longitude", frs."Address"
ORDER BY
    bl."UTC DATE", bl."UTC TIME";
