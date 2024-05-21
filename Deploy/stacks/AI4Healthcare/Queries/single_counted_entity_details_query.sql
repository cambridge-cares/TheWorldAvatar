-- Step 1: Create a buffer around the trajectory
WITH trajectory_buffer AS (
  SELECT ST_Buffer(ST_MakeLine(geom::geometry ORDER BY "UTC TIME")::geography, 100) AS buffer
  FROM gps_data
),

-- Step 2: Find the intersections of the buffer with food retailers
intersecting_food_retailers AS (
  SELECT fr.*
  FROM fr_fenland fr, trajectory_buffer tb
  WHERE ST_Intersects(fr.geom::geography, tb.buffer)
),

-- Step 3: Associate each food retailer with the nearest trajectory point
food_retailer_with_nearest_point AS (
  SELECT fr.*, 
         (SELECT gps.geom 
          FROM gps_data gps 
          ORDER BY fr.geom::geography <-> gps.geom::geography 
          LIMIT 1) AS nearest_point
  FROM intersecting_food_retailers fr
)

-- Final count
SELECT 
  ST_AsText(fr_gps.nearest_point) as "gps_point", ST_AsText(fr_gps.geom) as "fr_location",
  COUNT(*) AS no_of_food_retailers, fr_gps."Name" as "name", fr_gps."Address" as "address"
FROM food_retailer_with_nearest_point AS fr_gps
GROUP BY fr_gps.nearest_point, fr_gps.geom, fr_gps."Name", fr_gps."Address"
