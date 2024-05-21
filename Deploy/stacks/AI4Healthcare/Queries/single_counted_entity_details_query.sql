-- Step 1: Create a buffer around the trajectory
WITH trajectory_buffer AS (
  SELECT ST_Buffer(ST_MakeLine(geom::geometry ORDER BY "UTC TIME")::geography, 100) AS buffer
  FROM gps_data
),

-- Step 2: Find the intersections of the buffer with supermarkets
intersecting_supermarkets AS (
  SELECT sm.*
  FROM supermarket sm, trajectory_buffer tb
  WHERE ST_Intersects(sm.geom::geography, tb.buffer)
),

-- Step 3: Find the intersections of the buffer with takeaways
intersecting_takeaways AS (
  SELECT tk.*
  FROM takeaways tk, trajectory_buffer tb
  WHERE ST_Intersects(tk.geom::geography, tb.buffer)
),

-- Step 4: Associate each supermarket with the nearest trajectory point
supermarket_with_nearest_point AS (
  SELECT sm."Name", sm."Address", sm.geom, 
         (SELECT gps.geom 
          FROM gps_data gps 
          ORDER BY sm.geom::geography <-> gps.geom::geography 
          LIMIT 1) AS nearest_point
  FROM intersecting_supermarkets sm
),

-- Step 5: Associate each takeaway with the nearest trajectory point
takeaway_with_nearest_point AS (
  SELECT tk."Name", tk."Address", tk.geom, 
         (SELECT gps.geom 
          FROM gps_data gps 
          ORDER BY tk.geom::geography <-> gps.geom::geography 
          LIMIT 1) AS nearest_point
  FROM intersecting_takeaways tk
),

-- Combine both types of food retailers with nearest points
combined_food_retailers AS (
  SELECT * FROM supermarket_with_nearest_point
  UNION ALL
  SELECT * FROM takeaway_with_nearest_point
)

-- Final count
SELECT 
  ST_AsText(fr_gps.nearest_point)as "gps_point", ST_AsText(fr_gps.geom) as "fr_location",
  COUNT(*) AS no_of_food_retailers, fr_gps."Name" as "name", fr_gps."Address" as "address" 
FROM combined_food_retailers AS fr_gps
GROUP BY fr_gps.nearest_point, fr_gps.geom, fr_gps."Name", fr_gps."Address"
