
-- Create Common Table Expressions (CTEs) feat/stats
WITH
  feat AS (SELECT "LSOA_code" As code, geom FROM lsoa_geo_mini), -- feat is used later to join with stats
  stats AS 
    (SELECT code, (stats).* -- (stats).* is used as a shorthand for selecting all columns from the composite type stats 
    FROM (SELECT code, ST_SummaryStats(ST_Clip(rast,ARRAY[2],geom)) As stats
                     -- ST_SummaryStats() is used to calculates summary statistics for the cells in a raster dataset that intersect with a given geometry
                     -- count: the number of cells that intersect with the geometry
                     -- sum: the sum of the cell values

                     -- ST_Clip clip the 2nd band of rast using the geom, returns clipped raster dataset
        FROM lsoa_tas_temp
        INNER JOIN feat -- Join feat with lsoa_tas_temp
        ON ST_Intersects(feat.geom,rast) -- specifies that the join should only include rows with intersections
    ) 
  ),
  random_points AS (
    SELECT "LSOA_code", ST_Simplify(ST_PointOnSurface(geom), 0.01) AS random_point
    FROM lsoa_geo_mini
  )
SELECT feat.code, 
    SUM(COALESCE(stats.count, 0)) As num_pixels,
    CASE 
        WHEN SUM(stats.count) > 0 THEN SUM(stats.mean*stats.count)/SUM(stats.count)
        ELSE 
            (SELECT AVG(avg_pval) 
             FROM (SELECT ST_Value(lt.rast, 1, ST_Transform(rp.random_point, 4326)) AS avg_pval
                   FROM random_points AS rp
                   JOIN lsoa_tas_temp AS lt ON ST_Intersects(lt.rast, ST_Transform(rp.random_point, 4326))
                   WHERE rp."LSOA_code" = feat.code) AS random_vals)
    END AS avg_pval
FROM feat
LEFT JOIN stats ON feat.code = stats.code
GROUP BY feat.code
ORDER BY feat.code;


-- < -------------------- NEW VERSION --------------------------- > --
EXPLAIN ANALYZE
-- Create Common Table Expressions (CTEs) feat/stats
WITH feat AS ( -- feat is used later to join with stats
    SELECT "LSOA_code" As code, geom 
    FROM lsoa_geo_mini), 
    stats AS (
    SELECT code, (stats).*-- (stats).* is used as a shorthand for selecting all columns from the composite type stats 
    FROM (
      SELECT code, ST_SummaryStats(ST_Clip(rast, ARRAY[2], geom)) As stats
        -- ST_SummaryStats() is used to calculates summary statistics for the cells in a raster dataset that intersect with a given geometry
        -- count: the number of cells that intersect with the geometry
        -- sum: the sum of the cell values
        -- ST_Clip clip the 2nd band of rast using the geom, returns clipped raster dataset
      FROM lsoa_tas_temp
        INNER JOIN feat -- Join feat with lsoa_tas_temp
        ON ST_Intersects(feat.geom, rast) -- specifies that the join should only include rows with intersections
         ) AS foo)
-- Create returning results
SELECT feat.code, SUM(COALESCE(stats.count, 0)) As num_pixels, 
-- LSOA code and number of pixels included
-- COALESCE returns the non-null expression
-- if stats.count is null, the value of 0 will be returned instead
    CASE 
        WHEN SUM(stats.count) > 0 THEN -- when there do have pixel inside geom
        SUM(stats.mean) -- using mean value to represent the average
        ELSE 
        (SELECT AVG(avg_pval) 
        FROM (
                SELECT ST_Value(lt.rast, 2, dp.simplified_geom) AS avg_pval -- ST_Value retrieve the pixel value, taking (rast, band number, point) as arguments
                FROM lsoa_tas_temp AS lt --Selecting raster layer
                JOIN (
                    SELECT "LSOA_code", ST_Simplify(ST_PointOnSurface(geom), 0.01) AS simplified_geom
                    -- ST_PointOnSurface returns a point guaranteed to lie on the surface of the input geometry
                    -- ST_Simplify simplifies the resulting point geometry by removing vertices that do not significantly change the geometry's shape.
                    -- 0.01 represents the tolerance value for simplifying the geometry.
                    FROM lsoa_geo_mini -- Selecting geometry and respective LSOA code
                    WHERE "LSOA_code" = feat.code --filter the results to only include the rows where the LSOA code matches the code of the current feat being processed in the outer query 
                    ) AS dp
                ON ST_Intersects(lt.rast, dp.simplified_geom, 4326) -- specify the condition for JOIN
                ) AS random_vals)
    END  -- End the CASE
    AS avg_pval  
FROM feat
  LEFT JOIN stats ON feat.code = stats.code --Join the feat and stats tables
GROUP BY feat.code
ORDER BY feat.code;

-- < -------------------- DISSOCIATION --------------------------- > --

-- Create CTE feat
WITH feat AS ( -- feat is used later to join with stats
    SELECT "LSOA_code" As code, geom 
    FROM lsoa_geo_mini), 
    stats AS (
    SELECT code, (stats).*-- (stats).* is used as a shorthand for selecting all columns from the composite type stats 
    FROM (
      SELECT code, ST_SummaryStats(ST_Clip(rast, ARRAY[2], geom)) As stats
        -- ST_SummaryStats() is used to calculates summary statistics for the cells in a raster dataset that intersect with a given geometry
        -- count: the number of cells that intersect with the geometry
        -- sum: the sum of the cell values
        -- ST_Clip clip the 2nd band of rast using the geom, returns clipped raster dataset
      FROM lsoa_tas_temp
        INNER JOIN feat -- Join feat with lsoa_tas_temp
        ON ST_Intersects(feat.geom, rast) -- specifies that the join should only include rows with intersections
         ) AS foo)
SELECT *
FROM feat, stats
-- Create CTE stats
WITH feat AS ( -- feat is used later to join with stats
    SELECT "LSOA_code" As code, geom 
    FROM lsoa_geo_mini), 
    stats AS (
    SELECT code, (stats).*-- (stats).* is used as a shorthand for selecting all columns from the composite type stats 
    FROM (
      SELECT code, ST_SummaryStats(ST_Clip(rast, ARRAY[2], geom)) As stats
        -- ST_SummaryStats() is used to calculates summary statistics for the cells in a raster dataset that intersect with a given geometry
        -- count: the number of cells that intersect with the geometry
        -- sum: the sum of the cell values
        -- ST_Clip clip the 2nd band of rast using the geom, returns clipped raster dataset
      FROM lsoa_tas_temp
        INNER JOIN feat -- Join feat with lsoa_tas_temp
        ON ST_Intersects(feat.geom, rast) -- specifies that the join should only include rows with intersections
         ) AS foo)
SELECT *
FROM feat, stats
