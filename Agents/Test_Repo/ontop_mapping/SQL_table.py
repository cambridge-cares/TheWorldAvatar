# Jiying's old code
'''
SELECT lsoa_geo."LSOA_code" as code, lsoa_geo.geom as geo,
(sum((st_summarystats(st_clip(ST_Transform(lsoa_tas_temp.rast, 4326), lsoa_geo.geom))).sum) / 
(sum((st_summarystats(st_clip(ST_Transform(lsoa_tas_temp.rast, 4326), lsoa_geo.geom))).count))::double precision) AS Pixel_Value
FROM  "LSOA_Geometry" AS lsoa_geo, lsoa_tas_temp
WHERE ST_Intersects(ST_Transform(lsoa_tas_temp.rast, ST_SRID(lsoa_geo.geom)), ST_SetSRID(lsoa_geo.geom, 4326))
GROUP BY lsoa_geo."LSOA_code", lsoa_geo.geom;
'''

# Using ST_SummaryStats
'''
WITH
  feat AS (SELECT "LSOA_code" As code, geom FROM lsoa_geo_mini AS b),
  stats AS 
    (SELECT code, (stats).*
FROM (SELECT code, ST_SummaryStats(ST_Clip(rast,ARRAY[2],geom)) As stats
    FROM lsoa_tas_temp
        INNER JOIN feat
    ON ST_Intersects(feat.geom,rast) 
    ) As foo
    )
SELECT code, SUM(count) As num_pixels,
    SUM(mean*count)/SUM(count) As avg_pval
    FROM stats
    WHERE count > 0
        GROUP BY code
        ORDER BY code;
'''
# Select point and raster data
'''
WITH random_points AS (
    SELECT ST_Simplify(ST_PointOnSurface(geom), 0.01) AS random_point
    FROM lsoa_geo_mini
)
SELECT ST_Value(lt.rast, 1, ST_Transform(rp.random_point, 4326)) AS pixel_value
FROM random_points AS rp, lsoa_tas_temp AS lt
WHERE ST_Intersects(lt.rast, ST_Transform(rp.random_point, 4326));
'''
# Joint previous two SQL
'''
WITH
  feat AS (SELECT "LSOA_code" As code, geom FROM lsoa_geo_mini AS b),
  stats AS 
    (SELECT code, (stats).*
FROM (SELECT code, ST_SummaryStats(ST_Clip(rast,ARRAY[2],geom)) As stats
    FROM lsoa_tas_temp
        INNER JOIN feat
    ON ST_Intersects(feat.geom,rast) 
    ) As foo
    ),
  random_points AS (
    SELECT ST_Simplify(ST_PointOnSurface(geom), 0.01) AS random_point, "LSOA_code"
    FROM lsoa_geo_mini
  )
SELECT feat.code, 
    SUM(COALESCE(stats.count, 0)) As num_pixels,
    CASE 
        WHEN SUM(stats.count) > 0 THEN SUM(stats.mean*stats.count)/SUM(stats.count)
        ELSE 
            (SELECT AVG(ST_Value(lt.rast, 1, ST_Transform(rp.random_point, 4326))) 
             FROM random_points AS rp
             JOIN lsoa_tas_temp AS lt ON ST_Intersects(lt.rast, ST_Transform(rp.random_point, 4326))
             WHERE rp."LSOA_code" = feat.code)
    END AS avg_pval
FROM feat
LEFT JOIN stats ON feat.code = stats.code
GROUP BY feat.code
ORDER BY feat.code;
'''

# Create a TABLE lsoa_random_pixels
'''
DROP TABLE IF EXISTS lsoa_random_pixels;
CREATE TABLE lsoa_random_pixels AS
WITH random_points AS (
    SELECT "LSOA_code", ST_Simplify(ST_PointOnSurface(geom), 0.01) AS random_point
    FROM lsoa_geo_mini
)
SELECT rp."LSOA_code", rp.random_point, ST_Value(lt.rast, 1, ST_Transform(rp.random_point, 4326)) AS pixel_value
FROM random_points AS rp
JOIN lsoa_tas_temp AS lt ON ST_Intersects(lt.rast, ST_Transform(rp.random_point, 4326));
'''
# Joint using lsoa_random_pixels
'''
WITH
  feat AS (SELECT "LSOA_code" As code, geom FROM lsoa_geo_mini AS b),
  stats AS 
    (SELECT code, (stats).*
    FROM (SELECT code, ST_SummaryStats(ST_Clip(rast,ARRAY[2],geom)) As stats
        FROM lsoa_tas_temp
        INNER JOIN feat
        ON ST_Intersects(feat.geom,rast) 
    ) As foo
  )
SELECT feat.code, 
    SUM(COALESCE(stats.count, 0)) As num_pixels,
    CASE 
        WHEN SUM(stats.count) > 0 THEN SUM(stats.mean*stats.count)/SUM(stats.count)
        ELSE 
            (SELECT AVG(pixel_value) 
             FROM lsoa_random_pixels 
             WHERE "LSOA_code" = feat.code)
    END AS avg_pval
FROM feat
LEFT JOIN stats ON feat.code = stats.code
GROUP BY feat.code
ORDER BY feat.code;
'''
# Combining two previous one
'''
-- Create Common Table Expressions (CTEs) feat/stats
WITH
  feat AS (SELECT "LSOA_code" As code, geom FROM lsoa_geo_mini AS b), -- feat is used later to join with stats
  stats AS 
    (SELECT code, (stats).*
    FROM (SELECT code, ST_SummaryStats(ST_Clip(rast,ARRAY[2],geom)) As stats
        FROM lsoa_tas_temp
        INNER JOIN feat
        ON ST_Intersects(feat.geom,rast) 
    ) As foo
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
'''

# Change geometry to geom
'''
ALTER TABLE your_table ADD COLUMN geom geometry;
UPDATE your_table SET geom = ST_GeomFromText(geometry, 4326);
ALTER TABLE your_table DROP COLUMN geometry;
'''
# raster to wkt
'''
SELECT ST_AsText(geom) AS wkt_geometry
FROM (
  SELECT (ST_PixelAsPolygons(rast)).*
  FROM lsoa_tas_temp
) AS subq
'''
# geom to wkt
'''
SELECT ST_AsText(geom) FROM "LSOA_Geometry";
'''
# flip the coordinates
'''
UPDATE lsoa_geo_mini SET geom = ST_FlipCoordinates(geom)
'''
# find intersections
'''
SELECT ST_Value(lsoa_tas_temp.rast, ST_SetSRID(ST_PointOnSurface((dumped_geom).geom), 4326)) AS pixel_value
FROM lsoa_tas_temp
JOIN "LSOA_Geometry" ON ST_Intersects(lsoa_tas_temp.rast, "LSOA_Geometry".geom)
JOIN LATERAL ST_Dump("LSOA_Geometry".geom) AS dumped_geom ON true
'''
# chuncate into small table
'''
DROP TABLE IF EXISTS lsoa_geo_mini;
CREATE TABLE lsoa_geo_mini AS
SELECT *
FROM "LSOA_Geometry"
LIMIT 100;
'''
# metadata for netcdf
'''
Global attributes:
        comment: Monthly resolution gridded climate observations
        creation_date: 2022-03-11T17:15:21
        frequency: mon
        institution: Met Office
        references: doi: 10.1002/joc.1161
        short_name: monthly_meantemp
        source: HadUK-Grid_v1.1.0.0
        title: Gridded surface climate observations data for the UK
        version: v20220310
        Conventions: CF-1.5
Variable attributes:
        tas:
                _FillValue: 1e+20
                standard_name: air_temperature
                long_name: Mean air temperature
                units: degC
                description: Mean air temperature
                label_units: C
                level: 1.5m
                plot_label: Mean air temperature at 1.5m (C)
                cell_methods: time: mid_range within days time: mean over days
                grid_mapping: transverse_mercator
                coordinates: latitude longitude month_number season_year
        transverse_mercator:
                grid_mapping_name: transverse_mercator
                longitude_of_prime_meridian: 0.0
                semi_major_axis: 6377563.396
                semi_minor_axis: 6356256.909
                longitude_of_central_meridian: -2.0
                latitude_of_projection_origin: 49.0
                false_easting: 400000.0
                false_northing: -100000.0
                scale_factor_at_central_meridian: 0.9996012717
        time:
                axis: T
                bounds: time_bnds
                units: hours since 1800-01-01 00:00:00
                standard_name: time
                calendar: gregorian
        time_bnds:
        projection_y_coordinate:
                axis: Y
                bounds: projection_y_coordinate_bnds
                units: m
                standard_name: projection_y_coordinate
        projection_y_coordinate_bnds:
        projection_x_coordinate:
                axis: X
                bounds: projection_x_coordinate_bnds
                units: m
                standard_name: projection_x_coordinate
        projection_x_coordinate_bnds:
        latitude:
                units: degrees_north
                standard_name: latitude
        longitude:
                units: degrees_east
                standard_name: longitude
        month_number:
        season_year:
                units: 1
                long_name: season_year
'''
                      # Entrance Point #
# http://localhost:3846/adminer/ui/?username=postgres&pgsql=