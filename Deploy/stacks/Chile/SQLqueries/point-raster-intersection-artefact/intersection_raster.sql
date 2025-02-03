-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--EXAMPLE 3  MEAN WIND SPEED AT EXISTING WIND PLANTS (2018)
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT rid,
    ST_Value(rast, geom, true) AS value,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
    nombre,
    tipo
FROM
    "windfullchile05", (
    SELECT
        nombre,
        tipo,
        ST_Transform(ST_SetSRID(ST_Point(coord_este, coord_nort), 32719), 4326 ) AS geom
    FROM "gov_centraleolic"
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL



-- "windfullchile05": raster
-- gov_centraleolic: vector (points)

-- Then upload results with data uploader.




-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
-- EXAMPLE 3  ELEVATION OF EXISTING HYDRO PLANTS (2018)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT rid,
    ST_Value(rast, geom, true) AS value,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
    nombre,
    tipo
FROM
    "atlas_elevationwithbathymetry", (
    SELECT
        nombre,
        tipo,
        ST_Transform(ST_SetSRID(ST_Point(coord_este, coord_nort), 32719), 4326 ) AS geom
    FROM "gov_centralhydro"
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL


-- "atlas_elevationwithbathymetry": raster
-- gov_centralhydro: vector (points)

-- Then upload results with data uploader.

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- EXAMPLE 3  ELEVATION OF EXISTING SOLAR PLANTS (2018)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT rid,
    ST_Value(rast, geom, true) AS value,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
    nombre,
    tipo,
    potencia
FROM
    "atlas_elevationwithbathymetry", (
    SELECT
        nombre,
        potencia,
        tipo,
        ST_Transform(ST_SetSRID(ST_Point(coord_este, coord_nort), 32719), 4326 ) AS geom
    FROM "GOVcentralsolar"
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- EXAMPLE 4  ELEVATION OF EXISTING SOLAR PLANTS (2023-dataset)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT rid,
    ST_Value(rast, geom, true) AS value,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "atlas_elevationwithbathymetry", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'solar' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;



-- % ORDER BY potencia_neta_mw;

-- "capacidad6" : vector (points) DATA 2023

-- Then upload results with data uploader.


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- EXAMPLE 5  RELEVANT VARIABLES (2023-dataset)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


SELECT rid,
    ST_Value(rast, geom, true) AS DNI,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "solar_atlas_dni", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'solar' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;


SELECT rid,
    ST_Value(rast, geom, true) AS windspeed_m2,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "atlaswind50m", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'wind' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;


SELECT rid,
    ST_Value(rast, geom, true) AS altitude_m,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "atlas_elevationwithbathymetry", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'hydro' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;


SELECT rid,
    ST_Value(rast, geom, true) AS value,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "atlas_elevationwithbathymetry", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'biomass' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;


SELECT rid,
    ST_Value(rast, geom, true) AS elevation_m,
    ST_X(geom) AS lon,
    ST_Y(geom) AS lat,
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
    type
FROM
    "atlas_elevationwithbathymetry", (
SELECT
        central,
        anio_servicio_central,
        region_nombre,
        clasificacion,
        potencia_neta_mw,
        lat, lon,
        type,
        ST_Transform(ST_SetSRID(ST_Point(lat, lon), 4326 ), 4326 ) AS geom,
        error
    FROM "capacidad6" 
   WHERE type = 'fossilfuel' AND error = 'f' 
) testpoints
WHERE
    ST_Value(rast, geom, true) IS NOT NULL 
ORDER BY anio_servicio_central;

