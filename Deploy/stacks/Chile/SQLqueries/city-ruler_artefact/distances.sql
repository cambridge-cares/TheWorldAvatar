-- Note: The following file has been developed to copy and paste the queries on the adminer screen.

SELECT *, 
       ST_X(geom) AS lat2, 
       '-81' AS lat,
       ST_Y(geom) AS lon, 
       ST_Distance(geom::geography, ST_SetSRID(ST_Point(ST_X(geom), -53.151690150097124), 4326)::geography) / 1000 AS distance_km
FROM (
SELECT "REGION", "NOMBRE", "PESO_ADMIN", "TIPO", ST_Transform(ST_SetSRID(ST_Point(ST_X(ST_Centroid(wkb_geometry)), ST_Y(ST_Centroid(wkb_geometry))), 31979), 4326 ) AS geom
FROM "ine_asentamientos") AS foo


--Gives distance (only latitude) to Punta Arenas
--Punta Arenas	CAPITAL REGIONAL	PRINCIPAL	0101000020E610000065F2758855BA51C070E534956A934AC0	-70.91147052305458	-53.151690150097124



--ine_asentamientosc: vector (points)

--RESULT EXAMPLE
--REGION	NOMBRE	PESO_ADMIN	TIPO	geom	lat2	lat	lon	distance_km
--Región del Libertador General Bernardo O'Higgins	Rancagua - Machalí - Gultro - Los Lirios	CAPITAL REGIONAL	PRINCIPAL	0101000020E6100000DA400B020BAF51C0EC9FB7F0C31541C0	-70.73504687403639	-81	-34.17004212347078	2108.9784417199703


-- -81 is location of points.
--TABLE MUST BE UPLOADED AS VECTOR WITH STACK-DATA UPLOADER

--cities.json
--{
--    "database": "postgres",
--    "workspace": "the_world_avatar",
--    "datasetDirectory": "cities",
--    "dataSubsets": [
--        {
--            "type": "vector",
--            "skip": false,
--            "schema": "public",
--            "table": "cities",
--            "subdirectory": "vector",
--            "ogr2ogrOptions": {
--                "sridIn": "EPSG:4326",
--                "sridOut": "EPSG:4326",
--                "inputDatasetOpenOptions": {
--                    "X_POSSIBLE_NAMES": "lon",
--                    "Y_POSSIBLE_NAMES": "lat",
--                    "GEOM_POSSIBLE_NAMES": "wkb_geometry"
--                }
--            }
--        }
--    ]
--}

-- data/cities/vector/cities_ruler_distances.cvs:

--REGION,NOMBRE,PESO_ADMIN,TIPO,lat2,lat,lon,geom,distance_km
--Región del Libertador General Bernardo O'Higgins,Rancagua - Machalí - Gultro - Los Lirios,CAPITAL REGIONAL,PRINCIPAL,-70.73504687,-81,-34.17004212,"010100000000000000004054C0D72BB0F0C31541C0",2109.02479936765


SELECT "REGION", "NOMBRE", "PESO_ADMIN", "TIPO", "lat2", "lat", "lon",
ST_Point(lat, lon) AS geom,
ST_Distance(geom::geography, ST_SetSRID(ST_Point(-70.91147052305458, -53.151690150097124), 4326)::geography) / 1000 AS distance_km
FROM "cities"