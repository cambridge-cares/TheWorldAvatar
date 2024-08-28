-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.

-- ***************************
-- CREATE AREAS EXCELLENT FOR WIND SPEED, CAPACITY FACTOR AND ELEVATION (Area EE)
-- ***************************

-- Initial rasters: atlaswind50m, atlas_elevationwithbathymetry, atlas_capacity_iec1
-- Criteria: 7.5000000001-12.5:5-5, 20-500:5-5, 0.5000000001-0.95:5-5
-- ***********************************
-- RUN IN ADMINER:
-- ***********************************

CREATE TABLE windchile4 AS (SELECT * FROM "atlaswind50m");
CREATE TABLE windchile4neg AS (SELECT * FROM "atlaswind50m");

UPDATE windchile4neg SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-7.5:0-0,7.5000000001-12.5:5-5,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE windchile4 SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-7.5:0-0,7.5000000001-12.5:5-5,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE windchile4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM windchile4 AS a , windchile4neg AS b );

CREATE TABLE windchile4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "windchile4added");

CREATE TABLE windchile4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "windchile4addedpoly"



CREATE TABLE elevation4 AS (SELECT * FROM "atlas_elevationwithbathymetry");
CREATE TABLE elevation4neg AS (SELECT * FROM "atlas_elevationwithbathymetry");

UPDATE elevation4neg SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE elevation4 SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE elevation4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM elevation4 AS a , elevation4neg AS b );

CREATE TABLE elevation4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "elevation4added");

CREATE TABLE elevation4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "elevation4addedpoly"




CREATE TABLE capacity4 AS (SELECT * FROM "atlas_capacity_iec1");
CREATE TABLE capacity4neg AS (SELECT * FROM "atlas_capacity_iec1");

UPDATE capacity4neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.5:0-0,0.5000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE capacity4 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.5:0-0,0.5000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE capacity4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM capacity4 AS a , capacity4neg AS b );

CREATE TABLE capacity4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "capacity4added");

CREATE TABLE capacity4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "capacity4addedpoly"




CREATE TABLE intersection_ee AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "capacity4addedpolydis" AS a, (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "elevation4addedpolydis" AS a, "windchile4addedpolydis" AS b) AS b) 

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "intersection_ee"


-- ***********************************
-- "intersection_ee" can be downloaded and uploaded with the Data Uploader to be visualized
-- ***********************************

