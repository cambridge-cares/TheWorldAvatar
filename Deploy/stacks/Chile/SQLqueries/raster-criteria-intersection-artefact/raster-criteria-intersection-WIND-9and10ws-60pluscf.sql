-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.

-- ***************************
-- CREATE AREAS EXCELLENT FOR WIND SPEED, CAPACITY FACTOR AND ELEVATION (Area EE)
-- ***************************


SELECT  ST_CreateOverview('atlas_elevationwithbathymetry'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlas_capacity_iec1'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlaswind50m'::regclass, 'rast', 4, 'NearestNeighbor');
-- o_4_atlas_elevationwithbathymetry
-- o_4_atlas_capacity_iec1
-- o_4_atlaswind50m


-- Initial rasters: atlaswind50m, atlas_elevationwithbathymetry, atlas_capacity_iec1
-- Criteria: 9.000000001-10:5-5, 20-500:5-5, 0.6000000001-0.95:5-5
-- ***********************************
-- RUN IN ADMINER:
-- ***********************************

CREATE TABLE windchile4 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE windchile4neg AS (SELECT * FROM "o_4_atlaswind50m");

UPDATE windchile4neg SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-9.0:0-0,9.000000001-10.0:5-5,10.00000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE windchile4 SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-9.0:0-0,9.000000001-10.0:5-5,10.0000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE windchile4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM windchile4 AS a , windchile4neg AS b );

CREATE TABLE windchile4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "windchile4added");

CREATE TABLE windchile4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "windchile4addedpoly";



CREATE TABLE elevation4 AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");
CREATE TABLE elevation4neg AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");

UPDATE elevation4neg SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE elevation4 SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE elevation4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM elevation4 AS a , elevation4neg AS b );

CREATE TABLE elevation4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "elevation4added");

CREATE TABLE elevation4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "elevation4addedpoly";




CREATE TABLE capacity4 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE capacity4neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

UPDATE capacity4neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.6:0-0,0.6000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE capacity4 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.6:0-0,0.6000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE capacity4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM capacity4 AS a , capacity4neg AS b );

CREATE TABLE capacity4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "capacity4added");

CREATE TABLE capacity4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "capacity4addedpoly";




CREATE TABLE intersection_ee9_60 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "capacity4addedpolydis" AS a, (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "elevation4addedpolydis" AS a, "windchile4addedpolydis" AS b) AS b) 

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "intersection_ee9_60"



SELECT path, geom, GeometryType(geom), 9 as minwindspeed
FROM (SELECT (ST_dump(geom)).*, 9 as minwindspeed
FROM "intersection_ee9_60" ) as foo
WHERE GeometryType(geom) = 'POLYGON'

-- Download CSV

-- ***********************************
-- "intersection_ee9_60" can be downloaded and uploaded with the Data Uploader to be visualized














-- ***************************
-- CREATE AREAS EXCELLENT FOR WIND SPEED, CAPACITY FACTOR AND ELEVATION (Area EE)
-- 
-- ***************************


SELECT  ST_CreateOverview('atlas_elevationwithbathymetry'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlas_capacity_iec1'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlaswind50m'::regclass, 'rast', 4, 'NearestNeighbor');
-- o_4_atlas_elevationwithbathymetry
-- o_4_atlas_capacity_iec1
-- o_4_atlaswind50m


-- Initial rasters: atlaswind50m, atlas_elevationwithbathymetry, atlas_capacity_iec1
-- Criteria: 10.000000001-11:5-5, 20-500:5-5, 0.6000000001-0.95:5-5
-- ***********************************
-- RUN IN ADMINER:
-- ***********************************

CREATE TABLE windchile410 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE windchile410neg AS (SELECT * FROM "o_4_atlaswind50m");

UPDATE windchile410neg SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-10.0:0-0,10.000000001-11.0:5-5,11.00000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE windchile410 SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-10.0:0-0,10.000000001-11.0:5-5,11.0000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE windchile410added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM windchile410 AS a , windchile410neg AS b );

CREATE TABLE windchile410addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "windchile410added");

CREATE TABLE windchile410addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "windchile410addedpoly";



CREATE TABLE elevation4 AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");
CREATE TABLE elevation4neg AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");

UPDATE elevation4neg SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE elevation4 SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE elevation4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM elevation4 AS a , elevation4neg AS b );

CREATE TABLE elevation4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "elevation4added");

CREATE TABLE elevation4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "elevation4addedpoly";




CREATE TABLE capacity4 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE capacity4neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

UPDATE capacity4neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.6:0-0,0.6000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE capacity4 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.6:0-0,0.6000000001-0.95:5-5,0.950000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE capacity4added AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM capacity4 AS a , capacity4neg AS b );

CREATE TABLE capacity4addedpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "capacity4added");

CREATE TABLE capacity4addedpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "capacity4addedpoly";




CREATE TABLE intersection_ee10_60 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "capacity4addedpolydis" AS a, (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "elevation4addedpolydis" AS a, "windchile410addedpolydis" AS b) AS b) 

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "intersection_ee10_60"



SELECT path, geom, GeometryType(geom), 9 as minwindspeed
FROM (SELECT (ST_dump(geom)).*, 9 as minwindspeed
FROM "intersection_ee10_60" ) as foo
WHERE GeometryType(geom) = 'POLYGON'

-- Download CSV
-- 
-- ***********************************
-- "intersection_ee10_60" can be downloaded and uploaded with the Data Uploader to be visualized

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "intersection_ee10_60"
-- km2
-- 7220.907927081507

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "intersection_ee9_60"
-- km2
-- 14063.055187823637
















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
-- ***********************************

