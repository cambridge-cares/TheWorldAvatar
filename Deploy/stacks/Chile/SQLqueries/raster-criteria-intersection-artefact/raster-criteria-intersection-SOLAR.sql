-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.

-- *********************************
-- ********************************
-- SOLAR
-- ********************************
-- *********************************



SELECT  ST_CreateOverview('solar_atlas_dni'::regclass, 'rast', 4, 'NearestNeighbor')
SELECT  ST_CreateOverview('atlas_elevationwithbathymetry'::regclass, 'rast', 4, 'NearestNeighbor')

-- +++++++++
-- RESULTS: 
-- o_4_solar_atlas_dni
-- o_4_atlas_elevationwithbathymetry
-- ++++++++++



CREATE TABLE ysole4 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole4neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole4neg SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2500:0-0,2500.0000000001-7000:5-5 ', '32BF'::text,NULL);
UPDATE ysole4 SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2500:0-0,2500.0000000001-7000:5-5 ', '32BF'::text,0);

CREATE TABLE ysole4add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole4 AS a , ysole4neg AS b );

CREATE TABLE ysole4addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole4add");

CREATE TABLE ysole4addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole4addpoly"





CREATE TABLE ysolg4 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg4neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg4neg SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2500:5-5,2500.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg4 SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2500:5-5,2500.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg4add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg4 AS a , ysolg4neg AS b );

CREATE TABLE ysolg4addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg4add");

CREATE TABLE ysolg4addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg4addpoly"




CREATE TABLE yelesol4 AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");
CREATE TABLE yelesol4neg AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");

UPDATE yelesol4neg SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-2500:5-5,2500.00000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE yelesol4 SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-2500:5-5,2500.00000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE yelesol4add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM yelesol4 AS a , yelesol4neg AS b );

CREATE TABLE yelesol4addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "yelesol4add");

CREATE TABLE yelesol4addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "yelesol4addpoly"



CREATE TABLE inter_solgele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg4addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_soleele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole4addpolydis" AS a, "yelesol4addpolydis" AS b);


-- +++++++++++++
-- INFO ABOUT Tmed
-- +++++++++++++



CREATE TABLE inter_soleeleclimate AS 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_soleele" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11);

CREATE TABLE inter_solgeleclimate AS 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom, 
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solgele" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11);



SELECT ST_Area(geom::geography, true)/1000000 as km2, "Denominaci", "Tmed","Alt_min","Alt_max"
FROM "inter_soleeleclimate";
SELECT ST_Area(geom::geography, true)/1000000 as km2, "Denominaci", "Tmed","Alt_min","Alt_max"
FROM "inter_solgeleclimate";



