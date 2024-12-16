-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.



-- +++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++
-- SOLAR GRID
-- +++++++++++++++++++++++++++

SELECT  ST_CreateOverview('solar_atlas_dni'::regclass, 'rast', 4, 'NearestNeighbor')
SELECT  ST_CreateOverview('atlas_elevationwithbathymetry'::regclass, 'rast', 4, 'NearestNeighbor')

-- +++++++++
-- RESULTS: 
-- o_4_solar_atlas_dni
-- o_4_atlas_elevationwithbathymetry

-- ++++++++++ Note: Climate "IDE_climate" is Multipolygon
-- ++++++++++++++++++++++++++++



CREATE TABLE ysolg420 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg420neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg420neg SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2100:5-5,2100.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg420 SET rast = ST_Reclass(rast,1,'0-2000:0-0,2000.0000000001-2100:5-5,2100.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg420add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg420 AS a , ysolg420neg AS b );

CREATE TABLE ysolg420addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg420add");

CREATE TABLE ysolg420addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg420addpoly"





CREATE TABLE ysolg421 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg421neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg421neg SET rast = ST_Reclass(rast,1,'0-2100:0-0,2100.0000000001-2200:5-5,2200.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg421 SET rast = ST_Reclass(rast,1,'0-2100:0-0,2100.0000000001-2200:5-5,2200.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg421add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg421 AS a , ysolg421neg AS b );

CREATE TABLE ysolg421addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg421add");

CREATE TABLE ysolg421addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg421addpoly"



CREATE TABLE ysolg422 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg422neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg422neg SET rast = ST_Reclass(rast,1,'0-2200:0-0,2200.0000000001-2300:5-5,2300.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg422 SET rast = ST_Reclass(rast,1,'0-2200:0-0,2200.0000000001-2300:5-5,2300.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg422add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg422 AS a , ysolg422neg AS b );

CREATE TABLE ysolg422addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg422add");

CREATE TABLE ysolg422addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg422addpoly"



CREATE TABLE ysolg423 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg423neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg423neg SET rast = ST_Reclass(rast,1,'0-2300:0-0,2300.0000000001-2400:5-5,2400.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg423 SET rast = ST_Reclass(rast,1,'0-2300:0-0,2300.0000000001-2400:5-5,2400.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg423add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg423 AS a , ysolg423neg AS b );

CREATE TABLE ysolg423addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg423add");

CREATE TABLE ysolg423addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg423addpoly"





CREATE TABLE ysolg424 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysolg424neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysolg424neg SET rast = ST_Reclass(rast,1,'0-2400:0-0,2400.0000000001-2500:5-5,2500.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysolg424 SET rast = ST_Reclass(rast,1,'0-2400:0-0,2400.0000000001-2500:5-5,2500.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysolg424add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysolg424 AS a , ysolg424neg AS b );

CREATE TABLE ysolg424addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysolg424add");

CREATE TABLE ysolg424addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysolg424addpoly"





CREATE TABLE ysole425 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole425neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole425neg SET rast = ST_Reclass(rast,1,'0-2500:0-0,2500.0000000001-2600:5-5,2600.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole425 SET rast = ST_Reclass(rast,1,'0-2500:0-0,2500.0000000001-2600:5-5,2600.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole425add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole425 AS a , ysole425neg AS b );

CREATE TABLE ysole425addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole425add");

CREATE TABLE ysole425addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole425addpoly"




CREATE TABLE ysole426 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole426neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole426neg SET rast = ST_Reclass(rast,1,'0-2600:0-0,2600.0000000001-2700:5-5,2700.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole426 SET rast = ST_Reclass(rast,1,'0-2600:0-0,2600.0000000001-2700:5-5,2700.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole426add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole426 AS a , ysole426neg AS b );

CREATE TABLE ysole426addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole426add");

CREATE TABLE ysole426addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole426addpoly"




CREATE TABLE ysole427 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole427neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole427neg SET rast = ST_Reclass(rast,1,'0-2700:0-0,2700.0000000001-2800:5-5,2800.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole427 SET rast = ST_Reclass(rast,1,'0-2700:0-0,2700.0000000001-2800:5-5,2800.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole427add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole427 AS a , ysole427neg AS b );

CREATE TABLE ysole427addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole427add");

CREATE TABLE ysole427addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole427addpoly"




CREATE TABLE ysole428 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole428neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole428neg SET rast = ST_Reclass(rast,1,'0-2800:0-0,2800.0000000001-2900:5-5,2900.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole428 SET rast = ST_Reclass(rast,1,'0-2800:0-0,2800.0000000001-2900:5-5,2900.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole428add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole428 AS a , ysole428neg AS b );

CREATE TABLE ysole428addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole428add");

CREATE TABLE ysole428addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole428addpoly"




CREATE TABLE ysole429 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole429neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole429neg SET rast = ST_Reclass(rast,1,'0-2900:0-0,2900.0000000001-3000:5-5,3000.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole429 SET rast = ST_Reclass(rast,1,'0-2900:0-0,2900.0000000001-3000:5-5,3000.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole429add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole429 AS a , ysole429neg AS b );

CREATE TABLE ysole429addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole429add");

CREATE TABLE ysole429addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole429addpoly"




CREATE TABLE ysole430 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole430neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole430neg SET rast = ST_Reclass(rast,1,'0-3000:0-0,3000.0000000001-3100:5-5,3100.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole430 SET rast = ST_Reclass(rast,1,'0-3000:0-0,3000.0000000001-3100:5-5,3100.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole430add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole430 AS a , ysole430neg AS b );

CREATE TABLE ysole430addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole430add");

CREATE TABLE ysole430addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole430addpoly"



CREATE TABLE ysole431 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole431neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole431neg SET rast = ST_Reclass(rast,1,'0-3100:0-0,3100.0000000001-3200:5-5,3200.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole431 SET rast = ST_Reclass(rast,1,'0-3100:0-0,3100.0000000001-3200:5-5,3200.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole431add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole431 AS a , ysole431neg AS b );

CREATE TABLE ysole431addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole431add");

CREATE TABLE ysole431addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole431addpoly"






CREATE TABLE ysole432 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole432neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole432neg SET rast = ST_Reclass(rast,1,'0-3200:0-0,3200.0000000001-3300:5-5,3300.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole432 SET rast = ST_Reclass(rast,1,'0-3200:0-0,3200.0000000001-3300:5-5,3300.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole432add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole432 AS a , ysole432neg AS b );

CREATE TABLE ysole432addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole432add");

CREATE TABLE ysole432addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole432addpoly"



CREATE TABLE ysole433 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole433neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole433neg SET rast = ST_Reclass(rast,1,'0-3300:0-0,3300.0000000001-3400:5-5,3400.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole433 SET rast = ST_Reclass(rast,1,'0-3300:0-0,3300.0000000001-3400:5-5,3400.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole433add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole433 AS a , ysole433neg AS b );

CREATE TABLE ysole433addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole433add");

CREATE TABLE ysole433addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole433addpoly"






CREATE TABLE ysole434 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole434neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole434neg SET rast = ST_Reclass(rast,1,'0-3400:0-0,3400.0000000001-3500:5-5,3500.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole434 SET rast = ST_Reclass(rast,1,'0-3400:0-0,3400.0000000001-3500:5-5,3500.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole434add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole434 AS a , ysole434neg AS b );

CREATE TABLE ysole434addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole434add");

CREATE TABLE ysole434addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole434addpoly"




CREATE TABLE ysole435 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole435neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole435neg SET rast = ST_Reclass(rast,1,'0-3500:0-0,3500.0000000001-3600:5-5,3600.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole435 SET rast = ST_Reclass(rast,1,'0-3500:0-0,3500.0000000001-3600:5-5,3600.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole435add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole435 AS a , ysole435neg AS b );

CREATE TABLE ysole435addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole435add");

CREATE TABLE ysole435addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole435addpoly"




CREATE TABLE ysole436 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole436neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole436neg SET rast = ST_Reclass(rast,1,'0-3600:0-0,3600.0000000001-3700:5-5,3700.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole436 SET rast = ST_Reclass(rast,1,'0-3600:0-0,3600.0000000001-3700:5-5,3700.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole436add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole436 AS a , ysole436neg AS b );

CREATE TABLE ysole436addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole436add");

CREATE TABLE ysole436addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole436addpoly"




CREATE TABLE ysole437 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole437neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole437neg SET rast = ST_Reclass(rast,1,'0-3700:0-0,3700.0000000001-3800:5-5,3800.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole437 SET rast = ST_Reclass(rast,1,'0-3700:0-0,3700.0000000001-3800:5-5,3800.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole437add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole437 AS a , ysole437neg AS b );

CREATE TABLE ysole437addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole437add");

CREATE TABLE ysole437addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole437addpoly"



CREATE TABLE ysole438 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole438neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole438neg SET rast = ST_Reclass(rast,1,'0-3800:0-0,3800.0000000001-3900:5-5,3900.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole438 SET rast = ST_Reclass(rast,1,'0-3800:0-0,3800.0000000001-3900:5-5,3900.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole438add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole438 AS a , ysole438neg AS b );

CREATE TABLE ysole438addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole438add");

CREATE TABLE ysole438addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole438addpoly"




CREATE TABLE ysole439 AS (SELECT * FROM "o_4_solar_atlas_dni");
CREATE TABLE ysole439neg AS (SELECT * FROM "o_4_solar_atlas_dni");

UPDATE ysole439neg SET rast = ST_Reclass(rast,1,'0-3900:0-0,3900.0000000001-4000:5-5,4000.0000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE ysole439 SET rast = ST_Reclass(rast,1,'0-3900:0-0,3900.0000000001-4000:5-5,4000.0000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE ysole439add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM ysole439 AS a , ysole439neg AS b );

CREATE TABLE ysole439addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "ysole439add");

CREATE TABLE ysole439addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "ysole439addpoly"



-- +++++++++++++
-- INTERSECTIONS: solar interval + poligon E o poligon G (with elevation and T considered)
-- ++++++++++++++

CREATE TABLE inter_solgeleclimateun AS (SELECT ST_Union(a.geom) as geom
FROM "inter_solgeleclimate" AS a);
CREATE TABLE inter_soleeleclimateun AS (SELECT ST_Union(a.geom) as geom
FROM "inter_soleeleclimate" AS a);


CREATE TABLE inter_solg20el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg420addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_solg21el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg421addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_solg22el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg422addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_solg23el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg423addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_solg24el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysolg424addpolydis" AS a, "yelesol4addpolydis" AS b);

CREATE TABLE inter_sole25el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole425addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole26el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole426addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole27el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole427addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole28el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole428addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole29el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole429addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole30el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole430addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole31el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole431addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole32el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole432addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole33el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole433addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole34el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole434addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole35el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole435addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole36el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole436addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole37el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole437addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole38el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole438addpolydis" AS a, "yelesol4addpolydis" AS b);
CREATE TABLE inter_sole39el AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "ysole439addpolydis" AS a, "yelesol4addpolydis" AS b);






CREATE TABLE inter_solg20elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solg20el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_solg21elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solg21el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_solg22elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solg22el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_solg23elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solg23el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_solg24elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_solg24el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole25elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole25el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_sole26elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole26el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_sole27elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole27el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole28elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole28el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole29elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole29el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole30elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole30el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole31elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole31el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole32elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole32el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole33elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole33el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole34elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole34el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole35elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole35el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_sole36elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole36el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;


CREATE TABLE inter_sole37elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole37el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;



CREATE TABLE inter_sole38elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole38el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;

CREATE TABLE inter_sole39elcl AS SELECT ST_UNION(a.geom) as geom, ST_Area( ST_UNION(a.geom)::geography, true)/1000000 as km2 FROM 
(SELECT 
      ST_Intersection(a.geom, b.wkb_geometry) as geom,
      b."Denominaci", 
      b."Tmed", 
      b."Alt_min", 
      b."Alt_max"
      FROM "inter_sole39el" AS a, 
            "IDE_climate" AS b 
      WHERE b."Tmed" > 11) as a;




SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solg20elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solg21elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solg22elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solg23elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solg24elcl";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_solgeleclimateun";


SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole25elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole26elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole27elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole28elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole29elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole30elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole31elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole32elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole33elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole34elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole35elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole36elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole37elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole38elcl";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_sole39elcl";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_soleeleclimate";









