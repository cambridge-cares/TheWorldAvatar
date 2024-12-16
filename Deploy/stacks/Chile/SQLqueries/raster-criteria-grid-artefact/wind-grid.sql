-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.

-- ++++++++++++++++++
-- __________________

-- GRID USING REDUCED RESOLUTIONS
-- __________________

-- ++++++++++++++++++



SELECT  ST_CreateOverview('atlas_elevationwithbathymetry'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlas_capacity_iec1'::regclass, 'rast', 4, 'NearestNeighbor');
SELECT  ST_CreateOverview('atlaswind50m'::regclass, 'rast', 4, 'NearestNeighbor');
-- o_4_atlas_elevationwithbathymetry
-- o_4_atlas_capacity_iec1
-- o_4_atlaswind50m







CREATE TABLE xwchiles5 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles5neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles6 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles6neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles7 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles7neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles8 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles8neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles9 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles9neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles10 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles10neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles11 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles11neg AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles12 AS (SELECT * FROM "o_4_atlaswind50m");
CREATE TABLE xwchiles12neg AS (SELECT * FROM "o_4_atlaswind50m");


CREATE TABLE cap95 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap95neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap90 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap90neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap85 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap85neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap80 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap80neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap75 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap75neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap70 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap70neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap65 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap65neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap60 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap60neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap55 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap55neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap50 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap50neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap45 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap45neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap40 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap40neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap35 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap35neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");

CREATE TABLE cap30 AS (SELECT * FROM "o_4_atlas_capacity_iec1");
CREATE TABLE cap30neg AS (SELECT * FROM "o_4_atlas_capacity_iec1");



UPDATE xwchiles5neg SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-5.5:1-1,5.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles5 SET rast = ST_Reclass(rast,1,'0-4.5:0-0,4.500000001-5.5:1-1,5.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles5add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles5 AS a , xwchiles5neg AS b );


UPDATE xwchiles6neg SET rast = ST_Reclass(rast,1,'0-5.5:0-0,5.500000001-6.5:1-1,6.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles6 SET rast = ST_Reclass(rast,1,'0-5.5:0-0,5.500000001-6.5:1-1,6.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles6add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles6 AS a , xwchiles6neg AS b );


UPDATE xwchiles7neg SET rast = ST_Reclass(rast,1,'0-6.5:0-0,6.500000001-7.5:1-1,7.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles7 SET rast = ST_Reclass(rast,1,'0-6.5:0-0,6.500000001-7.5:1-1,7.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles7add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles7 AS a , xwchiles7neg AS b );

UPDATE xwchiles8neg SET rast = ST_Reclass(rast,1,'0-7.5:0-0,7.500000001-8.5:1-1,8.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles8 SET rast = ST_Reclass(rast,1,'0-7.5:0-0,7.500000001-8.5:1-1,8.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles8add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles8 AS a , xwchiles8neg AS b );


UPDATE xwchiles9neg SET rast = ST_Reclass(rast,1,'0-8.5:0-0,8.500000001-9.5:1-1,9.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles9 SET rast = ST_Reclass(rast,1,'0-8.5:0-0,8.500000001-9.5:1-1,9.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles9add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles9 AS a , xwchiles9neg AS b );


UPDATE xwchiles10neg SET rast = ST_Reclass(rast,1,'0-9.5:0-0,9.500000001-10.5:1-1,10.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles10 SET rast = ST_Reclass(rast,1,'0-9.5:0-0,9.500000001-10.5:1-1,10.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles10add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles10 AS a , xwchiles10neg AS b );


UPDATE xwchiles11neg SET rast = ST_Reclass(rast,1,'0-10.5:0-0,10.500000001-11.5:1-1,11.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles11 SET rast = ST_Reclass(rast,1,'0-10.5:0-0,10.500000001-11.5:1-1,11.5000000001-12.5:0-0,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles11add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles11 AS a , xwchiles11neg AS b );

UPDATE xwchiles12neg SET rast = ST_Reclass(rast,1,'0-11.5:0-0,11.500000001-12.5:1-1,12.50000000001-100:0-0 ', '32BF'::text,NULL);
UPDATE xwchiles12 SET rast = ST_Reclass(rast,1,'0-11.5:0-0,11.500000001-12.5:1-1,12.50000000001-100:0-0 ', '32BF'::text,0);

CREATE TABLE xwchiles12add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xwchiles12 AS a , xwchiles12neg AS b );



UPDATE cap95neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.5:0-0,0.5000000001-0.95:0-0,0.950000000001-1:1-1 ', '32BF'::text,NULL);
UPDATE cap95 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.5:0-0,0.5000000001-0.95:0-0,0.950000000001-1:1-1 ', '32BF'::text,0);

CREATE TABLE cap95add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap95 AS a , cap95neg AS b );

CREATE TABLE cap95addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap95add");

CREATE TABLE cap95addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap95addpoly";

UPDATE cap90neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.9:0-0,0.9000000001-0.95:1-1,0.950000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap90 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.9:0-0,0.9000000001-0.95:1-1,0.950000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap90add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap90 AS a , cap90neg AS b );

CREATE TABLE cap90addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap90add");

CREATE TABLE cap90addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap90addpoly";

UPDATE cap85neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.85:0-0,0.85000000001-0.9:1-1,0.90000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap85 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.85:0-0,0.85000000001-0.9:1-1,0.90000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap85add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap85 AS a , cap85neg AS b );

CREATE TABLE cap85addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap85add");

CREATE TABLE cap85addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap85addpoly";


UPDATE cap80neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.80:0-0,0.80000000001-0.85:1-1,0.85000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap80 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.80:0-0,0.80000000001-0.85:1-1,0.85000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap80add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap80 AS a , cap80neg AS b );

CREATE TABLE cap80addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap80add");

CREATE TABLE cap80addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap80addpoly";


UPDATE cap75neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.75:0-0,0.75000000001-0.8:1-1,0.80000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap75 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.75:0-0,0.75000000001-0.8:1-1,0.80000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap75add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap75 AS a , cap75neg AS b );

CREATE TABLE cap75addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap75add");

CREATE TABLE cap75addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap75addpoly";


UPDATE cap70neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.70:0-0,0.70000000001-0.75:1-1,0.75000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap70 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.70:0-0,0.70000000001-0.75:1-1,0.75000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap70add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap70 AS a , cap70neg AS b );

CREATE TABLE cap70addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap70add");

CREATE TABLE cap70addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap70addpoly";


UPDATE cap65neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.65:0-0,0.65000000001-0.7:1-1,0.70000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap65 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.65:0-0,0.65000000001-0.7:1-1,0.70000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap65add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap65 AS a , cap65neg AS b );

CREATE TABLE cap65addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap65add");

CREATE TABLE cap65addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap65addpoly";


UPDATE cap60neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.60:0-0,0.60000000001-0.65:1-1,0.65000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap60 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.60:0-0,0.60000000001-0.65:1-1,0.65000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap60add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap60 AS a , cap60neg AS b );

CREATE TABLE cap60addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap60add");

CREATE TABLE cap60addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap60addpoly";


UPDATE cap55neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.55:0-0,0.55000000001-0.6:1-1,0.60000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap55 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.55:0-0,0.55000000001-0.6:1-1,0.60000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap55add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap55 AS a , cap55neg AS b );

CREATE TABLE cap55addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap55add");

CREATE TABLE cap55addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap55addpoly";


UPDATE cap50neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.50:0-0,0.50000000001-0.55:1-1,0.55000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap50 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.50:0-0,0.50000000001-0.55:1-1,0.55000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap50add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap50 AS a , cap50neg AS b );

CREATE TABLE cap50addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap50add");

CREATE TABLE cap50addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap50addpoly";



UPDATE cap45neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.45:0-0,0.45000000001-0.5:1-1,0.50000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap45 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.45:0-0,0.45000000001-0.5:1-1,0.50000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap45add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap45 AS a , cap45neg AS b );

CREATE TABLE cap45addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap45add");

CREATE TABLE cap45addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap45addpoly";


UPDATE cap40neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.40:0-0,0.40000000001-0.45:1-1,0.45000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap40 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.40:0-0,0.40000000001-0.45:1-1,0.45000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap40add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap40 AS a , cap40neg AS b );

CREATE TABLE cap40addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap40add");

CREATE TABLE cap40addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap40addpoly";


UPDATE cap35neg SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.35:0-0,0.35000000001-0.4:1-1,0.40000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap35 SET rast = ST_Reclass(rast,1,'0-0.33:0-0,0.3300000001-0.35:0-0,0.35000000001-0.4:1-1,0.40000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap35add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap35 AS a , cap35neg AS b );

CREATE TABLE cap35addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap35add");

CREATE TABLE cap35addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap35addpoly";


UPDATE cap30neg SET rast = ST_Reclass(rast,1,'0-0.23:0-0,0.2300000001-0.30:0-0,0.30000000001-0.35:1-1,0.40000000001-1:0-0 ', '32BF'::text,NULL);
UPDATE cap30 SET rast = ST_Reclass(rast,1,'0-0.23:0-0,0.2300000001-0.30:0-0,0.30000000001-0.35:1-1,0.40000000001-1:0-0 ', '32BF'::text,0);

CREATE TABLE cap30add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM cap30 AS a , cap30neg AS b );

CREATE TABLE cap30addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "cap30add");

CREATE TABLE cap30addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "cap30addpoly";








CREATE TABLE xwchiles5addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles5add");

CREATE TABLE xwchiles5addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles5addpoly";

CREATE TABLE xwchiles6addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles6add");

CREATE TABLE xwchiles6addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles6addpoly";

CREATE TABLE xwchiles7addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles7add");

CREATE TABLE xwchiles7addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles7addpoly";

CREATE TABLE xwchiles8addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles8add");

CREATE TABLE xwchiles8addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles8addpoly";


CREATE TABLE xwchiles9addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles9add");

CREATE TABLE xwchiles9addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles9addpoly";


CREATE TABLE xwchiles10addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles10add");

CREATE TABLE xwchiles10addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles10addpoly";


CREATE TABLE xwchiles11addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles11add");

CREATE TABLE xwchiles11addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles11addpoly";


CREATE TABLE xwchiles12addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xwchiles12add");

CREATE TABLE xwchiles12addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xwchiles12addpoly";





CREATE TABLE xele4 AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");
CREATE TABLE xele4neg AS (SELECT * FROM "o_4_atlas_elevationwithbathymetry");

UPDATE xele4neg SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,NULL);
UPDATE xele4 SET rast = ST_Reclass(rast,1,'-5000-4.5:0-0,20-500:5-5,12.50000000001-7000:0-0 ', '32BF'::text,0);

CREATE TABLE xele4add AS (SELECT ST_MapAlgebra(a.rast, b.rast, '[rast1] * [rast2]') rast 
FROM xele4 AS a , xele4neg AS b );

CREATE TABLE xele4addpoly AS ( SELECT (ST_DumpAsPolygons(rast,1, TRUE)).geom as geom, (ST_DumpAsPolygons(rast,1, TRUE)).val as value
FROM "xele4add");

CREATE TABLE xele4addpolydis AS SELECT ST_UNION(ST_MAKEVALID(geom)) AS geom
FROM "xele4addpoly"


-- ++++++++++
-- INTERSECTION ELEVATION AND CAPACITY FACTOR
-- +++++++++++

CREATE TABLE inter_95ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap95addpolydis" AS b);
CREATE TABLE inter_90ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap90addpolydis" AS b);
CREATE TABLE inter_85ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap85addpolydis" AS b);
CREATE TABLE inter_80ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap80addpolydis" AS b);
CREATE TABLE inter_75ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap75addpolydis" AS b);
CREATE TABLE inter_70ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap70addpolydis" AS b);
CREATE TABLE inter_65ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap65addpolydis" AS b);
CREATE TABLE inter_60ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap60addpolydis" AS b);
CREATE TABLE inter_55ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap55addpolydis" AS b);
CREATE TABLE inter_50ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap50addpolydis" AS b);
CREATE TABLE inter_45ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap45addpolydis" AS b);
CREATE TABLE inter_40ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap40addpolydis" AS b);
CREATE TABLE inter_35ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap35addpolydis" AS b);
CREATE TABLE inter_30ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "cap30addpolydis" AS b);





-- ++++++++++
-- INTERSECTION ELEVATION AND WIND SPEED
-- +++++++++++


CREATE TABLE inter_s5ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles5addpolydis" AS b);
CREATE TABLE inter_s6ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles6addpolydis" AS b);
CREATE TABLE inter_s7ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles7addpolydis" AS b);
CREATE TABLE inter_s8ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles8addpolydis" AS b);
CREATE TABLE inter_s9ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles9addpolydis" AS b);
CREATE TABLE inter_s10ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles10addpolydis" AS b);
CREATE TABLE inter_s11ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles11addpolydis" AS b);
CREATE TABLE inter_s12ele AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "xele4addpolydis" AS a, "xwchiles12addpolydis" AS b);



-- ++++++++++
-- INTERSECTION (ELEVATION AND WIND SPEED) and (ELEVATION AND CAPACITY FACTOR)
-- +++++++++++



CREATE TABLE inter_95s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_95s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_95s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_95s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_95s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_95s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_95s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_95s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_95ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_90s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_90s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_90s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_90s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_90s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_90s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_90s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_90s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_90ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_85s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_85s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_85s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_85s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_85s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_85s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_85s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_85s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_85ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_80s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_80s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_80s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_80s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_80s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_80s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_80s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_80s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_80ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_75s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_75s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_75s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_75s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_75s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_75s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_75s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_75s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_75ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_70s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_70s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_70s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_70s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_70s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_70s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_70s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_70s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_70ele" AS a, "inter_s12ele" AS b);



CREATE TABLE inter_65s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_65s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_65s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_65s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_65s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_65s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_65s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_65s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_65ele" AS a, "inter_s12ele" AS b);


CREATE TABLE inter_60s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_60s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_60s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_60s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_60s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_60s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_60s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_60s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_60ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_55s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_55s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_55s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_55s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_55s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_55s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_55s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_55s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_55ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_50s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_50s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_50s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_50s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_50s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_50s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_50s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_50s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_50ele" AS a, "inter_s12ele" AS b);


CREATE TABLE inter_45s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_45s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_45s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_45s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_45s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_45s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_45s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_45s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_45ele" AS a, "inter_s12ele" AS b);

CREATE TABLE inter_40s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_40s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_40s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_40s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_40s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_40s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_40s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_40s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_40ele" AS a, "inter_s12ele" AS b);



CREATE TABLE inter_35s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_35s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_35s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_35s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_35s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_35s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_35s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_35s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_35ele" AS a, "inter_s12ele" AS b);


CREATE TABLE inter_30s5 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s5ele" AS b);
CREATE TABLE inter_30s6 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s6ele" AS b);
CREATE TABLE inter_30s7 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s7ele" AS b);
CREATE TABLE inter_30s8 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s8ele" AS b);
CREATE TABLE inter_30s9 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s9ele" AS b);
CREATE TABLE inter_30s10 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s10ele" AS b);
CREATE TABLE inter_30s11 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s11ele" AS b);
CREATE TABLE inter_30s12 AS (SELECT ST_Intersection(a.geom, b.geom) as geom
FROM "inter_30ele" AS a, "inter_s12ele" AS b);







SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_95s12";


SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_90s12";


SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_85s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_80s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_75s12";


SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_70s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_65s12";


SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_60s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_55s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_50s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_45s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_40s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_35s12";

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s5";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s6";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s7";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s8";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s9";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s10";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s11";
SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM "inter_30s12";






