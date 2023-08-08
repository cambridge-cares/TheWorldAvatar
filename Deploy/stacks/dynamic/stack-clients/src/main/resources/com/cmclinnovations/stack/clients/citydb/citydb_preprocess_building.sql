SET search_path TO public,citydb;
create table "public"."raw_building" AS
(SELECT "{IDval}", 'building_' || gen_random_uuid() AS "gmlid", ST_Translate(ST_Extrude("{footprint}",0,0,"{height}"),0,0,"{elevation}") AS "geom", "{height}" AS "mh"
FROM "public"."{table}" WHERE ST_Area("{footprint}")>{minArea} AND "{height}" IS NOT NULL);
create table "public"."raw_surface" AS
(SELECT "building_gmlid", 'surface_' || gen_random_uuid() AS "gmlid", "class", "geom" FROM
(SELECT "building_gmlid", "geom",
    CASE WHEN ST_Zmin("geom")=ST_Zmax("geom") THEN
        CASE WHEN ST_Zmin("geom")="bzl" THEN 35 ELSE 33 END
    ELSE 34 END AS "class" FROM
(SELECT "gmlid" AS "building_gmlid", ST_Zmin("geom") AS "bzl", (ST_Dump(ST_CollectionExtract("geom"))).geom AS "geom"
FROM "public"."raw_building") AS "table1") AS "table2");