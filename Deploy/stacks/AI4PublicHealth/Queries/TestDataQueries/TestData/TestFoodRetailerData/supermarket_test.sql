-- Adminer 4.8.1 PostgreSQL 15.4 (Debian 15.4-2.pgdg120+1) dump

DROP TABLE IF EXISTS "supermarket_test";
DROP SEQUENCE IF EXISTS supermarket_test_ogc_fid_seq;
CREATE SEQUENCE supermarket_test_ogc_fid_seq INCREMENT 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1;

CREATE TABLE "public"."supermarket_test" (
    "ogc_fid" integer DEFAULT nextval('supermarket_test_ogc_fid_seq') NOT NULL,
    "Name" character varying,
    "Address" character varying,
    "Postcode" character varying,
    "Coding" character varying,
    "Council" character varying,
    "Latitude" double precision,
    "Longitude" double precision,
    "Easting" double precision,
    "Northing" double precision,
    "geom" geometry(Point,4326),
    CONSTRAINT "supermarket_test_pkey" PRIMARY KEY ("ogc_fid")
) WITH (oids = false);

CREATE INDEX "supermarket_test_geom_geom_idx" ON "public"."supermarket_test" USING btree ("geom");

INSERT INTO "supermarket_test" ("ogc_fid", "Name", "Address", "Postcode", "Coding", "Council", "Latitude", "Longitude", "Easting", "Northing", "geom") VALUES
(1,	'Test Supermarket',	'Test Case Road',	'Test Case Postcode',	'Supermarket',	'Test Council',	52.221104,	0.095523,	543242,	260168,	'0101000020E6100000FE0B04013274B83FE481C8224D1C4A40');

-- 2024-05-21 15:32:23.57535+00
