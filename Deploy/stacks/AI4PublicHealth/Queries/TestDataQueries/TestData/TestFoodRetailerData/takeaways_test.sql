-- Adminer 4.8.1 PostgreSQL 15.4 (Debian 15.4-2.pgdg120+1) dump

DROP TABLE IF EXISTS "takeaways_test";
DROP SEQUENCE IF EXISTS takeaways_test_ogc_fid_seq;
CREATE SEQUENCE takeaways_test_ogc_fid_seq INCREMENT 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1;

CREATE TABLE "public"."takeaways_test" (
    "ogc_fid" integer DEFAULT nextval('takeaways_test_ogc_fid_seq') NOT NULL,
    "Name" character varying,
    "Address" character varying,
    "Postcode" character varying,
    "Coding" character varying,
    "Council" character varying,
    "Fenland_name" character varying,
    "Latitude" double precision,
    "Longitude" double precision,
    "Easting" integer,
    "Northing" integer,
    "geom" geometry(Point,4326),
    CONSTRAINT "takeaways_test_pkey" PRIMARY KEY ("ogc_fid")
) WITH (oids = false);

CREATE INDEX "takeaways_test_geom_geom_idx" ON "public"."takeaways_test" USING btree ("geom");

INSERT INTO "takeaways_test" ("ogc_fid", "Name", "Address", "Postcode", "Coding", "Council", "Fenland_name", "Latitude", "Longitude", "Easting", "Northing", "geom") VALUES
(165,	'Adams Fish and Pizza Bar',	'110 Bridge Road     ',	'PE129SA',	'Takeaway',	'South Holland',	'',	52.220310,	0.099244,	543498,	260087,	'0101000020E6100000580053060E68B93FAA7D3A1E331C4A40');