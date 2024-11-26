-- Adminer 4.8.1 PostgreSQL 15.4 (Debian 15.4-2.pgdg120+1) dump

DROP TABLE IF EXISTS "gps_test_case_1_data";
DROP SEQUENCE IF EXISTS gps_test_case_1_data_ogc_fid_seq;
CREATE SEQUENCE gps_test_case_1_data_ogc_fid_seq INCREMENT 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1;

CREATE TABLE "public"."gps_test_case_1_data" (
    "ogc_fid" integer DEFAULT nextval('gps_test_case_1_data_ogc_fid_seq') NOT NULL,
    "INDEX" integer,
    "RCR" character varying,
    "UTC DATE" date,
    "UTC TIME" time without time zone,
    "LOCAL DATE" date,
    "LOCAL TIME" time without time zone,
    "MS" integer,
    "VALID" character varying,
    "LATITUDE" double precision,
    "N/S" character varying,
    "LONGITUDE" double precision,
    "E/W" character varying,
    "HEIGHT" character varying,
    "SPEED" character varying,
    "HEADING" integer,
    "DSTA" integer,
    "DAGE" integer,
    "PDOP" double precision,
    "HDOP" double precision,
    "VDOP" double precision,
    "NSAT(USED/VIEW)" character varying,
    "SAT INFO (SID-ELE-AZI-SNR)" character varying,
    "DISTANCE" character varying,
    "geom" geometry(Point,4326),
    CONSTRAINT "gps_test_case_1_data_pkey" PRIMARY KEY ("ogc_fid")
) WITH (oids = false);

CREATE INDEX "gps_test_case_1_data_geom_geom_idx" ON "public"."gps_test_case_1_data" USING btree ("geom");

INSERT INTO "gps_test_case_1_data" ("ogc_fid", "INDEX", "RCR", "UTC DATE", "UTC TIME", "LOCAL DATE", "LOCAL TIME", "MS", "VALID", "LATITUDE", "N/S", "LONGITUDE", "E/W", "HEIGHT", "SPEED", "HEADING", "DSTA", "DAGE", "PDOP", "HDOP", "VDOP", "NSAT(USED/VIEW)", "SAT INFO (SID-ELE-AZI-SNR)", "DISTANCE", "geom") VALUES
(702,337,' T','2015-05-22','18:57:18','2015-05-22','18:57:18',0,' SPS',52.21957191,' N',0.09964365,' E',' 76.536 M',' 43.971 km/h',0,0,0,1.38,1.13,0.8,'  8/10',' 07-00-00-20;22-00-00-16;26-00-00-22;21-00-00-33;19-00-00-28;27-00-00-30;18-00-00-19;29-00-00-00;20-00-00-00;16-00-00-23',' 125.10 M','0101000020E6100000C20A010A3F82B93FFE48AEEE1A1C4A40'),
(703,338,' T','2015-05-22','18:57:28','2015-05-22','18:57:28',0,' SPS',52.22031022,' N',0.09819477,' E',' 74.233 M',' 41.097 km/h',0,0,0,1.38,1.13,0.8,'  8/10',' 07-00-00-19;22-00-00-15;26-00-00-24;21-00-00-33;19-00-00-20;27-00-00-21;18-00-00-22;29-00-00-00;20-00-00-00;16-00-00-27',' 128.68 M','0101000020E610000039CAC9DD4A23B93FF2EF1220331C4A40'),
(704,339,' T','2015-05-22','18:57:38','2015-05-22','18:57:38',0,' SPS',52.22103915,' N',0.09677016,' E',' 71.299 M',' 47.775 km/h',0,0,0,1.38,1.13,0.8,'  8/10',' 07-00-00-16;22-00-00-16;26-00-00-23;21-00-00-34;19-00-00-20;27-00-00-26;18-00-00-21;29-00-00-00;20-00-00-00;16-00-00-22',' 126.75 M','0101000020E610000074BE6DE0EDC5B83F5D31C8024B1C4A40'),
(705,340,' T','2015-05-22','18:57:48','2015-05-22','18:57:48',0,' SPS',52.22180425,' N',0.09532332,' E',' 67.905 M',' 43.335 km/h',0,0,0,1.38,1.13,0.8,'  8/10',' 07-00-00-19;22-00-00-18;26-00-00-21;21-00-00-34;19-00-00-30;27-00-00-29;18-00-00-24;29-00-00-00;20-00-00-00;16-00-00-23',' 130.52 M','0101000020E61000006936F2ED1B67B83F91EEE714641C4A40');