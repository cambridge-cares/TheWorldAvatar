ALTER TABLE "env_exposure"."%(point_table_name)s" DROP COLUMN IF EXISTS buffer_geom;
ALTER TABLE "env_exposure"."%(point_table_name)s" ADD COLUMN buffer_geom Geometry;
UPDATE "env_exposure"."%(point_table_name)s"
SET buffer_geom = ST_Transform(
    ST_Buffer(
        ST_Transform(
            ST_SetSRID(ST_MakePoint(lon, lat), 4326),
            3857
        ),
        %(buffer_radius)s
    ),
    4326
);