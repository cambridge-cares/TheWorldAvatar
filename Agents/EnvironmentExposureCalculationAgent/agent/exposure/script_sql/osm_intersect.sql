DO $$ 
BEGIN
IF EXISTS (SELECT 1 from information_schema.tables where table_schema = 'env_exposure' AND table_name = '%(result_table_name)s')
THEN 
    RAISE NOTICE 'Table %(point_table_name)s already exists. Exiting script.';
    RETURN;
END IF;

DROP TABLE IF EXISTS "env_exposure"."%(result_table_name)s";
CREATE TABLE "env_exposure"."%(result_table_name)s" (
    iri TEXT,
    osm_id VARCHAR,
    point_id INTEGER
);

INSERT INTO "env_exposure"."%(result_table_name)s" (iri, osm_id, point_id)
(
    select
        building_iri as iri,
        osm_id,
        p.id as point_id
    from
        "osm"."points" as points
        join "env_exposure"."%(point_table_name)s" as p on ST_Contains(
            buffer_geom,
            ST_Transform("geometryProperty", 4326)
        )
)
union all
(
    select
        building_iri as iri,
        osm_id,
        p.id as point_id
    from
        "osm"."polygons" as polygons
        join "env_exposure"."%(point_table_name)s" as p on ST_Intersects(
            buffer_geom,
            ST_Transform("geometryProperty", 4326)
        )
);

END $$

-- SELECT column_name, data_type, is_nullable, column_default 
-- FROM information_schema.columns 
-- WHERE table_schema = 'osm' 
-- AND table_name = 'points';