(
    select
        building_iri,
        osm_id,
        p.id as point_id
    from
        "osm"."points" as points
        join %(point_table_name)s as p on ST_Contains(
            buffer_geom,
            ST_Transform("geometryProperty", 4326)
        )
)
union all
(
    select
        building_iri,
        osm_id,
        p.id as point_id
    from
        "osm"."polygons" as polygons
        join %(point_table_name)s as p on ST_Intersects(
            buffer_geom,
            ST_Transform("geometryProperty", 4326)
        )
)

-- SELECT column_name, data_type, is_nullable, column_default 
-- FROM information_schema.columns 
-- WHERE table_schema = 'osm' 
-- AND table_name = 'points';