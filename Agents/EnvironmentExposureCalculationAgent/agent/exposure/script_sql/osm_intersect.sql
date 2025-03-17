(
    select
        building_iri,
        osm_id,
        p.id as point_id
    from
        "osm"."points"
        join %(point_table_name)s as p on ST_Contains(
            buffer_geom,
            ST_Transform(geometryProperty, 4326)
        )
)
union all
(
    select
        building_iri,
        osm_id,
        p.id as point_id
    from
        "osm"."polygons"
        join %(point_table_name)s as p on ST_Intersects(
            buffer_geom,
            ST_Transform(geometryProperty, 4326)
        )
)