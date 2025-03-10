(
    select
        building_iri,
        osm_id geometryProperty
    from
        "osm"."points"
        join buffered_points as bp on ST_Contains(
            buffer_geom,
            ST_Transform(geometryProperty, 4326)
        )
)
union all
(
    select
        building_iri,
        osm_id geometryProperty
    from
        "osm"."polygons"
        join buffered_points as bp on ST_Intersects(
            buffer_geom,
            ST_Transform(geometryProperty, 4326)
        )
)