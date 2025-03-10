-- with sel_points as (% buffer_func %),
-- intersect_city_building as (
--     select
--         iri,
--         geometry,
--         buffer_geom,
--         lat,
--         lon
--     from
--         "citydb"."bldg_mat_view"
--         join buffered_points as bp on ST_Intersects(buffer_geom, ST_Transform(geometry, 4326))
-- ),
-- intersect_osm as (
--     (
--         select
--             building_iri,
--             geometryProperty,
--             buffer_geom,
--             amenity,
--             leisure,
--             tourism,
--             office,
--             shop
--         from
--             "osm"."points"
--             join buffered_points as bp on ST_Contains(
--                 buffer_geom,
--                 ST_Transform(geometryProperty, 4326)
--             )
--     )
--     union
--     (
--         select
--             building_iri,
--             geometryProperty,
--             buffer_geom,
--             amenity,
--             leisure,
--             tourism,
--             office,
--             shop
--         from
--             "osm"."polygons"
--             join buffered_points as bp on ST_Intersects(
--                 buffer_geom,
--                 ST_Transform(geometryProperty, 4326)
--             )
--     )
-- )
-- select
--     *
-- from
--     "osm"."points" as "p"
--     join intersect_city_building as "icb" on position("p"."building_iri" in "icb"."iri") > 0


with intersect_city_building as (
    select
        iri,
        geometry,
        buffer_geom,
        lat,
        lon
    from
        "citydb"."bldg_mat_view"
        join buffered_points as bp on ST_Intersects(buffer_geom, ST_Transform(geometry, 4326))
),
intersect_osm as (
    (
        select
            building_iri,
            osm_id
            geometryProperty
        from
            "osm"."points"
            join buffered_points as bp on ST_Contains(
                buffer_geom,
                ST_Transform(geometryProperty, 4326)
            )
    )
    union
    (
        select
            building_iri,
            osm_id
            geometryProperty
        from
            "osm"."polygons"
            join buffered_points as bp on ST_Intersects(
                buffer_geom,
                ST_Transform(geometryProperty, 4326)
            )
    )
),
others as (
    select uuid 
    from historicsite as hs
    join buffered_points as bp on ST_Intersects(buffer_geom, ST_Transform(wkb_geometry, 4326))

)
select
    *
from
    "osm"."points" as "p"
    join intersect_city_building as "icb" on position("p"."building_iri" in "icb"."iri") > 0