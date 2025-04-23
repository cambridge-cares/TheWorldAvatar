UPDATE "env_exposure"."point_table"
SET buffer_geom = ST_Transform(
    ST_Buffer(
        ST_Transform(
            ST_SetSRID(ST_GeomFromWKB(wkb_geometry), 4326),
            3857
        ),
        %(buffer_radius)s
    ),
    4326
)
WHERE query_id = %(query_id)s;