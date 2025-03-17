ALTER TABLE %(point_table_name)s ADD COLUMN buffer_geom Geometry;
UPDATE %(point_table_name)s
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
