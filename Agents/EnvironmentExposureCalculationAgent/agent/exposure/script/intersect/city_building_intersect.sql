select
    iri,
    geometry,
    buffer_geom,
    lat,
    lon
from
    "citydb"."bldg_mat_view"
    join buffered_points as bp on ST_Intersects(buffer_geom, ST_Transform(geometry, 4326))