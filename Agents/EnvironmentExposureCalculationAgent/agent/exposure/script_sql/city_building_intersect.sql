select
    iri,
    p.id as point_id
from
    "citydb"."bldg_mat_view"
    join %(point_table_name)s as p on ST_Intersects(buffer_geom, ST_Transform(geometry, 4326))
-- need to identify what building it is corresponding to