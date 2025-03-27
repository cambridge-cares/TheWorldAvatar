DROP TABLE IF EXISTS "env_exposure"."%(result_table_name)s";
CREATE TABLE "env_exposure"."%(result_table_name)s" (
    iri TEXT,
    point_id INTEGER
);

INSERT INTO "env_exposure"."%(result_table_name)s" (iri, point_id)
select
    iri,
    p.id as point_id
from
    "citydb"."bldg_mat_view"
    join "env_exposure"."%(point_table_name)s" as p on ST_Intersects(buffer_geom, ST_Transform(geometry, 4326))
-- need to identify what building it is corresponding to