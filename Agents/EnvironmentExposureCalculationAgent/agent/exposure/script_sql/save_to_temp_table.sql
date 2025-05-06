DROP TABLE IF EXISTS "env_exposure"."%(feature_table_name)s";

CREATE TABLE "env_exposure"."%(feature_table_name)s" (
    iri VARCHAR,
    geom geometry(%(geom_type)s, 4326),
    type_label VARCHAR
);

INSERT INTO "env_exposure"."%(feature_table_name)s" (iri, geom, type_label) VALUES
%s
ON CONFLICT DO NOTHING;