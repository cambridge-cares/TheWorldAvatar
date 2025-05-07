DO $$ 
BEGIN
    -- Check if the table exists
    IF NOT EXISTS (SELECT 1 FROM information_schema.tables 
               WHERE table_schema = 'env_exposure' 
               AND table_name = 'results') THEN

        CREATE SCHEMA IF NOT EXISTS "env_exposure";

        -- Create the table
        CREATE TABLE "env_exposure"."results" (
            point_id INTEGER,
            query_id varchar,
            iri varchar,
            type_label varchar,
            area float default null
        );

        ALTER TABLE "env_exposure"."results"
        ADD CONSTRAINT unique_query_result UNIQUE (point_id, query_id, iri);
    END IF;

    INSERT INTO  "env_exposure"."results" (point_id, query_id, iri, type_label)
    WITH query_points AS (
        SELECT
            id,
            query_id,
            buffer_geom AS buffer_geom
        FROM
            "env_exposure"."point_table"
        WHERE
            query_id = %(query_id)s
    )
    SELECT
        qp.id as point_id,
        qp.query_id,
        points.iri,
        points.type_label
    FROM
        "env_exposure"."%(feature_table_name)s" as points
        JOIN query_points as qp ON ST_Intersects(
            qp.buffer_geom,
            points.geom
        )
    ON CONFLICT (point_id, query_id, iri) DO NOTHING;
END
$$