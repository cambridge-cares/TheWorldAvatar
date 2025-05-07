DO $$ 
BEGIN
    -- Check if the table exists
    IF NOT EXISTS (SELECT 1 FROM information_schema.tables 
               WHERE table_schema = 'env_exposure' 
               AND table_name = 'results') THEN
        -- Create the schema if it doesn't exist
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

    INSERT INTO "env_exposure"."results" (point_id, query_id, iri, type_label, area)
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
        qp.id,
        qp.query_id,
        poly.iri,
        poly.type_label,
        ST_Area(
            ST_Intersection(
                ST_Transform(qp.buffer_geom, 3857),
                ST_Transform(poly.geom, 3857)
            )
        ) as area
    FROM
        "env_exposure"."%(feature_table_name)s" as poly
        JOIN query_points as qp ON ST_Intersects(
            qp.buffer_geom,
            poly.geom
        )
    ON CONFLICT (point_id, query_id, iri) DO NOTHING;
END
$$