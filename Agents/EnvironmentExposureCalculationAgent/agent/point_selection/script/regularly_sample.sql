DO $$ 
BEGIN
    -- Check if the table exists
    IF EXISTS (SELECT 1 FROM information_schema.tables 
               WHERE table_schema = 'env_exposure' 
               AND table_name = 'point_table') THEN
        -- Exit the script if the table exists
        IF EXISTS (SELECT 1 FROM "env_exposure"."point_table" WHERE query_id = '%(query_id)s') THEN
            RAISE NOTICE 'Query result already exists. Exiting script.';
            RETURN;
        END IF;
    ELSE
        -- Create the schema if it doesn't exist
        CREATE SCHEMA IF NOT EXISTS "env_exposure";

        -- Create the table
        CREATE TABLE "env_exposure"."point_table" (
            id SERIAL PRIMARY KEY,
            query_id varchar,
            ogc_fid INTEGER,
            src_schema varchar,
            src_table varchar,
            wkb_geometry Geometry
        );
    END IF;

    -- Insert the data into the table
    INSERT INTO "env_exposure"."point_table" (query_id, wkb_geometry)
    WITH x_series AS (
        SELECT %(lng_start)s AS x_series
        WHERE %(lng_start)s = %(lng_end)s
        UNION ALL
        SELECT generate_series(%(lng_start)s, %(lng_end)s, %(lng_step)s) AS x_series
        WHERE %(lng_start)s <> %(lng_end)s
    ),
    y_series AS (
        SELECT %(lat_start)s AS y_series
        WHERE %(lat_start)s = %(lat_end)s
        UNION ALL
        SELECT generate_series(%(lat_start)s, %(lat_end)s, %(lat_step)s) AS y_series
        WHERE %(lat_start)s <> %(lat_end)s
    )
    SELECT '%(query_id)s' as query_id, ST_AsBinary(ST_SetSRID(ST_MakePoint(x_series, y_series), 4326)) as wkb_geometry
    FROM x_series, y_series;
END $$;