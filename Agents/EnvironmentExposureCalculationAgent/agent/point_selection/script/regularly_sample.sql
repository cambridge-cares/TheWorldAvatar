DO $$ 
BEGIN
    -- Check if the table exists
    IF EXISTS (SELECT 1 FROM information_schema.tables 
               WHERE table_schema = 'env_exposure' 
               AND table_name = '%(point_table_name)s') THEN
        -- Exit the script if the table exists
        RAISE NOTICE 'Table %(point_table_name)s already exists. Exiting script.';
        RETURN;
    END IF;

    -- Create the schema if it doesn't exist
    CREATE SCHEMA IF NOT EXISTS "env_exposure";

    -- Create the table
    CREATE TABLE "env_exposure"."%(point_table_name)s" (
        id SERIAL PRIMARY KEY,
        lon FLOAT,
        lat FLOAT
    );

    -- Insert the data into the table
    INSERT INTO "env_exposure"."%(point_table_name)s" (lon, lat)
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
    SELECT x_series AS lon, y_series AS lat
    FROM x_series, y_series;
END $$;