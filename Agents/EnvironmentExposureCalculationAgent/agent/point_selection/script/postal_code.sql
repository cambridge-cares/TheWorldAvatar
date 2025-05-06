DO $$ 
BEGIN
    -- Check if the table exists
    IF EXISTS (SELECT 1 FROM information_schema.tables 
               WHERE table_schema = 'env_exposure' 
               AND table_name = 'point_table') THEN
        -- Exit the script if the table exists
        IF EXISTS (SELECT 1 FROM "env_exposure"."point_table" WHERE query_id = %(query_id)s) THEN
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
            wkb_geometry geometry(Geometry, 4326),
            buffer_geom geometry(Geometry, 4326)
        );
    END IF;

    INSERT INTO "env_exposure"."point_table" (query_id, src_schema, src_table, ogc_fid, wkb_geometry)
    SELECT %(query_id)s as query_id, 'public' as src_schema, 'sgpostcode' as src_table, ogc_fid, wkb_geometry
    FROM "public"."sgpostcode";


END $$;