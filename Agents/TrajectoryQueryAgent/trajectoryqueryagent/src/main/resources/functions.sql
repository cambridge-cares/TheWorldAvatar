CREATE OR REPLACE FUNCTION get_column_name(iri VARCHAR)
RETURNS VARCHAR
LANGUAGE sql
STABLE
AS $$
    SELECT b.data_type
    FROM time_series_data_iri a
    JOIN time_series_data_type b
        ON a.data_type_index = b.data_type_index
    WHERE a.data_iri = iri;
$$;

CREATE OR REPLACE FUNCTION get_user_id(device TEXT)
RETURNS VARCHAR AS
$$
DECLARE
    user_result VARCHAR;
BEGIN
    -- Check if the schema 'timeline' exists
    IF EXISTS (
        SELECT 1
        FROM pg_catalog.pg_namespace
        WHERE nspname = 'timeline'
    ) THEN
        -- Check if the table 'smartPhone' exists in the 'timeline' schema
        IF EXISTS (
            SELECT 1
            FROM pg_catalog.pg_tables
            WHERE schemaname = 'timeline'
              AND tablename = 'smartPhone'
        ) THEN
            -- Perform the query to get the user_id
            SELECT user_id
            INTO user_result
            FROM timeline."smartPhone"
            WHERE phone_id = device;
        ELSE
            -- Return NULL if the table does not exist
            RETURN NULL;
        END IF;
    ELSE
        -- Return NULL if the schema does not exist
        RETURN NULL;
    END IF;


    RETURN user_result;
END;
$$
LANGUAGE plpgsql;

-- time_series_data.time_as_number is stored in epoch seconds. Variable
-- helpers retain that source precision so joins happen before conversion.
CREATE OR REPLACE FUNCTION get_geom_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "geom" geometry
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_point_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I AS geom
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_speed_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "speed" double precision
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_speed_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I AS speed
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_altitude_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "altitude" double precision
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_altitude_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I AS altitude
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_bearing_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "bearing" double precision
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_bearing_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I AS bearing
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_session_id_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "session_id" character varying
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_session_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I AS session_id
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_location_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" bigint,
    "geom" geometry,
    "speed" double precision,
    "altitude" double precision,
    "bearing" double precision,
    "session_id" character varying,
    "device_id" TEXT,
    "user_id" TEXT
) AS $$
DECLARE
    current_device_id TEXT;
BEGIN
    FOREACH current_device_id IN ARRAY device_id_array LOOP
        RETURN QUERY
        SELECT
            ROUND(geom_data."time" * 1000.0)::bigint,
            geom_data.geom,
            speed_data.speed,
            altitude_data.altitude,
            bearing_data.bearing,
            session_data.session_id,
            current_device_id,
            get_user_id(current_device_id)::TEXT
        FROM get_geom_table(ARRAY[current_device_id]) AS geom_data
        LEFT JOIN get_speed_table(ARRAY[current_device_id]) AS speed_data
            ON speed_data."time" = geom_data."time"
        LEFT JOIN get_altitude_table(ARRAY[current_device_id]) AS altitude_data
            ON altitude_data."time" = geom_data."time"
        LEFT JOIN get_bearing_table(ARRAY[current_device_id]) AS bearing_data
            ON bearing_data."time" = geom_data."time"
        LEFT JOIN get_session_id_table(ARRAY[current_device_id]) AS session_data
            ON session_data."time" = geom_data."time";
    END LOOP;
END $$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_activity_type_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "activity_type" character varying
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_activity_type_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I::character varying AS activity_type
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_confidence_level_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" double precision,
    "confidence_level" integer
) AS $$
DECLARE
    query TEXT := '';
    column_name TEXT;
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        column_name := get_column_name(
            get_confidence_level_iri(device_id_array[i])
        );

        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time_as_number AS time, %I::integer AS confidence_level
             FROM time_series_data
             WHERE %I IS NOT NULL',
            column_name,
            column_name
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_activity_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" bigint,
    "activity_type" character varying,
    "confidence_level" integer,
    "device_id" TEXT,
    "user_id" TEXT
) AS $$
DECLARE
    current_device_id TEXT;
BEGIN
    FOREACH current_device_id IN ARRAY device_id_array LOOP
        RETURN QUERY
        SELECT
            ROUND(activity_data."time" * 1000.0)::bigint,
            activity_data.activity_type,
            confidence_data.confidence_level,
            current_device_id,
            get_user_id(current_device_id)::TEXT
        FROM get_activity_type_table(ARRAY[current_device_id]) AS activity_data
        LEFT JOIN get_confidence_level_table(ARRAY[current_device_id]) AS confidence_data
            ON confidence_data."time" = activity_data."time";
    END LOOP;
END $$ LANGUAGE plpgsql;




-- used by timeline app only
CREATE OR REPLACE FUNCTION get_device_ids(id VARCHAR)
RETURNS TEXT AS
$$
DECLARE
    phone_id_list TEXT[];
BEGIN
    -- Aggregate phone_id values into an array, but only if phone_id exists in the devices table
    SELECT array_agg(phone_id)
    INTO phone_id_list
    FROM timeline."smartPhone" sp
    WHERE sp.user_id = id
    AND EXISTS (
        SELECT 1
        FROM devices d
        WHERE d.device_id = sp.phone_id
    );


    RETURN phone_id_list;
END;
$$
LANGUAGE plpgsql;




CREATE OR REPLACE FUNCTION fill_activity_types(activity_types varchar[], times bigint[])
RETURNS TABLE (
    "time" bigint,
    "activity_type" VARCHAR
) AS $$
DECLARE
    result varchar[] := '{}';
    last_valid varchar := '';
    activity varchar;
    activity_time bigint;
    i integer := 1;
BEGIN

    FOREACH activity IN ARRAY activity_types
    LOOP
        IF activity <> 'others' THEN
            last_valid := activity;
            EXIT;
        END IF;
    END LOOP;


    FOREACH activity IN ARRAY activity_types
    LOOP
        IF activity <> 'others' THEN
            last_valid := activity;
        END IF;


        result := array_append(result, last_valid);
    END LOOP;


    FOR i IN 1..array_length(times, 1)
    LOOP
        activity_time := times[i];
        RETURN QUERY SELECT activity_time, result[i];
    END LOOP;


    RETURN;
END;
$$
LANGUAGE plpgsql
