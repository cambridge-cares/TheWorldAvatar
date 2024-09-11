CREATE OR REPLACE FUNCTION get_column_name(iri VARCHAR)
RETURNS VARCHAR AS
$$
DECLARE
    column_name_result VARCHAR;
BEGIN
    SELECT column_name
    INTO column_name_result
    FROM time_series_quantities
    WHERE data_iri = iri;

    RETURN column_name_result;
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_table_name(iri VARCHAR)
RETURNS VARCHAR AS
$$
DECLARE
    table_name_result VARCHAR;
BEGIN
    SELECT table_name
    INTO table_name_result
    FROM time_series_quantities
    WHERE data_iri = iri;

    RETURN table_name_result;
END;
$$
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_time_series(iri VARCHAR)
RETURNS VARCHAR AS
$$
DECLARE
    time_series_result VARCHAR;
BEGIN
    SELECT time_series_iri
    INTO time_series_result
    FROM time_series_quantities
    WHERE data_iri = iri;

    RETURN time_series_result;
END;
$$
LANGUAGE plpgsql;

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


CREATE OR REPLACE FUNCTION get_location_table(
    device_id_array TEXT[]
)
RETURNS TABLE (
    "time" bigint, 
    "geom" geometry, 
    "speed" double precision, 
    "altitude" double precision, 
    "bearing" double precision,
    "device_id" TEXT,
    "user_id" TEXT
) AS $$
DECLARE
    query TEXT := '';
BEGIN
    FOR i IN 1..array_length(device_id_array, 1) LOOP
        IF i > 1 THEN
            query := query || ' UNION ALL ';
        END IF;

        query := query || format(
            'SELECT time, %I AS geom, %I AS speed, %I AS altitude, %I AS bearing, %L AS device_id, %L AS user_id FROM %I WHERE time_series_iri=%L',
            get_column_name(get_point_iri(device_id_array[i])),
            get_column_name(get_speed_iri(device_id_array[i])),
            get_column_name(get_altitude_iri(device_id_array[i])),
            get_column_name(get_bearing_iri(device_id_array[i])),
            device_id_array[i],
            get_user_id(device_id_array[i]),
            get_table_name(get_point_iri(device_id_array[i])),
            get_time_series(get_point_iri(device_id_array[i]))
        );
    END LOOP;

    RETURN QUERY EXECUTE query;
END $$ LANGUAGE plpgsql;

-- used by timeline app only
CREATE OR REPLACE FUNCTION get_device_ids(id VARCHAR)
RETURNS TEXT AS
$$
DECLARE
    phone_id_list TEXT[];
BEGIN
    -- Aggregate phone_id values into an array
    SELECT array_agg(phone_id)
    INTO phone_id_list
    FROM timeline."smartPhone"
    WHERE user_id = id;

    RETURN phone_id_list;
END;
$$
LANGUAGE plpgsql;
