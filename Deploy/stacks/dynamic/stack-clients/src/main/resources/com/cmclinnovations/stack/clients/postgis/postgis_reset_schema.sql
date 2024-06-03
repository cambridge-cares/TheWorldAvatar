UPDATE pg_catalog.pg_db_role_setting
SET setconfig = (
        SELECT array_agg(new)
        FROM
            unnest(setconfig) AS existing,
            regexp_replace(
                existing,
                '^(search_path=)(.*?)(?:(?:, *public)|(?:public *,))(.*)$',
                '\1public,\2\3'
            ) AS new
        GROUP BY setdatabase
    )
WHERE (
        SELECT TRUE
        FROM pg_catalog.pg_database
        WHERE datname = '{database}'
            AND setdatabase = oid
    )
