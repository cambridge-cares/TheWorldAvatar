WITH "sdb" AS (
    SELECT
        "pg_catalog"."pg_db_role_setting"."setdatabase" AS "id"
    FROM
        "pg_catalog"."pg_db_role_setting"
        JOIN "pg_catalog"."pg_database" ON "pg_catalog"."pg_db_role_setting"."setdatabase" = "pg_catalog"."pg_database"."oid"
    WHERE
        "pg_catalog"."pg_database"."datname" = '{database}'
),
"rr" AS (
    SELECT
        (
            regexp_replace(
                unnest("setconfig"),
                '^(search_path=)(.*?)(?:(?:, *public)|(?:public *,))(.*)$',
                '\1public,\2\3'
            )
        ) AS x
    FROM
        "pg_catalog"."pg_db_role_setting"
        JOIN "sdb" ON "pg_catalog"."pg_db_role_setting"."setdatabase" = "sdb"."id"
),
"new_value" AS (
    SELECT
        array_agg("x") AS "y"
    FROM
        "rr"
)
UPDATE
    "pg_catalog"."pg_db_role_setting"
SET
    "setconfig" = "new_value"."y"
FROM
    "sdb",
    "new_value"
WHERE
    "pg_catalog"."pg_db_role_setting"."setdatabase" = "sdb"."id"