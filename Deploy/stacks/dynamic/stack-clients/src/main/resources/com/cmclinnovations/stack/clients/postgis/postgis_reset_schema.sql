WITH "sdb" AS (
    SELECT
        "pg_catalog"."pg_db_role_setting"."setdatabase" AS "id"
    FROM
        "pg_catalog"."pg_db_role_setting"
        JOIN "pg_catalog"."pg_database" ON "pg_catalog"."pg_db_role_setting"."setdatabase" = "pg_catalog"."pg_database"."oid"
    WHERE
        "pg_catalog"."pg_database"."datname" = '{database}'
)
UPDATE
    "pg_catalog"."pg_db_role_setting"
SET
    "setconfig"[1] = replace(
            replace(setconfig [1], 'public,', ''),
            'search_path=',
            'search_path=public,'
        )
FROM
    "sdb"
WHERE
    "pg_catalog"."pg_db_role_setting"."setdatabase" = "sdb"."id"