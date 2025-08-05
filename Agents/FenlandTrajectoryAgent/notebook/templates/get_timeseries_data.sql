SELECT
    to_timestamp("UNIX_time" / 1000.0) AT TIME ZONE 'UTC' AS "time",
    "column1",
    "column2",
    "column3",
    "column4",
    "column5",
    "column6",
    "column7"
FROM "{table_name}"
ORDER BY "UNIX_time" ASC;