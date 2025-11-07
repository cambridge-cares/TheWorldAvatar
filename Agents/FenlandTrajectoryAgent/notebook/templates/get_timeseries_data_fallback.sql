SELECT
  "time",
  ST_Y("column4") AS "LATITUDE",
  ST_X("column4") AS "LONGITUDE"
FROM "{table_name}"
WHERE "time_series_iri" = %s
ORDER BY "time" ASC;