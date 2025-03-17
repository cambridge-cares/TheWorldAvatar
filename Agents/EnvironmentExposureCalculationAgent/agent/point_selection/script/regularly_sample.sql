DROP TABLE IF EXISTS %(point_table_name)s;
CREATE TABLE %(point_table_name)s (
    id SERIAL PRIMARY KEY,
    lon FLOAT,
    lat FLOAT
);

INSERT INTO %(point_table_name)s (lon, lat)
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