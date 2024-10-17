WITH adjusted_times AS (
    SELECT DISTINCT
        to_timestamp(time / 1000) AT TIME ZONE 'UTC' AT TIME ZONE '%s' AS local_time
    FROM
        public.get_location_table(CAST(get_device_ids('%s') AS TEXT[]))
),
date_parts AS (
    SELECT
        EXTRACT(YEAR FROM local_time) AS year,
        EXTRACT(MONTH FROM local_time) AS month,
        EXTRACT(DAY FROM local_time) AS day
    FROM
        adjusted_times
),
grouped_dates AS (
    SELECT
        year,
        month,
        array_agg(DISTINCT day) AS days
    FROM
        date_parts
    GROUP BY
        year, month
)

SELECT
    year,
    month,
    days
FROM
    grouped_dates
ORDER BY
    year, month;