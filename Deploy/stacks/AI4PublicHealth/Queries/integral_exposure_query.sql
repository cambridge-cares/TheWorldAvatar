WITH entity_counts AS (
    SELECT 
        gps."UTC DATE" AS date, 
        gps."UTC TIME" AS time, 
        COUNT(frs."Name") AS no_of_entities,
        LAG(gps."UTC TIME") OVER (PARTITION BY gps."UTC DATE" ORDER BY gps."UTC TIME") AS prev_time
    FROM public.gps_data AS gps
    JOIN (
        SELECT "Name", "Address", geom
        FROM public.supermarket
        UNION ALL
        SELECT "Name", "Address", geom
        FROM public.takeaways
    ) AS frs
    ON ST_Intersects(ST_Buffer(geography(gps.geom), 100), geography(frs.geom))
    GROUP BY gps."UTC DATE", gps."UTC TIME"
),
epoch_calculation AS (
    SELECT 
        date,
        EXTRACT(EPOCH FROM (time - prev_time)) AS epoch_seconds
    FROM entity_counts
    WHERE prev_time IS NOT NULL
    LIMIT 1
)
SELECT
    ec.date,
    ec.time,
    ec.no_of_entities,
    e.epoch_seconds,
    ec.no_of_entities * e.epoch_seconds AS entity_time_product
FROM entity_counts ec
CROSS JOIN epoch_calculation e
ORDER BY ec.date, ec.time;
