-- remove ships with very sparse data and small time range
WITH "STAT" AS (
    SELECT
        DISTINCT "MMSI",
        COUNT(*) AS "COUNT",
        EXTRACT(
            EPOCH
            FROM
                (MAX("BaseDateTime") - MIN("BaseDateTime"))
        ) / 3600.0 AS "TIMERANGE"
    FROM
        "rawship"
    GROUP BY
        "MMSI"
),
"SPARSE" AS (
    SELECT
        "MMSI"
    FROM
        "STAT"
    WHERE
        "COUNT" <= 100
        AND "TIMERANGE" <= 23.0
)
DELETE FROM
    "rawship" USING "SPARSE"
WHERE
    "rawship"."MMSI" = "SPARSE"."MMSI";

-- interpolate ship data
CREATE TABLE "ship" AS WITH distinct_mmsi AS (
    SELECT
        DISTINCT "MMSI",
        "VesselType"
    FROM
        "rawship"
),
time_series AS (
    SELECT
        generate_series(
            MIN("BaseDateTime"),
            MAX("BaseDateTime"),
            '30 minutes' :: interval
        ) AS interval_start
    FROM
        "rawship"
)
SELECT
    dm."MMSI",
    ts.interval_start AS "BaseDateTime",
    AVG(rs."LAT") AS "LAT",
    AVG(rs."LON") AS "LON",
    AVG(rs."SOG") AS "SOG",
    AVG(rs."COG") AS "COG",
    dm."VesselType"
FROM
    distinct_mmsi dm
    CROSS JOIN time_series ts
    LEFT JOIN "rawship" rs ON rs."MMSI" = dm."MMSI"
    AND rs."BaseDateTime" >= ts.interval_start
    AND rs."BaseDateTime" < ts.interval_start + '30 minutes' :: interval
GROUP BY
    dm."MMSI",
    ts.interval_start,
    dm."VesselType"
ORDER BY
    dm."MMSI",
    ts.interval_start;

-- clean up NULL entries
DELETE FROM
    "ship"
WHERE
    "LAT" IS NULL
    OR "LON" IS NULL
    OR "SOG" IS NULL
    OR "COG" IS NULL;

-- create geom column
ALTER TABLE
    "ship"
ADD
    COLUMN "geom" geometry(Point, 4326);

UPDATE
    "ship"
SET
    "geom" = ST_SetSRID(ST_MakePoint("LON", "LAT"), 4326);

-- delete raw ship data
DROP TABLE "rawship";