-- create as temp table
CREATE TEMP TABLE buffered_points AS WITH x_series AS (
    SELECT
        % lng_start % AS x_series
    WHERE
        % lng_start % = % lng_end %
    UNION
    ALL
    SELECT
        generate_series(% lng_start %, % lng_end %, % lng_deg %) AS x_series -- Longitude range (step = lng_deg°)
    WHERE
        % lng_start % <> % lng_end %
),
y_series AS (
    SELECT
        % lat_start % AS y_series
    WHERE
        % lat_start % = % lat_end %
    UNION
    ALL
    SELECT
        generate_series(% lat_start %, % lat_end %, % lat_deg %) AS y_series -- Latitude range (step = lat_deg°)
    WHERE
        % lat_start % <> % lat_end %
)
SELECT
    x_series AS lon,
    y_series AS lat,
    ST_Transform(
        ST_Buffer(
            ST_Transform(
                ST_SetSRID(ST_MakePoint(x_series, y_series), 4326),
                3857
            ),
            % buffer_radius %
        ),
        4326
    ) as buffer_geom
FROM
    x_series,
    y_series