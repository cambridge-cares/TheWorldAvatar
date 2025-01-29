WITH OrderedPoints AS (
    SELECT
        "time" AS time,
        "time"::date AS date,
        "column7" AS geom,
        ROW_NUMBER() OVER (ORDER BY "time") AS row_id
    FROM "35cb3043-9bfc-4cc7-b995-fe6290d8c86b"
),
StaticBuffers AS (
    SELECT
        row_id,
        date,
        time,
        ST_Transform(ST_Buffer(geom::geography, 100)::geometry, 27700) AS static_buffer
    FROM OrderedPoints
)
SELECT
    sb.row_id,
    sb.date,
    sb.time,
    COUNT(g.ogc_fid) AS greenspace_count,
    COALESCE(SUM(ST_Area(ST_Intersection(sb.static_buffer, g.wkb_geometry))), 0) AS total_intersection_area
FROM StaticBuffers sb
LEFT JOIN public."GB_GreenspaceSite" g
ON ST_Intersects(sb.static_buffer, g.wkb_geometry)
GROUP BY sb.row_id, sb.date, sb.time
ORDER BY sb.row_id;