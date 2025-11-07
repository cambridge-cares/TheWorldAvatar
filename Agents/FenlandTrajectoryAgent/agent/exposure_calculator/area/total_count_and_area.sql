WITH OrderedPoints AS (
    SELECT
        "time" AS time,
        "column7" AS geom,
        ROW_NUMBER() OVER (ORDER BY "time") AS row_id
    FROM "35cb3043-9bfc-4cc7-b995-fe6290d8c86b"
),
Trajectory AS (
    SELECT
        ST_Transform(
            ST_Buffer(
                ST_MakeLine(o.geom::geometry ORDER BY o.row_id)::geography,
                100
            )::geometry,
            27700
        ) AS buffered_trajectory
    FROM OrderedPoints o
)
SELECT
    COUNT(g.ogc_fid) AS total_greenspace_count,
    COALESCE(SUM(ST_Area(ST_Intersection(t.buffered_trajectory, g.wkb_geometry))), 0) AS total_intersection_area
FROM Trajectory t
LEFT JOIN public."GB_GreenspaceSite" g
ON ST_Intersects(g.wkb_geometry, t.buffered_trajectory);
