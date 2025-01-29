WITH OrderedPoints AS (
    SELECT
        "time" AS time,
        "column7" AS geom,
        ROW_NUMBER() OVER (ORDER BY "time") AS row_id
    FROM "35cb3043-9bfc-4cc7-b995-fe6290d8c86b"
),
Trajectory AS (
    SELECT
        o1.row_id AS start_row,
        o1.time,
        ST_Transform(
            ST_Buffer(
                ST_MakeLine(o2.geom::geometry ORDER BY o2.row_id)::geography,
                100
            )::geometry,
            27700
        ) AS buffered_trajectory
    FROM OrderedPoints o1
    JOIN OrderedPoints o2
    ON o2.row_id <= o1.row_id
    GROUP BY o1.row_id, o1.time
)
SELECT
    t.start_row AS row_id,
    t.time,
    COUNT(g.ogc_fid) AS greenspace_count,
    COALESCE(SUM(ST_Area(ST_Intersection(t.buffered_trajectory, g.wkb_geometry))), 0) AS total_intersection_area
FROM Trajectory t
LEFT JOIN public."GB_GreenspaceSite" g
ON ST_Intersects(g.wkb_geometry, t.buffered_trajectory)
GROUP BY t.start_row, t.time
ORDER BY t.start_row