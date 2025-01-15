WITH Trajectory AS (
    SELECT trip_index, ST_Transform(ST_Buffer(ST_MakeLine(geom::geometry ORDER BY "UTC TIME")::geography, 100)::geometry, 27700) AS buffered_trajectory
    FROM public.gps_data
    GROUP BY trip_index
)
SELECT t.trip_index, COUNT(g.ogc_fid) AS greenspace_count
FROM public."GB_GreenspaceSite" g, Trajectory t
WHERE ST_Intersects(g.wkb_geometry, t.buffered_trajectory)
GROUP BY t.trip_index
ORDER BY t.trip_index;
