WITH Trajectory AS (
    SELECT trip, ST_Transform(ST_Buffer(ST_MakeLine(geom::geometry ORDER BY "UTC TIME")::geography, 100)::geometry, 27700) AS buffered_trajectory
    FROM public.gps_data
    GROUP BY trip
)
SELECT t.trip, COUNT(g.ogc_fid) AS greenspace_count
FROM public."GB_GreenspaceSite" g, Trajectory t
WHERE ST_Intersects(g.wkb_geometry, t.buffered_trajectory)
GROUP BY t.trip
ORDER BY t.trip;
