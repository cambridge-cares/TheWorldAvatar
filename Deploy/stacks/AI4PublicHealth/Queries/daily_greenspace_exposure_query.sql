WITH Trajectory AS (
    SELECT ST_Transform(ST_Buffer(ST_MakeLine(geom::geometry ORDER BY "UTC TIME")::geography, 100)::geometry, 27700) AS buffered_trajectory
    FROM public.gps_data
)
SELECT g.ogc_fid, g.function, g."distName1", g."distName2", g.wkb_geometry
FROM public."GB_GreenspaceSite" g, Trajectory t
WHERE ST_Intersects(g.wkb_geometry, t.buffered_trajectory);
