WITH Trajectory AS (
    SELECT ST_Transform(
        ST_Buffer(
            ST_MakeLine("column7"::geometry ORDER BY "time")::geography, 
            {exposure_radius}
        )::geometry, 
        27700
    ) AS buffered_trajectory
    FROM "{table_name}"
)
SELECT g.ogc_fid, g.function, g."distName1", g."distName2", g.wkb_geometry
FROM public."GB_GreenspaceSite" g, Trajectory t
WHERE ST_Intersects(g.wkb_geometry, t.buffered_trajectory);
