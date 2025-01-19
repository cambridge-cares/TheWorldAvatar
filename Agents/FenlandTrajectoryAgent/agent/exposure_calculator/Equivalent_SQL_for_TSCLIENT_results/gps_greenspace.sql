WITH Trajectory AS (
    SELECT ST_Transform(
        ST_Buffer(
            ST_MakeLine("column7"::geometry ORDER BY "time")::geography, 
            100
        )::geometry, 
        27700
    ) AS buffered_trajectory
    FROM "aca2947a-8d41-4edd-a9dd-4ffa8ed616d0" --placehodler, replace with table name from dbtable
)
SELECT g.ogc_fid, g.function, g."distName1", g."distName2", g.wkb_geometry
FROM public."GB_GreenspaceSite" g, Trajectory t
WHERE ST_Intersects(g.wkb_geometry, t.buffered_trajectory);