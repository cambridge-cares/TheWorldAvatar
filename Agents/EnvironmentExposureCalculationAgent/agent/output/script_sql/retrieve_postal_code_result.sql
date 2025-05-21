WITH lat_lng AS (
    SELECT ST_X(geom) AS lng, ST_Y(geom) AS lat, id
    FROM (
        SELECT ST_GeomFromWKB(wkb_geometry) AS geom, id
        FROM "env_exposure"."point_table"
    ) AS subquery
)

SELECT pc.postal_code, ll.lng, ll.lat, COALESCE(po.greenspace_area, 0) AS greenspace_area, COALESCE(po.greenspace, 0) AS greenspace
FROM "env_exposure"."point_table" as po
JOIN "public"."sgpostcode" as pc ON pc.ogc_fid = po.ogc_fid
JOIN lat_lng as ll ON ll.id = po.id
WHERE query_id = %(query_id)s