-- Note: The following file has been developed to copy and paste the queries on the adminer screen, and later commented to form an sql file.


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %  AREA OF INTERSECTION BETWEEN EE (BEST WIND AREAS AND CIRCLE AROUND PORTS OF 100 KM IN MAGALLANES): result 20360 km2
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM (select ST_Union(a.geom) as geom, 
             '100' as distance
              
from ((SELECT ST_Intersection(a.geom, b.geom) as geom, 
              a.nombre,
              a.region, 
              a.portwkb_geometry
FROM (  SELECT ST_Transform(ST_Buffer(a.wkb_geometry, 100000, 'quad_segs=8') ,4326) as geom, 
        nombre, 
        region, 
        wkb_geometry as portwkb_geometry
   FROM "GOVseaports" AS a
) AS a, "intersection_ee" AS b
WHERE a.region = 'REGION DE MAGALLANES Y ANTARTICA CHILENA'
)
) as a) as foo;


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %  AREA OF INTERSECTION BETWEEN EE (BEST WIND AREAS AND CIRCLE AROUND PORTS OF 100 KM IN CHILE): result 20985 km2
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT ST_Area(geom::geography, true)/1000000 as km2
FROM (select ST_Union(a.geom) as geom, 
             '100' as distance
from ((SELECT ST_Intersection(a.geom, b.geom) as geom, 
              a.nombre, 
              a.region, 
              a.portwkb_geometry
FROM (  SELECT ST_Transform(ST_Buffer(a.wkb_geometry, 100000, 'quad_segs=8') ,4326) as geom, 
        nombre, 
        region, 
        wkb_geometry as portwkb_geometry
   FROM "GOVseaports" AS a
) AS a, "intersection_ee" AS b

)
) as a) as foo;

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %  AREA OF INTERSECTION BETWEEN EE (BEST WIND AREAS AND CIRCLE AROUND PORTS OF 100 KM IN CHILE): GROUPED BY REGION
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT region, ST_Area(geom::geography, true)/1000000 as km2
FROM (select ST_Union(a.geom) as geom, 
             '100' as distance, region
              
from ((SELECT ST_Intersection(a.geom, b.geom) as geom, 
              a.nombre, 
              a.region as region, 
              a.portwkb_geometry
FROM (  SELECT ST_Transform(ST_Buffer(a.wkb_geometry, 100000, 'quad_segs=8') ,4326) as geom, 
        nombre, 
        region, 
        wkb_geometry as portwkb_geometry
   FROM "GOVseaports" AS a
) AS a, "intersection_ee" AS b

)
) as a GROUP BY region) as foo



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %  CHECK GEOMETRY TYPE
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


SELECT GeometryType(geom), 100 as diameter
FROM (SELECT (ST_dump(geom)).*, 100 as diameter
FROM "intersec_ee_portsv100unionv2" ) as foo



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %  SELECT BY GEOMETRY TYPE
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT path, geom, GeometryType(geom), 100 as diameter
FROM (SELECT (ST_dump(geom)).*, 100 as diameter
FROM "intersec_ee_portsv100unionv2" ) as foo
WHERE  GeometryType(geom) = 'POLYGON'

