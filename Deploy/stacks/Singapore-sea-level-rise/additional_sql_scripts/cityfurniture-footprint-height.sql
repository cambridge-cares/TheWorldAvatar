-- This SQL extract footprint and height of city furniture, and store in cityobject_genericattrib as geomval and realval separately.
-- Two ways to run it: 1) put in data json file to run it in stack data uploader 2) run in sql command in PostgreSQL adminer

SET search_path TO public, citydb;

DROP TABLE IF EXISTS cf_without_footprint_CityDB;
CREATE TABLE cf_without_footprint_CityDB AS (
    SELECT cf.id,
        c.gmlid,
        public.ST_Zmin(c.envelope) AS zmin
    FROM city_furniture AS cf
        JOIN cityobject AS c ON cf.id = c.id
    WHERE COALESCE(
                    cf.lod4_brep_id,
                    cf.lod3_brep_id,
                    cf.lod2_brep_id,
                    cf.lod1_brep_id
                ) IS NOT NULL
);

INSERT INTO cityobject_genericattrib (attrname, realval, cityobject_id)
    SELECT DISTINCT ON (attrname, cityobject_id) *
        FROM  (
            SELECT 'height',
            MAX(public.ST_ZMax(surface_geometry.geometry)) - MIN(public.ST_ZMin(surface_geometry.geometry)) AS realval, city_furniture.id
            FROM city_furniture, surface_geometry
            WHERE surface_geometry.root_id = COALESCE(
                    city_furniture.lod4_brep_id,
                    city_furniture.lod3_brep_id,
                    city_furniture.lod2_brep_id,
                    city_furniture.lod1_brep_id
                )
            GROUP BY city_furniture.id
        ) AS cg(attrname, realval, cityobject_id)
    ON CONFLICT (attrname, cityobject_id) DO UPDATE SET realval= cityobject_genericattrib.realval;

INSERT INTO cityobject_genericattrib (attrname, geomval, cityobject_id)
    SELECT DISTINCT ON (attrname, cityobject_id) *
        FROM  (
            SELECT 'footprint',
            public.ST_Force3D(
            public.ST_Union(
                public.ST_MakeValid(public.ST_Force2D(surface_geometry.geometry)),
                0.00000001
            ), cf.zmin) AS geomval, city_furniture.id
            FROM city_furniture, surface_geometry, public.cf_without_footprint_CityDB AS cf
            WHERE surface_geometry.root_id = COALESCE(
                    city_furniture.lod4_brep_id,
                    city_furniture.lod3_brep_id,
                    city_furniture.lod2_brep_id,
                    city_furniture.lod1_brep_id
                ) AND cf.id = city_furniture.id
            GROUP BY city_furniture.id, cf.zmin
        ) AS cg(attrname, geomval, cityobject_id)
ON CONFLICT (attrname, cityobject_id) DO UPDATE SET realval= cityobject_genericattrib.realval;

DROP TABLE IF EXISTS cf_without_footprint_CityDB;