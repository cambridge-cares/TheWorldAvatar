SET search_path TO public,
    citydb;
CREATE INDEX IF NOT EXISTS idx_building_log_id ON citydb.building (
    (
        COALESCE(
            lod4_multi_surface_id,
            lod3_multi_surface_id,
            lod2_multi_surface_id,
            lod1_multi_surface_id,
            lod4_solid_id,
            lod3_solid_id,
            lod2_solid_id,
            lod1_solid_id
        )
    )
);
CREATE INDEX IF NOT EXISTS idx_thematic_surface_lod_multi_surface_id ON citydb.thematic_surface (
    (
        COALESCE(
            lod4_multi_surface_id,
            lod3_multi_surface_id,
            lod2_multi_surface_id
        )
    )
);
DROP TABLE IF EXISTS building_without_footprint_CityDB,
precise_footprint_CityDB,
rough_footprint_CityDB,
footprint_soup_CityDB,
fp_parent;
CREATE TABLE building_without_footprint_CityDB AS (
    SELECT b.id,
        c.gmlid,
        ST_Zmin(c.envelope) AS zmin
    FROM building AS b
        JOIN cityobject AS c ON b.id = c.id
    WHERE b.lod0_footprint_id IS NULL
);
CREATE TABLE public.precise_footprint_CityDB AS (
    SELECT building_id,
        ST_Union(
            ST_MakeValid(ST_Force2D(s.geometry)),
            0.00000001
        ) AS U
    FROM surface_geometry AS s
        JOIN thematic_surface AS t ON s.root_id = COALESCE(
            t.lod4_multi_surface_id,
            t.lod3_multi_surface_id,
            t.lod2_multi_surface_id
        )
        JOIN building_without_footprint_CityDB ON building_without_footprint_CityDB.id = t.building_id
    WHERE s.geometry IS NOT NULL
        AND objectclass_id = 35
    GROUP BY building_id
);
CREATE TABLE public.rough_footprint_CityDB AS (
    SELECT COALESCE(
            b.id,
            t.building_id
        ) AS building_id,
        ST_Union(
            ST_MakeValid(ST_Force2D(s.geometry)),
            0.00000001
        ) AS U
    FROM surface_geometry AS s
        LEFT JOIN building AS b ON s.root_id = COALESCE(
            b.lod4_multi_surface_id,
            b.lod3_multi_surface_id,
            b.lod2_multi_surface_id,
            b.lod1_multi_surface_id,
            b.lod4_solid_id,
            b.lod3_solid_id,
            b.lod2_solid_id,
            b.lod1_solid_id
        )
        LEFT JOIN thematic_surface AS t ON s.root_id = COALESCE(
            t.lod4_multi_surface_id,
            t.lod3_multi_surface_id,
            t.lod2_multi_surface_id
        )
    WHERE geometry IS NOT NULL
        AND ST_Area(geometry) > 0.0
        AND NOT EXISTS (
            SELECT 1
            FROM precise_footprint_CityDB AS p
            WHERE p.building_id = COALESCE(b.id, t.building_id)
        )
        AND (
            COALESCE(
                b.id,
                t.building_id
            ) IN (
                SELECT id
                FROM public.building_without_footprint_CityDB
            )
        )
    GROUP BY COALESCE(
            b.id,
            t.building_id
        )
);
CREATE TABLE public.footprint_soup_CityDB AS (
    SELECT b.id,
        b.gmlid,
        ST_Force3D(
            (
                ST_Dump(
                    COALESCE(
                        p.U,
                        r.U
                    )
                )
            ).geom,
            b.zmin
        ) AS geometry
    FROM building_without_footprint_CityDB AS b
        LEFT JOIN precise_footprint_CityDB AS p ON b.id = p.building_id
        LEFT JOIN rough_footprint_CityDB AS r ON b.id = r.building_id
);
WITH temp AS (
    INSERT INTO citydb.surface_geometry (
            gmlid,
            is_solid,
            is_composite,
            is_triangulated,
            is_xlink,
            is_reverse,
            cityobject_id
        ) (
            SELECT b.gmlid || '_footprint',
                0,
                0,
                0,
                0,
                0,
                b.id
            FROM building_without_footprint_CityDB AS b
        )
    RETURNING id AS footprint_id,
        cityobject_id AS bid,
        gmlid
)
SELECT footprint_id,
    bid,
    gmlid INTO TABLE public.fp_parent
FROM temp;
UPDATE building AS b
SET lod0_footprint_id = f.footprint_id
FROM fp_parent AS f
WHERE b.id = f.bid
    AND b.lod0_footprint_id IS NULL;
INSERT INTO citydb.surface_geometry (
        gmlid,
        parent_id,
        root_id,
        is_solid,
        is_composite,
        is_triangulated,
        is_xlink,
        is_reverse,
        geometry,
        cityobject_id
    ) (
        SELECT s.gmlid || '_footprint_child',
            f.footprint_id,
            f.footprint_id,
            0,
            0,
            0,
            0,
            0,
            s.geometry,
            s.id
        FROM footprint_soup_CityDB AS s
            JOIN fp_parent AS f ON s.id = f.bid
        WHERE GeometryType(geometry) = 'POLYGON'
    );
UPDATE surface_geometry
SET root_id = id
WHERE root_id IS NULL;
DROP TABLE IF EXISTS building_without_footprint_CityDB,
precise_footprint_CityDB,
rough_footprint_CityDB,
footprint_soup_CityDB,
fp_parent;