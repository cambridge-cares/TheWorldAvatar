DO $$ 
BEGIN
IF EXISTS (SELECT 1 from information_schema.tables where table_schema = 'env_exposure' AND table_name = '%(result_table_name)s')
THEN 
    RAISE NOTICE 'Table %(point_table_name)s already exists. Exiting script.';
    RETURN;
END IF;


DROP TABLE IF EXISTS "env_exposure"."%(result_table_name)s";
CREATE TABLE "env_exposure"."%(result_table_name)s" (
    iri TEXT,
    point_id INTEGER
);

INSERT INTO "env_exposure"."%(result_table_name)s" (iri, point_id)
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/HistoricSite/',
        hs.uuid
    ) AS iri,
    p.id as point_id
FROM
    historicsite AS hs
    JOIN "env_exposure"."%(point_table_name)s" AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(hs.wkb_geometry, 4326)
    )
UNION ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/HeritageTree/',
        ht.uuid
    ) AS iri,
    p.id as point_id
FROM
    heritagetree AS ht
    JOIN "env_exposure"."%(point_table_name)s" AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(ht.wkb_geometry, 4326)
    )
UNION ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/Museum/',
        m.uuid
    ) AS iri,
    p.id as point_id
FROM
    museum AS m
    JOIN "env_exposure"."%(point_table_name)s" AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(m.wkb_geometry, 4326)
    )
UNION ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/Monument/',
        mo.uuid
    ) AS iri,
    p.id as point_id
FROM
    monument AS mo
    JOIN "env_exposure"."%(point_table_name)s" AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(mo.wkb_geometry, 4326)
    )
UNION ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/TouristAttraction/',
        ta.uuid
    ) AS iri,
    p.id as point_id
FROM
    touristattraction AS ta
    JOIN "env_exposure"."%(point_table_name)s" AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(ta.wkb_geometry, 4326)
    );

END $$