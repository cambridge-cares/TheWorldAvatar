SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/HistoricSite/',
        hs.uuid
    ) AS iri,
    p.id as point_id
FROM
    historicsite AS hs
    JOIN %(point_table_name)s AS p ON ST_Intersects(
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
    JOIN %(point_table_name)s AS p ON ST_Intersects(
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
    JOIN %(point_table_name)s AS p ON ST_Intersects(
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
    JOIN %(point_table_name)s AS p ON ST_Intersects(
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
    JOIN %(point_table_name)s AS p ON ST_Intersects(
        p.buffer_geom,
        ST_Transform(ta.wkb_geometry, 4326)
    );