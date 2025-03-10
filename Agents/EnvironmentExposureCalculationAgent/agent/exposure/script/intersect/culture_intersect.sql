SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/HistoricSite/',
        hs.uuid
    ) AS iri,
    bp.buffer_geom,
    bp.lat,
    bp.lon
FROM
    historicsite AS hs
    JOIN buffered_points AS bp ON ST_Intersects(
        bp.buffer_geom,
        ST_Transform(hs.wkb_geometry, 4326)
    )
UNION
ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/HeritageTree/',
        ht.uuid
    ) AS iri,
    bp.buffer_geom,
    bp.lat,
    bp.lon
FROM
    heritagetree AS ht
    JOIN buffered_points AS bp ON ST_Intersects(
        bp.buffer_geom,
        ST_Transform(ht.wkb_geometry, 4326)
    )
UNION
ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/Museum/',
        m.uuid
    ) AS iri,
    bp.buffer_geom,
    bp.lat,
    bp.lon
FROM
    museum AS m
    JOIN buffered_points AS bp ON ST_Intersects(
        bp.buffer_geom,
        ST_Transform(m.wkb_geometry, 4326)
    )
UNION
ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/Monument/',
        mo.uuid
    ) AS iri,
    bp.buffer_geom,
    bp.lat,
    bp.lon
FROM
    monument AS mo
    JOIN buffered_points AS bp ON ST_Intersects(
        bp.buffer_geom,
        ST_Transform(mo.wkb_geometry, 4326)
    )
UNION
ALL
SELECT
    CONCAT(
        'https://www.theworldavatar.com/kg/TouristAttraction/',
        ta.uuid
    ) AS iri,
    bp.buffer_geom,
    bp.lat,
    bp.lon
FROM
    touristattraction AS ta
    JOIN buffered_points AS bp ON ST_Intersects(
        bp.buffer_geom,
        ST_Transform(ta.wkb_geometry, 4326)
    );