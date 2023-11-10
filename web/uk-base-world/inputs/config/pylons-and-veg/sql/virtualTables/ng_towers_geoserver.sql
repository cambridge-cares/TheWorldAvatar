    SELECT
    "wkb_geometry",
    "srcName" as name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ng_poles_towers/',
        ng_towers.ogc_fid
    ) as iri,
    pylon_icon as icon,
    icon_size
FROM
    ng_towers,
    ng_styling
WHERE
    ng_towers.voltage_level = ng_styling.voltage_level