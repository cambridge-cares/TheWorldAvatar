SELECT
    "wkb_geometry",
    "srcName" as name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ng_poles_towers/',
        ng_poles.ogc_fid
    ) as iri,
    pylon_icon as icon,
    icon_size
FROM
    ng_poles,
    ng_styling
WHERE
    ng_poles.voltage_level = ng_styling.voltage_level