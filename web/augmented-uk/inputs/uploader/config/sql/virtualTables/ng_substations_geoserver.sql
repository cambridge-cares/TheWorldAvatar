SELECT
    "wkb_geometry",
    "siteName" as name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ng_substations/',
        ng_substations.ogc_fid
    ) as iri,
    pylon_icon as icon,
    icon_size
FROM
    ng_substations,
    ng_styling
WHERE
    ng_substations.voltage_level = ng_styling.voltage_level