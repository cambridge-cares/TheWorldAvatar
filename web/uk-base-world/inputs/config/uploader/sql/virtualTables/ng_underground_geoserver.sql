SELECT
    wkb_geometry,
    name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ng_underground_cables/',
        ng_underground_cables.ogc_fid
    ) as iri,
    line_colour,
    line_width,
    outline_colour,
    outline_width
FROM
    ng_underground_cables,
    ng_styling
WHERE
    ng_underground_cables.voltage_level = ng_styling.voltage_level