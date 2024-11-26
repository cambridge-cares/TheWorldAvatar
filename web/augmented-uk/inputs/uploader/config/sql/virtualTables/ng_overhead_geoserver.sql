SELECT
    wkb_geometry,
    name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ng_overhead_lines/',
        ng_overhead_lines.ogc_fid
    ) as iri,
    line_colour,
    line_width
FROM
    ng_overhead_lines,
    ng_styling
WHERE
    ng_overhead_lines.voltage_level = ng_styling.voltage_level