SELECT
    wkb_geometry,
    name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ukpn_overhead_lines/',
        ukpn_overhead_lines_low_distribution.ogc_fid
    ) as iri,
    line_colour,
    line_width
FROM
    ukpn_overhead_lines_low_distribution,
    ukpn_line_styling
WHERE
    ukpn_overhead_lines_low_distribution.voltage_level = ukpn_line_styling.voltage_level