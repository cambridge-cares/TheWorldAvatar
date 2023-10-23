SELECT
    wkb_geometry,
    CONCAT(
        'UKPN Overhead Line (',
        ukpn_overhead_lines_high.ogc_fid,
        '-H)'
    ) AS name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ukpn_overhead_lines/',
        ukpn_overhead_lines_high.ogc_fid
    ) as iri,
    line_colour,
    line_width
FROM
    ukpn_overhead_lines_high,
    ukpn_line_styling
WHERE
    ukpn_overhead_lines_high.voltage_level = ukpn_line_styling.voltage_level