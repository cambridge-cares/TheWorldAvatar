SELECT
    wkb_geometry,
    name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ukpn_overhead_lines/',
        ukpn_overhead_lines_medium.ogc_fid
    ) as iri,
    line_colour,
    line_width
FROM
    ukpn_overhead_lines_medium,
    ukpn_line_styling
WHERE
    ukpn_overhead_lines_medium.voltage_level = ukpn_line_styling.voltage_level
UNION
ALL
SELECT
    wkb_geometry,
    CONCAT(
        'UKPN Overhead Line (',
        ukpn_overhead_lines_medium_66kv.ogc_fid,
        '-',
        ukpn_overhead_lines_medium_66kv.voltage_level,
        ')'
    ) AS name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ukpn_overhead_lines_medium_66kv/',
        ukpn_overhead_lines_medium_66kv.ogc_fid
    ) as iri,
    line_colour,
    line_width
FROM
    ukpn_overhead_lines_medium_66kv,
    ukpn_line_styling
WHERE
    ukpn_overhead_lines_medium_66kv.voltage_level = ukpn_line_styling.voltage_level