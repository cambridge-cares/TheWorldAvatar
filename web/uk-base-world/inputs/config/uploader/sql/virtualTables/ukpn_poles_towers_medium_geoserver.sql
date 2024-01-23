SELECT
    wkb_geometry,
    name,
    CONCAT(
        'http://theworldavatar.com/ontology/ontopower/OntoPower.owl#ukpn_poles_towers/',
        ukpn_poles_towers_medium.ogc_fid
    ) as iri,
    pylon_icon AS icon,
    icon_size
FROM
    ukpn_poles_towers_medium,
    ukpn_line_styling
WHERE
    ukpn_poles_towers_medium.voltage_level = ukpn_line_styling.voltage_level