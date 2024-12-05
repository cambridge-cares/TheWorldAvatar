-- Initialise empty table for Ontop 
CREATE TABLE IF NOT EXISTS isochrone_building_ref (
    iri VARCHAR,
    poi_iri VARCHAR,
    poi_type VARCHAR
);

-- population is dependent on the actual case. There may be multiple population columns

CREATE TABLE IF NOT EXISTS isochrone_aggregated (
    minute integer,
    transportmode VARCHAR,
    transportmode_iri VARCHAR,
    poi_type VARCHAR,
    roadcondition VARCHAR,
    roadcondition_iri VARCHAR,
    iri VARCHAR,
    geometry_iri VARCHAR,
    population bigint,
    geom geometry
);