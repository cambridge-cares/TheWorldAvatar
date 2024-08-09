CREATE TABLE IF NOT EXISTS isochrone_aggregated (
    minute integer,
    transportmode VARCHAR,
    transportmode_iri VARCHAR,
    poi_type VARCHAR,
    roadcondition VARCHAR,
    roadcondition_iri VARCHAR,
    iri VARCHAR,
    geometry_iri VARCHAR,
    geom geometry
);

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";