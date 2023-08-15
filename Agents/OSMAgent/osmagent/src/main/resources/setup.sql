BEGIN;

-- Add columns to planet_osm_point
ALTER TABLE planet_osm_point
ADD COLUMN building_iri TEXT,
ADD COLUMN propertyusage_iri TEXT,
ADD COLUMN ontobuilt TEXT,
ADD COLUMN usageshare FLOAT,

-- Add columns to planet_osm_polygon
ALTER TABLE planet_osm_polygon
ADD COLUMN building_iri TEXT,
ADD COLUMN propertyusage_iri TEXT,
ADD COLUMN ontobuilt TEXT,
ADD COLUMN usageshare FLOAT,

COMMIT;
