--Importing grid_primary_site--
ALTER TABLE grid_primary_site
ADD COLUMN IF NOT EXISTS geom geometry(Point, 4326),
ADD COLUMN IF NOT EXISTS uuid VARCHAR; 


CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

UPDATE grid_primary_site
SET geom = ST_SetSRID(ST_MakePoint("Longitude", "Latitude"), 4326);

UPDATE grid_primary_site
SET uuid = uuid_generate_v4()::text;

--Remove out of bounds site--
DELETE FROM grid_primary_site
WHERE geom IS NULL OR NOT ST_Within(geom, ST_MakeEnvelope(0.334832,52.732131,0.455535,52.777448, 4326));