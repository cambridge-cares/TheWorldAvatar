--Importing grid_primary_site--
ALTER TABLE grid_primary_site
ADD COLUMN IF NOT EXISTS geom geometry(Point, 4326),
ADD COLUMN IF NOT EXISTS uuid VARCHAR; 


CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

UPDATE grid_primary_site
SET geom = 
    CASE
        WHEN "Spatial Coordinates" ~ '^-?[0-9]+(\.[0-9]+)?,\s*-?[0-9]+(\.[0-9]+)?$' THEN
            ST_SetSRID(ST_MakePoint(
                CAST(split_part("Spatial Coordinates", ',', 2) AS double precision),
                CAST(split_part("Spatial Coordinates", ',', 1) AS double precision)
            ), 4326)
        ELSE
            NULL
    END;

UPDATE grid_primary_site
SET uuid = uuid_generate_v4()::text;

--Remove out of bounds site--
DELETE FROM grid_primary_site
WHERE geom IS NULL OR NOT ST_Within(geom, ST_MakeEnvelope(0.334832,52.732131,0.455535,52.777448, 4326));

