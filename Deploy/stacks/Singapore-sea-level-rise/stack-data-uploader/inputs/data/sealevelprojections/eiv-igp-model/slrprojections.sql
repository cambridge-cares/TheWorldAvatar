-- Step 1: Add uuid and wkt columns
ALTER TABLE sealevelprojections
ADD COLUMN uuid VARCHAR,
ADD COLUMN ssp NUMERIC,
ADD COLUMN rcp NUMERIC,
ADD COLUMN geom geometry;

-- Step 2: Enable uuid-ossp extension if not already enabled
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Step 3: Populate uuid column with unique identifiers
UPDATE sealevelprojections
SET uuid = uuid_generate_v4(), 
ssp = CAST(SUBSTRING(ssp_scenario FROM 4 FOR 1) AS NUMERIC), 
rcp = CAST(RIGHT(ssp_scenario, 2) AS NUMERIC),
confidence = INITCAP(confidence);