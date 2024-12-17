-- Add ssp and rcp Columns
ALTER TABLE sealevelprojections
ADD COLUMN confidence VARCHAR,
ADD COLUMN ssp_scenario VARCHAR,
ADD COLUMN quantile INTEGER,
ADD COLUMN projectionreferenceyear INTEGER,
ADD COLUMN projectionyear INTEGER ,
ADD COLUMN sealevelriseinmeters DOUBLE PRECISION,
ADD COLUMN ssp NUMERIC,
ADD COLUMN rcp NUMERIC,
ADD COLUMN uuid VARCHAR;

ALTER TABLE sealevelprojections
RENAME COLUMN wkb_geometry TO geom;

-- Update Table
UPDATE sealevelprojections
SET confidence= SPLIT_PART(key, '_', 1),
   ssp_scenario=  CAST(SPLIT_PART(key, '_', 2) AS VARCHAR) ,
     quantile = CAST(SPLIT_PART(key, '_', 3) AS INTEGER),
    projectionreferenceyear = CAST(SPLIT_PART(key, '_', 4) AS INTEGER),
    projectionyear=  CAST(SPLIT_PART(key, '_', 5) AS INTEGER) ,
    sealevelriseinmeters = CAST(SPLIT_PART(key, '_', 6) AS DOUBLE PRECISION);


-- Update Table
UPDATE sealevelprojections
SET ssp = CAST(SUBSTRING(ssp_scenario FROM 4 FOR 1) AS NUMERIC),
    rcp = CAST(RIGHT(ssp_scenario, 2) AS NUMERIC),
    confidence = INITCAP(confidence);

-- Step 2: Enable uuid-ossp extension if not already enabled
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Step 3: Populate uuid column with unique identifiers
UPDATE sealevelprojections
SET uuid = uuid_generate_v4();