ALTER TABLE
    ng_substations
ADD
    COLUMN IF NOT EXISTS max_voltage_kv FLOAT,
ADD
    COLUMN IF NOT EXISTS voltage_level VARCHAR,
ADD
    COLUMN IF NOT EXISTS name VARCHAR;

UPDATE
    ng_substations
SET
    max_voltage_kv = CAST(TRIM(trailing 'KV' from "voltage") AS FLOAT) ;

UPDATE ng_substations SET voltage_level = (CASE
    WHEN max_voltage_kv >= 230 THEN 'SubS' 
    WHEN max_voltage_kv < 230 and max_voltage_kv >= 35 THEN 'SubH' 
    WHEN max_voltage_kv < 35 and max_voltage_kv >= 1 THEN 'SubM' end) ;

UPDATE ng_substations SET name = CONCAT('National Grid Substation (', "ogc_fid", '-', voltage_level, ')') ;    