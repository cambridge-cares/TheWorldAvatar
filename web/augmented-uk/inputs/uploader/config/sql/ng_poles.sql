ALTER TABLE
    ng_poles
ADD
    COLUMN IF NOT EXISTS max_voltage_kv FLOAT,
ADD
    COLUMN IF NOT EXISTS voltage_level VARCHAR,
ADD
    COLUMN IF NOT EXISTS name VARCHAR;

UPDATE
    ng_poles
SET
    max_voltage_kv = CAST(TRIM(trailing 'KV' from "voltage") AS FLOAT) ;

UPDATE ng_poles SET voltage_level = (CASE
    WHEN max_voltage_kv >= 230 THEN 'S' 
    WHEN max_voltage_kv < 230 and max_voltage_kv >= 35 THEN 'H' 
    WHEN max_voltage_kv < 35 and max_voltage_kv >= 1 THEN 'M' end) ;

UPDATE ng_poles SET name = CONCAT('National Grid Pole (', "ogc_fid", '-', voltage_level, ')') ;    