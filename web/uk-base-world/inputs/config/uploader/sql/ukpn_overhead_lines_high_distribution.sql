ALTER TABLE
    ukpn_overhead_lines_high_distribution
ADD
    COLUMN IF NOT EXISTS max_voltage_kv FLOAT,
ADD
    COLUMN IF NOT EXISTS name VARCHAR;

UPDATE
    ukpn_overhead_lines_high_distribution
SET
    max_voltage_kv = CAST(
        REGEXP_REPLACE(betr_spann, '[^0-9\\.]+', '', 'g') AS FLOAT
    ),
    name = CONCAT(
        'UKPN Overhead Line (',
        dno,
        '-',
        ogc_fid,
        '-',
        voltage_level,
        ')'
    );