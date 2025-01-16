ALTER TABLE
    ukpn_overhead_lines_low_distribution
ADD
    COLUMN IF NOT EXISTS name VARCHAR;

UPDATE
    ukpn_overhead_lines_low_distribution
SET
    name = CONCAT(
        'UKPN Overhead Line (',
        dno,
        '-',
        ogc_fid,
        '-',
        voltage_level,
        ')'
    );