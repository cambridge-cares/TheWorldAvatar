-- Create a new table with the same structure as the old table
CREATE TABLE routing_ways_segment AS
SELECT *
FROM routing_ways
WHERE 1 = 0; -- This ensures that the new table has the same structure but no data

-- Insert data into the new table with a split "the_geom" column
INSERT INTO routing_ways_segment
SELECT
    gid,
    osm_id,
    tag_id,
    length,
    length_m,
    name,
    source,
    target,
    source_osm,
    target_osm,
    cost,
    reverse_cost,
    cost_s,
    reverse_cost_s,
    rule,
    one_way,
    oneway,
    x1,
    y1,
    x2,
    y2,
    maxspeed_forward,
    maxspeed_backward,
    priority,
    (ST_DumpSegments(ST_Segmentize(the_geom, 0.0005))).geom AS the_geom -- Split the "the_geom" column
FROM
    routing_ways

-- Step 1: Create a temporary sequence
CREATE SEQUENCE temp_sequence;

-- Step 2: Update the gid column with new values from the sequence
UPDATE routing_ways_segment
SET gid = nextval('temp_sequence');

-- Step 3: Reset the sequence to the next available value
SELECT setval('temp_sequence', (SELECT max(gid) FROM routing_ways_segment) + 1);

-- Step 4: Drop the temporary sequence (optional)
DROP SEQUENCE temp_sequence;

SELECT pgr_createTopology('routing_ways_segment', 0.000001, 'the_geom', 'gid', 'source', 'target', clean := true);