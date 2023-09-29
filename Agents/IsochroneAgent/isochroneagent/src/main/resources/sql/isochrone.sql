--CREATE ISOCHRONE TABLE
DROP TABLE IF EXISTS isochrone_final_aggregated;
-- Create isochrone_final table to store all the isochrones table
CREATE TABLE isochrone_final_aggregated (
    minute integer,
    transportmode VARCHAR,
    ontobuilttype VARCHAR,
		population bigint,
    geom geometry
);


-- Create a PL/pgSQL function to loop through nearest_nodes
CREATE OR REPLACE FUNCTION create_eventspace_etas(v_transport VARCHAR , v_ontoBuilt VARCHAR, v_endminute int, v_minuteinterval int) RETURNS VOID AS $$
DECLARE node bigint;
    edges_table text;
BEGIN

-- Drop the existing tables
DROP TABLE IF EXISTS eventspace_etas;
DROP TABLE IF EXISTS eventspace_isochrones;
DROP TABLE IF EXISTS isochrone_final; 


-- Create the ETA table
CREATE TABLE eventspace_etas (
    id bigint,
    agg_cost double precision,
    the_geom geometry(PointZ, 4326)
);

-- Create the single_isochrone_table table
CREATE TABLE eventspace_isochrones (
    id bigint,
    geom geometry,
    transportmode VARCHAR,
    ontobuilttype VARCHAR,
    minute integer
);

-- Create isochrone_final table to store all the isochrones table
CREATE TABLE isochrone_final (
    id bigint,
    minute integer,
    transportmode VARCHAR,
    ontobuilttype VARCHAR,
    geom geometry
);



-- Cursor to iterate through  nodes
FOR node IN (SELECT DISTINCT nearest_node FROM buildings WHERE ontobuilt = v_ontoBuilt) LOOP

TRUNCATE TABLE  eventspace_etas;
TRUNCATE TABLE  eventspace_isochrones;

-- Set the SQL query based on the value of v_transport
CASE
    WHEN v_transport = 'Walk' THEN
        edges_table := 'SELECT gid as id, source, target, length_m/1.3 as cost FROM routing_ways_segment';
    WHEN v_transport = 'Bicycle' THEN
        edges_table := 'SELECT gid as id, source, target, length_m/5.5 as cost FROM routing_ways_segment';
    WHEN v_transport = 'Car' THEN
        edges_table := 'SELECT gid as id, source, target, cost_s as cost FROM routing_ways_segment WHERE tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 121, 123, 124, 125, 401)';
    ELSE
        edges_table := 'SELECT gid as id, source, target, cost_s as cost FROM routing_ways_segment WHERE tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 121, 123, 124, 125, 401)';
END CASE;



INSERT INTO eventspace_etas (id, agg_cost, the_geom)
SELECT
    id,
    dd.agg_cost,
    ST_SetSRID(ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost), 4326) AS the_geom
FROM pgr_drivingDistance(
    edges_table, -- Use the dynamically set SQL query here
    node,
    10000,
    false
) AS dd
JOIN routing_ways_segment_vertices_pgr AS v ON dd.node = v.id;





-- Update the_geom to ensure correct SRID
UPDATE eventspace_etas SET the_geom = ST_SetSRID(ST_MakePoint(ST_X(the_geom), ST_Y(the_geom), agg_cost), 4326);


--eventspace_delaunay
drop table if exists eventspace_delaunay;
create table eventspace_delaunay
   as (
          select (ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))).geom from eventspace_etas
      );

--eventspace_isochrone
-- Insert data into the table using the SELECT query
INSERT INTO eventspace_isochrones (geom, minute)
SELECT
    ST_Union(ST_ConvexHull(ST_LocateBetweenElevations(ST_Boundary(geom),
                                                         0,
                                                         minute * 60))) geom,
              minute
FROM
    eventspace_delaunay,
    generate_series(0, v_endminute, v_minuteinterval ) AS minute
GROUP BY
    minute;

UPDATE eventspace_isochrones
SET transportmode = v_transport, ontobuilttype = v_ontoBuilt;


--Store all the isochrones that passed through each node
INSERT INTO isochrone_final (minute, transportmode, ontobuilttype, geom)
SELECT minute,transportmode, ontobuilttype, ST_Union(geom) AS geom
FROM eventspace_isochrones
GROUP BY minute,transportmode, ontobuilttype;

END LOOP;

--Store all the dissolved aggregated isochrones
INSERT INTO isochrone_final_aggregated (minute, transportmode, ontobuilttype, geom)
SELECT
    minute,transportmode, ontobuilttype,
    ST_UNION(geom) AS geom
FROM
    isochrone_final
WHERE minute !=0
GROUP BY
    minute,transportmode, ontobuilttype;


DROP TABLE eventspace_etas;
DROP TABLE eventspace_delaunay;
DROP TABLE eventspace_isochrones;
DROP TABLE isochrone_final;

END;
$$ LANGUAGE plpgsql;