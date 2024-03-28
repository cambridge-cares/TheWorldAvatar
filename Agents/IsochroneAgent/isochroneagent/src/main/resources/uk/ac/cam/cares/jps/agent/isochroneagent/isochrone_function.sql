DO $ $ DECLARE node bigint;

-- Drop the existing tables
BEGIN DROP TABLE IF EXISTS eventspace_etas;

DROP TABLE IF EXISTS eventspace_isochrones;

DROP TABLE IF EXISTS isochrone_individual;

-- Create the ETA table CREATE TABLE
eventspace_etas (
    id bigint,
    agg_cost double precision,
    the_geom geometry(PointZ, 4326)
);

-- Create the eventspace_isochrones 
table CREATE TABLE eventspace_isochrones (
    id bigint,
    geom geometry,
    transportmode VARCHAR,
    transportmode_iri VARCHAR,
    roadcondition VARCHAR,
    roadcondition_iri VARCHAR,
    poi_type VARCHAR,
    minute integer
);

-- Create isochrone_individual table to store all the isochrones table
CREATE TABLE isochrone_individual (
    id bigint,
    minute integer,
    transportmode VARCHAR,
    transportmode_iri VARCHAR,
    roadcondition VARCHAR,
    roadcondition_iri VARCHAR,
    poi_type VARCHAR,
    geom geometry
);

-- Cursor to iterate through nodes 
FOR node IN (
    SELECT
        DISTINCT nearest_node
    FROM
        poi_nearest_node
    WHERE
        poi_type = ' poiType '
) LOOP TRUNCATE TABLE eventspace_etas;

TRUNCATE TABLE eventspace_isochrones;

INSERT INTO
    eventspace_etas (id, agg_cost, the_geom)
SELECT
    id,
    dd.agg_cost,
    ST_SetSRID(
        ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost),
        4326
    ) AS the_geom
FROM
    pgr_drivingDistance(
        ' edgeTable ',
        -- Use the dynamically set SQL query here
        node,
        10000,
        false
    ) AS dd
    JOIN routing_ways_segment_vertices_pgr AS v ON dd.node = v.id;

-- Update the_geom to ensure correct SRID 
UPDATE
    eventspace_etas
SET
    the_geom = ST_SetSRID(
        ST_MakePoint(ST_X(the_geom), ST_Y(the_geom), agg_cost),
        4326
    );

--eventspace_delaunay
drop table if exists eventspace_delaunay;

create table eventspace_delaunay as (
    select
        (
            ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))
        ).geom
    from
        eventspace_etas
);

--eventspace_isochrone 
-- Insert data into the table using the SELECT query 
INSERT INTO
    eventspace_isochrones (geom, minute)
SELECT
    ST_Union(
        ST_ConvexHull(
            ST_LocateBetweenElevations(
                ST_Boundary(geom),
                0,
                minute * 60
            )
        )
    ) geom,
    minute
FROM
    eventspace_delaunay,
    generate_series(0, upperTimeLimit, timeInterval) AS minute
GROUP BY
    minute;

UPDATE
    eventspace_isochrones
SET
    transportmode = ' transportMode ',
    roadcondition = ' roadCondition ',
    poi_type = ' poiType ';

--Store all the isochrones that passed through each node 
INSERT INTO
    isochrone_individual (
        minute,
        transportmode,
        roadcondition,
        poi_type,
        geom
    )
SELECT
    minute,
    transportmode,
    roadcondition,
    poi_type,
    ST_Union(geom) AS geom
FROM
    eventspace_isochrones
GROUP BY
    minute,
    transportmode,
    roadcondition,
    poi_type;

END LOOP;

--Store all the dissolved aggregated isochrones 
INSERT INTO
    isochrone_aggregated (
        minute,
        transportmode,
        roadcondition,
        poi_type,
        geom
    )
SELECT
    minute,
    transportmode,
    roadcondition,
    poi_type,
    ST_UNION(geom) AS geom
FROM
    isochrone_individual
WHERE
    minute != 0
GROUP BY
    minute,
    transportmode,
    roadcondition,
    poi_type;

WITH unique_values AS (
    SELECT
        poi_type,
        minute,
        transportmode,
        roadcondition,
        uuid_generate_v4() :: text AS iri,
        uuid_generate_v4() :: text AS geometry_iri
    FROM
        isochrone_aggregated
    WHERE
        iri IS NULL
    GROUP BY
        minute,
        poi_type,
        transportmode,
        roadcondition
)
UPDATE
    isochrone_aggregated AS ia
SET
    iri = uv.iri,
    geometry_iri = uv.geometry_iri,
    transportmode_iri = ' transportMode _' || uuid_generate_v4() :: text,
    roadcondition_iri = ' roadCondition _' || uuid_generate_v4() :: text
FROM
    unique_values uv
WHERE
    ia.iri IS NULL
    AND ia.poi_type = uv.poi_type
    AND ia.transportmode = uv.transportmode
    AND ia.minute = uv.minute;

UPDATE
    isochrone_aggregated AS t1
SET
    roadcondition_iri = t2.min_iri
FROM
    (
        SELECT
            roadcondition,
            MIN(roadcondition_iri) AS min_iri
        FROM
            isochrone_aggregated
        GROUP BY
            roadcondition
    ) AS t2
WHERE
    t1.roadcondition = t2.roadcondition;

UPDATE
    isochrone_aggregated AS t1
SET
    transportmode_iri = t2.min_iri
FROM
    (
        SELECT
            transportmode,
            MIN(transportmode_iri) AS min_iri
        FROM
            isochrone_aggregated
        GROUP BY
            transportmode
    ) AS t2
WHERE
    t1.transportmode = t2.transportmode;

DROP TABLE eventspace_etas;

DROP TABLE eventspace_delaunay;

DROP TABLE eventspace_isochrones;

DROP TABLE isochrone_individual;

END $ $;

;