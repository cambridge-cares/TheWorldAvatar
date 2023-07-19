# Kings Lynn Flood Router

This document marks down the steps taken to create flood router, isochrone under flooding and transport network criticality analysis. 

Note: Currently, QGIS is used to consume the .asc file, crops desired location and exported in tif file. 

## Massaging the tables 
### Creating Flood Polygon

This step convert the raster data into polygon. 
```
CREATE OR REPLACE VIEW flood_polygon AS
SELECT (ST_DumpAsPolygons(rast, 1, true)).*
FROM public.flood;
```

This step convert the raster data into single polygon, and keep only flood depth more than 0.3m. Converted into single polygon to ease geoserver in displaying the flood data on DTVF.  
```
CREATE TABLE flood_polygon_single AS
(
    SELECT ST_UNION(geom_selected) AS geom
    FROM
    (
        SELECT ST_Transform(geom, 4326) AS geom_selected
        FROM flood_polygon
        WHERE val >= 0.3
    ) AS subquery
);
```



### Create flood_cost
```
CREATE OR REPLACE VIEW flood_cost AS
SELECT rw.gid AS id,
    rw.source,
    rw.target,
        CASE
            WHEN (EXISTS ( SELECT 1
               FROM flood_polygon_single
              WHERE st_intersects(rw.the_geom, flood_polygon_single.geom))) THEN (- abs(rw.cost))
            ELSE rw.cost
        END AS cost,
        CASE
            WHEN (EXISTS ( SELECT 1
               FROM flood_polygon_single
              WHERE st_intersects(rw.the_geom, flood_polygon_single.geom))) THEN (- abs(rw.reverse_cost))
            ELSE rw.reverse_cost
        END AS reverse_cost
   FROM routing_ways rw;
```

Simplfied

```
CREATE OR REPLACE VIEW flood_cost AS
SELECT rw.gid AS id, rw.tag_id as tag_id,
    rw.source,
    rw.target,
        CASE
            WHEN st_intersects(rw.the_geom, fps.geom) THEN (- abs(rw.cost))
            ELSE rw.cost
        END AS cost,
        CASE
            WHEN st_intersects(rw.the_geom, fps.geom) THEN (- abs(rw.reverse_cost))
            ELSE rw.reverse_cost
        END AS reverse_cost
   FROM (routing_ways rw
     LEFT JOIN flood_polygon_single fps ON (st_intersects(rw.the_geom, fps.geom)));
```

### Creating elevation (Optional)
This step intersects the nodes with raster DEM data and assigns elevation value to each node. 
``` 
ALTER TABLE routing_ways_vertices_pgr ADD COLUMN elevation double precision;
UPDATE routing_ways_vertices_pgr
SET elevation = ST_Value(elevation.rast, 1,  routing_ways_vertices_pgr.the_geom)
FROM elevation
WHERE ST_Intersects(elevation.rast, routing_ways_vertices_pgr.the_geom);
```

Calculate the slope of `way`. 
```
ALTER TABLE routing_ways ADD COLUMN source_elevation double precision;
ALTER TABLE routing_ways ADD COLUMN target_elevation double precision;

UPDATE routing_ways w
SET source_elevation = v1.elevation,
    target_elevation = v2.elevation
FROM routing_ways_vertices_pgr v1, routing_ways_vertices_pgr v2
WHERE w.source = v1.id
AND w.target = v2.id;

ALTER TABLE routing_ways ADD COLUMN slope FLOAT;

UPDATE routing_ways SET slope = ((target_elevation - source_elevation) / source_elevation) * 100;
```
## SQL VIEW
#### Nearest_vertex
```
SELECT
  v.id,
  v.the_geom
FROM
  routing_ways_vertices_pgr AS v,
  routing_ways AS e
WHERE
  v.id = (SELECT
            id
          FROM routing_ways_vertices_pgr
          ORDER BY the_geom <-> ST_SetSRID(ST_MakePoint(%lon%, %lat%), 4326) LIMIT 1)
  AND (e.source = v.id OR e.target = v.id)
GROUP BY v.id, v.the_geom
```

- Guess new parameters, specify any numbers for the default value.
- Deselect Escape special SQL characters. 
- Validataion regular expression `^[\d\.\+-eE]+$`
- Geometry type: Point
- Change SRS to EPSG:4326
- Specify Bounding Boxes MinX:-180, MinY:-90, MaxX:180,MaxY:90 for both fields.
- Ensure Tile Image Formats: application/json;type=geojson is enabled under `Tile Caching` 

#### Shortest_paths
```
SELECT min(r.seq) AS seq, e.gid AS id, sum(e.cost) AS cost, ST_Collect(e.the_geom) AS geom FROM pgr_dijkstra('SELECT id, source, target, cost, reverse_cost FROM flood_cost WHERE flood_cost.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)
',%source%,%target%,true) AS r,routing_ways AS e WHERE r.edge=e.gid GROUP BY e.gid


```

- Guess new parameters, specify any numbers for the default value.
- Specify any numbers for the default value 
- Deselect Escape special SQL characters. 
- Validataion regular expression `^[\d]+$`
- Geometry type: Multi-Line String. 
- Change SRS to EPSG:4326
- Specify Bounding Boxes MinX:-180, MinY:-90, MaxX:180,MaxY:90 for both fields.
- Ensure Tile Image Formats: application/json;type=geojson is enabled under `Tile Caching` 

Both of the steps above, create SQL views as a layers. Based on the endpoints of this SQL view, modify the geojson endpoint in [index.html] as accordingly. 

### Road Display
```
SELECT w.name, w.length_m, c.tag_value AS road_type, w.oneway, w.maxspeed_forward, w.maxspeed_backward,  w.the_geom
FROM routing_ways w
JOIN configuration c ON w.tag_id = c.tag_id
```

Based on the endpoints of this SQL view, modify the geojson endpoint in [data.json] as accordingly. 

Modify the geojson endpoint in index.html



## Creating Isochrone flooded and unflooded
## Creating Optimal AlphaShape Isochrone Polygon 
Note: The selected node for the source of Isochrone must be routable. 

Isochrone is in 2 minute increment. 
```
-- Create a table to store the isochrone polygons
SELECT
    minute_limit * 2 AS minute,
    ST_OptimalAlphaShape(ST_Collect(subquery.the_geom)) AS isochrone_polygon
FROM
    generate_series(1, 5) AS minute_limit
CROSS JOIN LATERAL (
    SELECT
        id,
        the_geom
    FROM
        pgr_drivingDistance(
            'SELECT id, source, target, cost_s_flood as cost FROM ways WHERE ways.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)',
            7536,
            minute_limit * 120,
            false
        ) AS dd
    JOIN
        ways_vertices_pgr AS v ON dd.node = v.id
) AS subquery
GROUP BY minute_limit
```


## Criticaly Road Network Analysis
### Calculating Trip Centrality
```
CREATE TABLE trips_centrality AS (
SELECT
  b.id,
  b.the_geom AS geom,
  count(b.the_geom) AS count
FROM
	ways AS t,
  pgr_dijkstra(
      'SELECT
          g.id AS id,
          g.source,
          g.target,
          g.cost_s AS cost
          FROM ways AS g WHERE
                g.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)', 
          7536, t.target,
          directed := FALSE) AS j
JOIN ways AS b 
  ON j.edge = b.id 
GROUP BY b.id, b.the_geom
);
```

### Return Critical Road 
Based on results, return the path with highest trip centrality.  

```
SELECT tc.*
FROM trips_centrality tc
INNER JOIN ways w ON tc.id = w.id
WHERE w.cost_s_flood < 0 AND w.cost_s_flood < 0
ORDER BY tc.count DESC;
```

Duplicate new cost function column.
```
ALTER TABLE ways
ADD COLUMN cost_s_flood_fixed numeric,
ADD COLUMN reverse_cost_s_flood_fixed numeric;

UPDATE ways
SET cost_s_flood_fixed = cost_s_flood,
    reverse_cost_s_flood_fixed = reverse_cost_s_flood;
```

Modify the cost function of top 3 critical bridge. 

```
UPDATE ways
SET cost_s_flood_fixed = abs(cost_s_flood_fixed),
    reverse_cost_s_flood_fixed = abs(reverse_cost_s_flood_fixed)
WHERE id IN (
    SELECT tc.id
    FROM trips_centrality tc
    INNER JOIN ways w ON tc.id = w.id
    WHERE w.cost_s_flood < 0 AND w.cost_s_flood < 0
    ORDER BY tc.count DESC
    LIMIT 3
);
```


Recreate Optimal_AlphaShape Isochrone Polygon with cost_s_fixed as cost function to calculate the isochrone after fixing the bridge.
```
DO $$
DECLARE
    minute_limit integer;
BEGIN
    FOR minute_limit IN 1..5 LOOP
        -- Execute the isochrone query for the current minute limit and store the results in the isochrone_results table
        EXECUTE '
            INSERT INTO isochrone_results (minute, isochrone_polygon)
            SELECT
                ' || minute_limit || ' AS minute,
                ST_OptimalAlphaShape(ST_Collect(the_geom)) AS isochrone_polygon
            FROM
                (
                    SELECT
                        id,
                        the_geom
                    FROM
                        pgr_drivingDistance(
                            ''SELECT id, source, target, cost_s_flood_fixed as cost FROM ways WHERE ways.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)'', -- Replace with your table name and cost column
                            7536, -- Specify the source vertex ID here
                            ' || (minute_limit * 120) || ', -- Specify the time/distance limit (in seconds) here
                            false
                        ) AS dd
                    JOIN
                        ways_vertices_pgr AS v ON dd.node = v.id
                ) AS subquery';
    END LOOP;
END $$;
```


## Unreachable Population
This step calculates the difference between isochrone flooded and unflooded and subsequently match with the population raster data to return the unreachable population in less than 10 minutes.
```
SELECT (
    SELECT ST_Difference(u.isochrone_polygon, i.isochrone_polygon)
    FROM isochrone_results AS i, isochrone_results_unflooded AS u
    WHERE i.minute = 10 AND u.minute = 10
),ROUND(SUM((ST_SummaryStats(ST_Clip(population_raster.rast, ST_Transform((
    SELECT ST_Difference(u.isochrone_polygon, i.isochrone_polygon)
    FROM isochrone_results AS i, isochrone_results_unflooded AS u
    WHERE i.minute = 10 AND u.minute = 10
), ST_SRID(population_raster.rast)), TRUE))).sum)) AS "Unreachable Population In Less than 10 Minutes"
FROM population_raster, unreachablelayer
```



## Travelling Salesman Problem with KingsLynn
### SPARQL Query for richest houses in KingsLynn
```
PREFIX obe:       <https://www.theworldavatar.com/kg/ontobuiltenv/>
PREFIX om:		  <http://www.ontology-of-units-of-measure.org/resource/om-2/>	
PREFIX rdf:       <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:      <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dabgeo:	  <http://www.purl.org/oema/infrastructure/>

select ?property ?price ?area
where { ?property  rdf:type dabgeo:Building ;
                   obe:hasPropertyUsage ?use ;
                   obe:hasMarketValue ?mv ; 
                   obe:hasTotalFloorArea ?floor . 
       # Consider only residential buildings
       ?use rdf:type ?usage .
       VALUES ?usage {obe:Domestic obe:SingleResidential obe:MultiResidential}
       # Consider only buildings with floor area in certain range (i.e., to exclude potential "outliers")
       ?floor om:hasValue/om:hasNumericalValue ?area . 
       FILTER (?area > 100 && ?area < 1000)
       ?mv om:hasValue/om:hasNumericalValue ?price . 
      }
ORDER BY DESC(?price)
LIMIT 10
```


### Inserting Richest House into Postgis
```
CREATE TABLE building_properties (
  id SERIAL PRIMARY KEY,
  building_IRI VARCHAR(255),
  propertyType VARCHAR(255),
  value INT,
  area DECIMAL(10, 2),
  geom GEOMETRY(Point, 4326)
);

INSERT INTO building_properties (building_IRI, propertyType, value, area, geom)
VALUES (
  'https://www.theworldavatar.com/kg/ontobuiltenv/Building_3700ce3b-ce64-48c4-9299-8b1b7434fa3b',
  'https://www.theworldavatar.com/kg/ontobuiltenv/MultiResidential_3bf425d3-23d0-4c2b-bba1-e7731439a923',
  4143000,
  471.53,
  ST_SetSRID(ST_MakePoint( 0.3947932037886142,52.75482278377076), 4326)
);

INSERT INTO building_properties (building_IRI, propertyType, value, area, geom)
VALUES (
  'https://www.theworldavatar.com/kg/ontobuiltenv/Building_8b32980e-c96f-42f1-a8a0-e1d27f96235e',
  'https://www.theworldavatar.com/kg/ontobuiltenv/MultiResidential_4f05693f-655e-4cfb-a8b7-c1c7b37e4d21',
  3336000,
  981.0,
  ST_SetSRID(ST_MakePoint( 0.38297175627405344,52.74186217408458), 4326)
);

INSERT INTO building_properties (building_IRI, propertyType, value, area, geom)
VALUES (
  'https://www.theworldavatar.com/kg/ontobuiltenv/Building_c3f28506-4dbf-4bd1-9d96-f55387f35ddb',
  'https://www.theworldavatar.com/kg/ontobuiltenv/MultiResidential_865a7f0d-f8df-44f1-84f1-8d987d2e9113',
  2851000,
  547.0,
  ST_SetSRID(ST_MakePoint( 0.43617714686580783,52.79243541416795), 4326)
);

INSERT INTO building_properties (building_IRI, propertyType, value, area, geom)
VALUES (
  'https://www.theworldavatar.com/kg/ontobuiltenv/Building_a15d1582-c216-4e77-a0fc-0227126b1e95',
  'https://www.theworldavatar.com/kg/ontobuiltenv/SingleResidential_9746e338-ff11-4209-81cc-fcc50f45804a',
  2638000,
  410.0,
  ST_SetSRID(ST_MakePoint( 0.3639685124294437,52.75416651734307), 4326)
);

INSERT INTO building_properties (building_IRI, propertyType, value, area, geom)
VALUES (
  'https://www.theworldavatar.com/kg/ontobuiltenv/Building_37166f06-6333-4693-a596-265630624b32',
  'https://www.theworldavatar.com/kg/ontobuiltenv/MultiResidential_20ca3fe0-c6e4-4c93-b42a-20dc12f4a862',
  2597000,
  880.0,
  ST_SetSRID(ST_MakePoint( 0.4020831131566202,52.765336446212174), 4326)
);

INSERT INTO building_properties (id,building_IRI, propertyType, value, geom)
VALUES (
    '1',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/Building_a15d1582-c216-4e77-a0fc-0227126b1e95>',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/SingleResidential>',
    2638000,
    ST_SetSRID(ST_MakePoint( 0.3639685124294437, 52.75416651734307), 4326)
);

INSERT INTO building_properties (id,building_IRI, propertyType, value, geom)
VALUES (
    '2',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/Building_8d0e581b-7d12-48c4-99a3-3d7c451392d1>',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/SingleResidential>',
    1996000,
    ST_SetSRID(ST_MakePoint(0.4310341724847265, 52.77611280632098), 4326)
);

INSERT INTO building_properties (id,building_IRI, propertyType, value, geom)
VALUES (
    '3',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/Building_e5b7a4c0-d6d3-43be-ab20-93bdb49d6ac0>',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/SingleResidential>',
    1950000,
    ST_SetSRID(ST_MakePoint(0.450150,52.786824), 4326)
);

```

### Adding hospital
```

INSERT INTO building_properties (id,building_IRI, propertyType, value, geom, closest_node)
VALUES (
    '6',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/Building_89b375af-1be1-4bdd-888d-3f17e9afde7c>',
    '<https://www.theworldavatar.com/kg/ontobuiltenv/Hospital_685675f8-9d61-4020-96cf-9a2c8e3f2332>',
    208233000,
    ST_SetSRID(ST_MakePoint(0.4449964774757034,52.756232805460925), 4326), 7536
);
```



### Add source and target to building
```
-- Alter the "building_properties" table to add the "source" and "target" columns
ALTER TABLE building_properties
ADD COLUMN source BIGINT,
ADD COLUMN target BIGINT;
```

### Find closest edge
Find the closest edge and subsequently the nearest node on this edge. Subsequently assign the node for pgr_tsp. 
```
ALTER TABLE building_properties ADD COLUMN closest_edge INTEGER;

UPDATE building_properties SET closest_edge = (
  SELECT edge_id FROM pgr_findCloseEdges(
    $$SELECT id, the_geom as geom FROM public.ways$$,
    (SELECT building_properties.geom),
    0.5, partial => false)
  LIMIT 1
);

-- Update the "source" and "target" columns based on the matching "closest_edge" with "id" in the "ways" table
UPDATE building_properties
SET source = ways.source, target = ways.target
FROM ways
WHERE building_properties.closest_edge = ways.id;

ALTER TABLE building_properties
ADD COLUMN closest_node INT;

ALTER TABLE building_properties ADD COLUMN source_distance numeric;
UPDATE building_properties AS p
SET source_distance = (
    SELECT ST_DISTANCE(p.geom, w.the_geom)
    FROM ways_vertices_pgr AS w
    WHERE p.source = w.id
);

ALTER TABLE building_properties ADD COLUMN target_distance numeric;
UPDATE building_properties AS p
SET target_distance = (
    SELECT ST_DISTANCE(p.geom, w.the_geom)
    FROM ways_vertices_pgr AS w
    WHERE p.target = w.id
);

UPDATE building_properties
SET closest_node = CASE
    WHEN source_distance < target_distance THEN source
    ELSE target
END;
```

Create TSP table to specify the order of sequence to visit each node. 
```
CREATE TABLE tsp AS SELECT *
FROM pgr_TSP(
  $$SELECT * FROM pgr_dijkstraCostMatrix(
    'SELECT id, source, target, cost_s as cost FROM ways WHERE ways.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 121, 123, 124, 125, 401)',
    (SELECT array_agg(id)
    FROM ways_vertices_pgr
    WHERE id IN (SELECT closest_node FROM building_properties)),
    false)$$, (SELECT MIN(closest_node) FROM building_properties), (SELECT MAX(closest_node) FROM building_properties));
```


### SQL View
#### TSP Nodes KingsLynn 
```
SELECT ways.the_geom
FROM (
    SELECT ROW_NUMBER() OVER() as seq, tsp.node
    FROM tsp
) n1
JOIN (
    SELECT ROW_NUMBER() OVER() as seq, tsp.node
    FROM tsp
) n2 ON n1.seq + 1 = n2.seq
JOIN pgr_dijkstra(
    'SELECT id, source, target, cost, reverse_cost FROM ways WHERE ways.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 115, 116, 120, 121, 123, 124, 125, 401)',
    n1.node,
    n2.node
) AS di ON true
JOIN ways ON di.edge = ways.id
ORDER BY n1.seq
```
#### TSP Nodes KingsLynn 
This step returns the nodes in travelling salesman problem. 
```
SELECT t.Seq, t.node, w.the_geom, bp.building_iri, bp.value, bp.area FROM tsp as t, ways_vertices_pgr as w, building_properties as bp WHERE t.node=w.id AND bp.closest_node=t.node
```