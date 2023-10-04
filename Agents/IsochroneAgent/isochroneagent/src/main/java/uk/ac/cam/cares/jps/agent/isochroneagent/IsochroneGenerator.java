package uk.ac.cam.cares.jps.agent.isochroneagent;

import org.json.JSONArray;
import java.util.Map;


public class IsochroneGenerator {
    public static void generateIsochrone(JSONArray poisArraywithNodes, int upperTimeLimit, int timeInterval, Map<String,String> EdgesTableSQLMap ){

        String isochrone_sql = "--CREATE ISOCHRONE TABLE\n" +
                "DROP TABLE IF EXISTS isochrone_final_aggregated;\n" +
                "-- Create isochrone_final table to store all the isochrones table\n" +
                "CREATE TABLE isochrone_final_aggregated (\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    ontobuilttype VARCHAR,\n" +
                "\t\tpopulation bigint,\n" +
                "    geom geometry\n" +
                ");\n" +
                "\n" +
                "\n" +
                "-- Create a PL/pgSQL function to loop through nearest_nodes\n" +
                "CREATE OR REPLACE FUNCTION create_eventspace_etas(v_transport VARCHAR , v_ontoBuilt VARCHAR, v_endminute int, v_minuteinterval int) RETURNS VOID AS $$\n" +
                "DECLARE node bigint;\n" +
                "    edges_table text;\n" +
                "BEGIN\n" +
                "\n" +
                "-- Drop the existing tables\n" +
                "DROP TABLE IF EXISTS eventspace_etas;\n" +
                "DROP TABLE IF EXISTS eventspace_isochrones;\n" +
                "DROP TABLE IF EXISTS isochrone_final; \n" +
                "\n" +
                "\n" +
                "-- Create the ETA table\n" +
                "CREATE TABLE eventspace_etas (\n" +
                "    id bigint,\n" +
                "    agg_cost double precision,\n" +
                "    the_geom geometry(PointZ, 4326)\n" +
                ");\n" +
                "\n" +
                "-- Create the single_isochrone_table table\n" +
                "CREATE TABLE eventspace_isochrones (\n" +
                "    id bigint,\n" +
                "    geom geometry,\n" +
                "    transportmode VARCHAR,\n" +
                "    ontobuilttype VARCHAR,\n" +
                "    minute integer\n" +
                ");\n" +
                "\n" +
                "-- Create isochrone_final table to store all the isochrones table\n" +
                "CREATE TABLE isochrone_final (\n" +
                "    id bigint,\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    ontobuilttype VARCHAR,\n" +
                "    geom geometry\n" +
                ");\n" +
                "\n" +
                "\n" +
                "\n" +
                "-- Cursor to iterate through  nodes\n" +
                "FOR node IN (SELECT DISTINCT nearest_node FROM buildings WHERE ontobuilt = v_ontoBuilt) LOOP\n" +
                "\n" +
                "TRUNCATE TABLE  eventspace_etas;\n" +
                "TRUNCATE TABLE  eventspace_isochrones;\n" +
                "\n" +
                "-- Set the SQL query based on the value of v_transport\n" +
                "CASE\n" +
                "    WHEN v_transport = 'Walk' THEN\n" +
                "        edges_table := 'SELECT gid as id, source, target, length_m/1.3 as cost FROM routing_ways_segment';\n" +
                "    WHEN v_transport = 'Bicycle' THEN\n" +
                "        edges_table := 'SELECT gid as id, source, target, length_m/5.5 as cost FROM routing_ways_segment';\n" +
                "    WHEN v_transport = 'Car' THEN\n" +
                "        edges_table := 'SELECT gid as id, source, target, cost_s as cost FROM routing_ways_segment WHERE tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 121, 123, 124, 125, 401)';\n" +
                "    ELSE\n" +
                "        edges_table := 'SELECT gid as id, source, target, cost_s as cost FROM routing_ways_segment WHERE tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 121, 123, 124, 125, 401)';\n" +
                "END CASE;\n" +
                "\n" +
                "\n" +
                "\n" +
                "INSERT INTO eventspace_etas (id, agg_cost, the_geom)\n" +
                "SELECT\n" +
                "    id,\n" +
                "    dd.agg_cost,\n" +
                "    ST_SetSRID(ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost), 4326) AS the_geom\n" +
                "FROM pgr_drivingDistance(\n" +
                "    edges_table, -- Use the dynamically set SQL query here\n" +
                "    node,\n" +
                "    10000,\n" +
                "    false\n" +
                ") AS dd\n" +
                "JOIN routing_ways_segment_vertices_pgr AS v ON dd.node = v.id;\n" +
                "\n" +
                "\n" +
                "\n" +
                "\n" +
                "\n" +
                "-- Update the_geom to ensure correct SRID\n" +
                "UPDATE eventspace_etas SET the_geom = ST_SetSRID(ST_MakePoint(ST_X(the_geom), ST_Y(the_geom), agg_cost), 4326);\n" +
                "\n" +
                "\n" +
                "--eventspace_delaunay\n" +
                "drop table if exists eventspace_delaunay;\n" +
                "create table eventspace_delaunay\n" +
                "   as (\n" +
                "          select (ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))).geom from eventspace_etas\n" +
                "      );\n" +
                "\n" +
                "--eventspace_isochrone\n" +
                "-- Insert data into the table using the SELECT query\n" +
                "INSERT INTO eventspace_isochrones (geom, minute)\n" +
                "SELECT\n" +
                "    ST_Union(ST_ConvexHull(ST_LocateBetweenElevations(ST_Boundary(geom),\n" +
                "                                                         0,\n" +
                "                                                         minute * 60))) geom,\n" +
                "              minute\n" +
                "FROM\n" +
                "    eventspace_delaunay,\n" +
                "    generate_series(0, v_endminute, v_minuteinterval ) AS minute\n" +
                "GROUP BY\n" +
                "    minute;\n" +
                "\n" +
                "UPDATE eventspace_isochrones\n" +
                "SET transportmode = v_transport, ontobuilttype = v_ontoBuilt;\n" +
                "\n" +
                "\n" +
                "--Store all the isochrones that passed through each node\n" +
                "INSERT INTO isochrone_final (minute, transportmode, ontobuilttype, geom)\n" +
                "SELECT minute,transportmode, ontobuilttype, ST_Union(geom) AS geom\n" +
                "FROM eventspace_isochrones\n" +
                "GROUP BY minute,transportmode, ontobuilttype;\n" +
                "\n" +
                "END LOOP;\n" +
                "\n" +
                "--Store all the dissolved aggregated isochrones\n" +
                "INSERT INTO isochrone_final_aggregated (minute, transportmode, ontobuilttype, geom)\n" +
                "SELECT\n" +
                "    minute,transportmode, ontobuilttype,\n" +
                "    ST_UNION(geom) AS geom\n" +
                "FROM\n" +
                "    isochrone_final\n" +
                "WHERE minute !=0\n" +
                "GROUP BY\n" +
                "    minute,transportmode, ontobuilttype;\n" +
                "\n" +
                "\n" +
                "DROP TABLE eventspace_etas;\n" +
                "DROP TABLE eventspace_delaunay;\n" +
                "DROP TABLE eventspace_isochrones;\n" +
                "DROP TABLE isochrone_final;\n" +
                "\n" +
                "END;\n" +
                "$$ LANGUAGE plpgsql;";
    }
    
}
