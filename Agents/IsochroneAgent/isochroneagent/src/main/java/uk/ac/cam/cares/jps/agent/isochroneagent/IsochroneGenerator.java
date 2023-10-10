package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class IsochroneGenerator {
    public void generateIsochrone(RemoteRDBStoreClient remoteRDBStoreClient, int upperTimeLimit, int timeInterval, Map<String,String> EdgesTableSQLMap ){

        generateTable(remoteRDBStoreClient);
        
        List<String> poiTypes;
        try {
            //Initilize poiTypes and get distinct poiType; 
            poiTypes = getPoiTypes(remoteRDBStoreClient);

            // Outer loop: Run createIsochrone method for each poiType
            for (String poiType : poiTypes) {
                //Inner Loop: Run createIsochrone method for each transport mode

                for (Map.Entry<String, String> entry : EdgesTableSQLMap.entrySet()) {
                    //Get transport mode and replace the .sql with empty spaces
                    String transportMode = entry.getKey().replaceAll("\\.sql$", "");;
                    String edgeTable = entry.getValue();
        
                    // Process each entry (key and value) as needed
                    createIsochrone(remoteRDBStoreClient, upperTimeLimit, timeInterval, transportMode, edgeTable,  poiType);
                }

            }

            smoothIsochrone(remoteRDBStoreClient);

        } catch (SQLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private void generateTable(RemoteRDBStoreClient remoteRDBStoreClient){

        String tableGeneration = "--CREATE ISOCHRONE TABLE\n" +
                "DROP TABLE IF EXISTS isochrone_aggregated;\n" +
                "-- Create isochrone_final table to store all the isochrones table\n" +
                "CREATE TABLE isochrone_aggregated (\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    poi_type VARCHAR,\n" +
                "    isochrone_iri VARCHAR,\n" +
                "    geometry_iri VARCHAR, \n" +
                "    geom geometry\n" +
                ");";

        String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, add_uuid_ossp_Extension);
            executeSql(connection, tableGeneration);
            System.out.println("Isochrone_aggregated table created.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    private void createIsochrone(RemoteRDBStoreClient remoteRDBStoreClient, int upperTimeLimit, int timeInterval, String transportMode, String edgeTable, String poiType){

        String isochroneFunction = 
                "DO $$ \n"+
                "DECLARE node bigint;\n" +
                "BEGIN\n" +
                
                "-- Drop the existing tables\n" +
                "DROP TABLE IF EXISTS eventspace_etas;\n" +
                "DROP TABLE IF EXISTS eventspace_isochrones;\n" +
                "DROP TABLE IF EXISTS isochrone_individual; \n" +
                
                
                "-- Create the ETA table\n" +
                "CREATE TABLE eventspace_etas (\n" +
                "    id bigint,\n" +
                "    agg_cost double precision,\n" +
                "    the_geom geometry(PointZ, 4326)\n" +
                ");\n" +
                
                "-- Create the eventspace_isochrones table\n" +
                "CREATE TABLE eventspace_isochrones (\n" +
                "    id bigint,\n" +
                "    geom geometry,\n" +
                "    transportmode VARCHAR,\n" +
                "    poi_type VARCHAR,\n" +
                "    minute integer\n" +
                ");\n" +
                
                "-- Create isochrone_individual table to store all the isochrones table\n" +
                "CREATE TABLE isochrone_individual (\n" +
                "    id bigint,\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    poi_type VARCHAR,\n" +
                "    geom geometry\n" +
                ");\n" +
                
                
                
                "-- Cursor to iterate through  nodes\n" +
                "FOR node IN (SELECT DISTINCT nearest_node FROM poi_nearest_node WHERE poi_type = '"+poiType+"') LOOP\n" +
                
                "TRUNCATE TABLE  eventspace_etas;\n" +
                "TRUNCATE TABLE  eventspace_isochrones;\n" +
                
                
                "INSERT INTO eventspace_etas (id, agg_cost, the_geom)\n" +
                "SELECT\n" +
                "    id,\n" +
                "    dd.agg_cost,\n" +
                "    ST_SetSRID(ST_MakePoint(ST_X(v.the_geom), ST_Y(v.the_geom), dd.agg_cost), 4326) AS the_geom\n" +
                "FROM pgr_drivingDistance(\n" +
                "    '"+edgeTable+"', -- Use the dynamically set SQL query here\n" +
                "    node,\n" +
                "    10000,\n" +
                "    false\n" +
                ") AS dd\n" +
                "JOIN routing_ways_segment_vertices_pgr AS v ON dd.node = v.id;\n" +
                
                
                "-- Update the_geom to ensure correct SRID\n" +
                "UPDATE eventspace_etas SET the_geom = ST_SetSRID(ST_MakePoint(ST_X(the_geom), ST_Y(the_geom), agg_cost), 4326);\n" +
                
                
                "--eventspace_delaunay\n" +
                "drop table if exists eventspace_delaunay;\n" +
                "create table eventspace_delaunay\n" +
                "   as (\n" +
                "          select (ST_Dump(ST_DelaunayTriangles(ST_Collect(the_geom)))).geom from eventspace_etas\n" +
                "      );\n" +
                
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
                "    generate_series(0, "+upperTimeLimit+", "+timeInterval+" ) AS minute\n" +
                "GROUP BY\n" +
                "    minute;\n" +
                
                "UPDATE eventspace_isochrones\n" +
                "SET transportmode = '"+transportMode+"', poi_type = '"+poiType+"';\n" +
                
                
                "--Store all the isochrones that passed through each node\n" +
                "INSERT INTO isochrone_individual (minute, transportmode, poi_type, geom)\n" +
                "SELECT minute,transportmode, poi_type, ST_Union(geom) AS geom\n" +
                "FROM eventspace_isochrones\n" +
                "GROUP BY minute,transportmode, poi_type;\n" +
                
                "END LOOP;\n" +
                
                "--Store all the dissolved aggregated isochrones\n" +
                "INSERT INTO isochrone_aggregated (minute, transportmode, poi_type, geom)\n" +
                "SELECT\n" +
                "    minute,transportmode, poi_type,\n" +
                "    ST_UNION(geom) AS geom\n" +
                "FROM\n" +
                "    isochrone_individual\n" +
                "WHERE minute !=0\n" +
                "GROUP BY\n" +
                "    minute,transportmode, poi_type;\n" +

                "WITH unique_values AS (\n" +
                "    SELECT\n" +
                "        poi_type,\n" +
                "        minute,\n" +
                "        transportmode,\n" +
                "        'https://www.theworldavatar.com/kg/ontoisochrone/Isochrone/' || uuid_generate_v4()::text AS isochrone_iri,\n" +
                "        'http://www.opengis.net/ont/geosparql#Geometry/' || uuid_generate_v4()::text AS geometry_iri\n" +
                "    FROM isochrone_aggregated\n" +
                "    WHERE isochrone_iri IS NULL\n" +
                "    GROUP BY minute, poi_type, transportmode\n" +
                ")\n" +
                "\n" +
                "UPDATE isochrone_aggregated AS ia\n" +
                "SET isochrone_iri = uv.isochrone_iri,\n" +
                "    geometry_iri = uv.geometry_iri\n" +
                "FROM unique_values uv\n" +
                "WHERE ia.isochrone_iri IS NULL\n" +
                "    AND ia.poi_type = uv.poi_type\n" +
                "    AND ia.transportmode = uv.transportmode\n"+
                "    AND ia.minute = uv.minute;"+

                "DROP TABLE eventspace_etas;\n" +
                "DROP TABLE eventspace_delaunay;\n" +
                "DROP TABLE eventspace_isochrones;\n" +
                "DROP TABLE isochrone_individual;\n" +
                
                "END $$;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, isochroneFunction);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    public List<String> getPoiTypes (RemoteRDBStoreClient remoteRDBStoreClient) throws SQLException {
        String getNearestNode_sql = "SELECT DISTINCT poi_type FROM poi_nearest_node";
    
        List<String> poiTypes = new ArrayList<>();
    
        try (Connection connection = remoteRDBStoreClient.getConnection();
             Statement statement = connection.createStatement();
             ResultSet resultSet = statement.executeQuery(getNearestNode_sql)) {
    
            while (resultSet.next()) {
                poiTypes.add(resultSet.getString("poi_type"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    
        return poiTypes;
    }

    
    /**
     * Create connection to remoteStoreClient and execute SQL statement
     * @param connection PostgreSQL connection object
     * @param sql SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }
    
    private void smoothIsochrone(RemoteRDBStoreClient remoteRDBStoreClient){

        String smoothIsochrone_sql = "update isochrone_aggregated  set geom = ST_CollectionExtract(geom, 3);\n" +
                "update isochrone_aggregated set geom = ST_ChaikinSmoothing(geom, 3, false);";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, smoothIsochrone_sql);
            System.out.println("Isochrone_aggregated smoothened.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }



    public void createIsochroneBuilding (RemoteRDBStoreClient remoteRDBStoreClient){

    String createIsochroneBuildingTable_sqlString ="-- Drop the table if it exists\n" +
                                                    "DROP TABLE IF EXISTS isochrone_building_ref;\n" +
                                                    "\n" +
                                                    "-- Create the table\n" +
                                                    "CREATE TABLE isochrone_building_ref AS\n" +
                                                    "SELECT DISTINCT ia.isochrone_iri, pno.poi_iri, pno.poi_type\n" +
                                                    "FROM isochrone_aggregated AS ia\n" +
                                                    "LEFT JOIN poi_nearest_node AS pno ON ia.poi_type = pno.poi_type\n" +
                                                    "ORDER BY ia.isochrone_iri;";
    try (Connection connection = remoteRDBStoreClient.getConnection()) {
        executeSql(connection, createIsochroneBuildingTable_sqlString);
        System.out.println("isochrone_building_ref table created.");
    }
    catch (Exception e) {
        e.printStackTrace();
        throw new JPSRuntimeException(e);
    }

    }

    

}
