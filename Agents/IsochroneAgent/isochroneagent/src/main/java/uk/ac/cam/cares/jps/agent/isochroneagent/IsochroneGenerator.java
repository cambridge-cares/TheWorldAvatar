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
    /**
     * Generates isochrone based on time threshold, time interval, transportmode and road conditions.
     * It derives TransportMode, RoadCondition from the .sql file name, edgeTableSQL from its content.
     * @param remoteRDBStoreClient
     * @param upperTimeLimit Maximum time limit of the multiple isochrones.
     * @param timeInterval Time increments between each isochrone
     * @param EdgesTableSQLMap The transport mode and road conditions to be used for isochrone calculations
     */
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
                    String strippedFileExtension = entry.getKey().replaceAll("\\.sql$", "");;

                    String[] parts = strippedFileExtension.split("_");
                    if (parts.length == 2) {
                        String transportMode = parts[0];
                        String roadCondition = parts[1];
                        String edgeTable = entry.getValue();

                        // Process each entry (key and value) as needed
                        createIsochrone(remoteRDBStoreClient, upperTimeLimit, timeInterval, transportMode, roadCondition, edgeTable,  poiType);

                    } else {
                        System.out.println("The input string is not in the right format.");
                    }
                }

            }

            smoothIsochrone(remoteRDBStoreClient);

        } catch (SQLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Create isochrone_aggregated table to store all final isochrone.
     * @param remoteRDBStoreClient
     */
    private void generateTable(RemoteRDBStoreClient remoteRDBStoreClient){

        String tableGeneration = "--CREATE ISOCHRONE TABLE\n" +
                "-- Create isochrone_final table to store all the isochrones table\n" +
                "CREATE TABLE IF NOT EXISTS isochrone_aggregated (\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    transportmode_iri VARCHAR,\n" +
                "    poi_type VARCHAR,\n" +
                "    roadcondition VARCHAR,\n" +
                "    roadcondition_iri VARCHAR,\n" +
                "    iri VARCHAR,\n" +
                "    geometry_iri VARCHAR, \n" +
                "    geom geometry\n" +
                ");";

        String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, add_uuid_ossp_Extension);
            executeSql(connection, tableGeneration);
            System.out.println("Isochrone_aggregated table created. Calculating isochrones based on POI now.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Perform isochrone calculations based on the multiple point of interests, upperTimeLimit of isochrones, time interval between isocrhones, edgeTable.
     * @param remoteRDBStoreClient
     * @param upperTimeLimit
     * @param timeInterval Time interval between isochrones
     * @param transportMode Transport mode used to calculate isochrones
     * @param roadCondition Road conditions of the isochrones
     * @param edgeTable EdgeTable contains the cost table of the road data
     * @param poiType Different points of interest
     */
    private void createIsochrone(RemoteRDBStoreClient remoteRDBStoreClient, int upperTimeLimit, int timeInterval, String transportMode, String roadCondition, String edgeTable, String poiType){

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
                "    transportmode_iri VARCHAR,\n" +
                "    roadcondition VARCHAR,\n" +
                "    roadcondition_iri VARCHAR,\n" +
                "    poi_type VARCHAR,\n" +
                "    minute integer\n" +
                ");\n" +
                
                "-- Create isochrone_individual table to store all the isochrones table\n" +
                "CREATE TABLE isochrone_individual (\n" +
                "    id bigint,\n" +
                "    minute integer,\n" +
                "    transportmode VARCHAR,\n" +
                "    transportmode_iri VARCHAR,\n" +
                "    roadcondition VARCHAR,\n" +
                "    roadcondition_iri VARCHAR,\n" +
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
                "SET transportmode = '"+transportMode+"', roadcondition = '"+roadCondition+"', poi_type = '"+poiType+"';\n" +
                
                
                "--Store all the isochrones that passed through each node\n" +
                "INSERT INTO isochrone_individual (minute, transportmode, roadcondition, poi_type, geom)\n" +
                "SELECT minute,transportmode, roadcondition, poi_type, ST_Union(geom) AS geom\n" +
                "FROM eventspace_isochrones\n" +
                "GROUP BY minute,transportmode, roadcondition, poi_type;\n" +
                
                "END LOOP;\n" +
                
                "--Store all the dissolved aggregated isochrones\n" +
                "INSERT INTO isochrone_aggregated (minute, transportmode, roadcondition, poi_type, geom)\n" +
                "SELECT\n" +
                "    minute,transportmode, roadcondition, poi_type,\n" +
                "    ST_UNION(geom) AS geom\n" +
                "FROM\n" +
                "    isochrone_individual\n" +
                "WHERE minute !=0\n" +
                "GROUP BY\n" +
                "    minute,transportmode, roadcondition, poi_type;\n" +

                "WITH unique_values AS (\n" +
                "    SELECT\n" +
                "        poi_type,\n" +
                "        minute,\n" +
                "        transportmode,\n" +
                "        roadcondition,\n" +
                "        uuid_generate_v4()::text AS iri,\n" +
                "        uuid_generate_v4()::text AS geometry_iri\n" +
                "    FROM isochrone_aggregated\n" +
                "    WHERE iri IS NULL\n" +
                "    GROUP BY minute, poi_type, transportmode, roadcondition\n" +
                ")\n" +
                "\n" +
                "UPDATE isochrone_aggregated AS ia\n" +
                "SET iri = uv.iri,\n" +
                "    geometry_iri = uv.geometry_iri,\n" +
                "       transportmode_iri =  '"+transportMode+"_' || uuid_generate_v4()::text,\n" +
            "       roadcondition_iri =  '"+roadCondition+"_' || uuid_generate_v4()::text\n" +
                "FROM unique_values uv\n" +
                "WHERE ia.iri IS NULL\n" +
                "    AND ia.poi_type = uv.poi_type\n" +
                "    AND ia.transportmode = uv.transportmode\n"+
                "    AND ia.minute = uv.minute;"+

                "UPDATE isochrone_aggregated AS t1\n" +
                "SET roadcondition_iri = t2.min_iri\n" +
                "FROM (\n" +
                "    SELECT roadcondition, MIN(roadcondition_iri) AS min_iri\n" +
                "    FROM isochrone_aggregated\n" +
                "    GROUP BY roadcondition\n" +
                ") AS t2\n" +
                "WHERE t1.roadcondition = t2.roadcondition;\n"+


                "UPDATE isochrone_aggregated AS t1\n" +
                "SET transportmode_iri = t2.min_iri\n" +
                "FROM (\n" +
                "    SELECT transportmode, MIN(transportmode_iri) AS min_iri\n" +
                "    FROM isochrone_aggregated\n" +
                "    GROUP BY transportmode\n" +
                ") AS t2\n" +
                "WHERE t1.transportmode = t2.transportmode;\n"+

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

    /**
     * Retrieve the different POI types.
     * @param remoteRDBStoreClient
     * @return
     * @throws SQLException
     */
    public List<String> getPoiTypes (RemoteRDBStoreClient remoteRDBStoreClient) throws SQLException {
        String getNearestNode_sql = "SELECT DISTINCT pnn.poi_type FROM poi_nearest_node AS pnn LEFT JOIN isochrone_aggregated AS ia ON pnn.poi_type = ia.poi_type WHERE ia.poi_type IS NULL";
    
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

    /**
     * Smoothen the hard corners of all isochrones using ChaikinSmoothing.
     * @param remoteRDBStoreClient
     */
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


    /**
     * Create reference table to indicate each isochrone is originated by which poi_iri.
     * @param remoteRDBStoreClient
     */
    public void createIsochroneBuilding (RemoteRDBStoreClient remoteRDBStoreClient){

    String createIsochroneBuildingTable_sqlString ="-- Drop the table if it exists\n" +
                                                    "DROP TABLE IF EXISTS isochrone_building_ref;\n" +
                                                    "\n" +
                                                    "-- Create the table\n" +
                                                    "CREATE TABLE isochrone_building_ref AS\n" +
                                                    "SELECT DISTINCT ia.iri, pno.poi_iri, pno.poi_type\n" +
                                                    "FROM isochrone_aggregated AS ia\n" +
                                                    "LEFT JOIN poi_nearest_node AS pno ON ia.poi_type = pno.poi_type\n" +
                                                    "ORDER BY ia.iri;";
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
