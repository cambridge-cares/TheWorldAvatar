package uk.ac.cam.cares.jps.agent.sealevelimpactagent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;


public class ImpactAssessor {

    /**
     * @param remoteRDBStoreClient
     * @param sspScenario
     * @param confidence
     * @param quantile
     * @return
     * @throws SQLException
     */
    public String getSeaLevelChangeUUID(RemoteRDBStoreClient remoteRDBStoreClient, String sspScenario, Integer projectionyear, String confidence, Integer quantile) throws SQLException {

        String findSeaLevelChangeUUID_sql = "SELECT uuid FROM sealevelprojections WHERE \"ssp scenario\" = '"+sspScenario+"' AND confidence = '"+confidence+"' AND quantile = "+quantile+" AND projectionyear = "+projectionyear+"";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            
            try (Statement statement = connection.createStatement()) {

                try (ResultSet resultSet = statement.executeQuery(findSeaLevelChangeUUID_sql)) {
                    if (resultSet.next()) {
                        // Assuming 'uuid' and 'distance' are columns in your query result
                        String uuid = resultSet.getString("uuid");
                        return uuid;
                    } else {
                        // No results found
                        return null;
                    }
                }
            }
        }
    }

    public void createTableIfNotExists(RemoteRDBStoreClient remoteRDBStoreClient)throws SQLException{

            try (Connection connection = remoteRDBStoreClient.getConnection()) {
                //Cultural Sites
                //Create table for heritagetrees
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.heritagetreesTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.heritagetreesTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.heritagetreesTable+"_uuid));");

                //Create table for historicsites
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.historicsitesTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.heritagetreesTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.heritagetreesTable+"_uuid));");
                
                //Create table for monuments
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.monumentsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.monumentsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.monumentsTable+"_uuid));");

                //Create table for museums
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.museumsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.museumsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.museumsTable+"_uuid));");
                
                //Create table for touristattractions
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.touristattractionsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.touristattractionsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.touristattractionsTable+"_uuid));");


                //Create table for landplot
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.landplotTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.landplotTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.landplotTable+"_uuid));");

                //Create table for population
                
                //Create table for landplot
                
                //Create table for buildings
                
                //Create table for road 
            }


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
}



