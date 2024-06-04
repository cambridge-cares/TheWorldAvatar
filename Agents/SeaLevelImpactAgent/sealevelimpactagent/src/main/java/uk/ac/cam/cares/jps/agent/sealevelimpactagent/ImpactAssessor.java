package uk.ac.cam.cares.jps.agent.sealevelimpactagent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
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
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.historicsitesTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.historicsitesTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.historicsitesTable+"_uuid));");
                
                //Create table for monuments
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.monumentsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.monumentsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.monumentsTable+"_uuid));");

                //Create table for museums
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.museumsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.museumsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.museumsTable+"_uuid));");
                
                //Create table for touristattractions
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.touristattractionsTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.touristattractionsTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.touristattractionsTable+"_uuid));");

                //Create table for landplot
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.landplotTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.landplotTable+"_uuid VARCHAR, affectedarea DOUBLE PRECISION, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.landplotTable+"_uuid));");

                //Create table for population
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.populationTable+" (slr_uuid VARCHAR, populationatrisk INTEGER, PRIMARY KEY (slr_uuid));");
                
                //Create table for buildings
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.buildingsMatViewName +"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.buildingsMatViewName +"_uuid VARCHAR);");

                //Create table for road
                executeSql(connection,"CREATE TABLE IF NOT EXISTS slr_"+SeaLevelImpactAgent.osm_streetTable+"(slr_uuid VARCHAR,"+SeaLevelImpactAgent.osm_streetTable+"_uuid VARCHAR, PRIMARY KEY (slr_uuid, "+SeaLevelImpactAgent.osm_streetTable+"_uuid));");
            }
    }

    public void mapPopulationAtRisk(RemoteRDBStoreClient remoteRDBStoreClient, String slr_uuid, String populationTable)throws SQLException{

        //check if slr_uuid ID exists in slr_population
        if (!isSLRDataExistsInSLRTable(remoteRDBStoreClient, slr_uuid, populationTable)) {

            try (Connection connection = remoteRDBStoreClient.getConnection()) {
                String populationInsertSQL = "INSERT INTO slr_population (slr_uuid, populationatrisk)\n" +
                        "SELECT subquery.uuid, subquery.sum\n" +
                        "FROM (\n" +
                        "SELECT sealevelprojections.uuid, SUM((ST_SummaryStats(ST_Clip(population.rast, sealevelprojections.geom, TRUE))).sum) AS sum\n" +
                        "FROM sealevelprojections, population\n" +
                        "WHERE sealevelprojections.uuid = '" + slr_uuid + "'\n" +
                        "GROUP BY sealevelprojections.uuid\n" +
                        ") AS subquery;";

                executeSql(connection, populationInsertSQL);
            }
            System.out.println("SLR_UUID: "+slr_uuid+" is now mapped with "+populationTable+" at risk.");
        }
        else {
            System.out.println("SLR_UUID: "+slr_uuid+" has already been mapped with "+populationTable+" at risk, data skipped.");
        }
    }

     public void mapLandplotAtRisk(RemoteRDBStoreClient remoteRDBStoreClient, String slr_uuid, String landplotTable)throws SQLException{

         //check if slr_uuid ID exists in slr_population
         if (!isSLRDataExistsInSLRTable(remoteRDBStoreClient, slr_uuid, landplotTable)) {

             try (Connection connection = remoteRDBStoreClient.getConnection()) {
                 String landplotInsertSQL = "INSERT INTO slr_landplot (slr_uuid, landplot_uuid, affectedarea)\n" +
                                             "WITH slr AS (\n" +
                                             "    SELECT uuid, geom\n" +
                                             "    FROM sealevelprojections\n" +
                                             "    WHERE uuid='"+slr_uuid+"'\n" +
                                             "    ORDER BY sealevelriseinmeters DESC\n" +
                                             ")\n" +
                                             "SELECT\n" +
                                             "    slr.uuid AS slr_uuid,\n" +
                                             "    lp.ogc_fid AS lp_uuid,\n" +
                                             "    ROUND(ST_AREA(ST_TRANSFORM(ST_INTERSECTION(slr.geom, ST_MAKEVALID(lp.\"lod1Geometry\")), 3857))::numeric, 2) AS affectedarea\n" +
                                             "FROM slr\n" +
                                             "JOIN landplot lp ON ST_INTERSECTS(slr.geom, lp.\"lod1Geometry\");";

                 executeSql(connection, landplotInsertSQL);
             }
             System.out.println("SLR_UUID: "+slr_uuid+" is now mapped with "+landplotTable+" at risk.");
         }
         else {
             System.out.println("SLR_UUID: "+slr_uuid+" has already been mapped with "+landplotTable+" at risk, data skipped.");
         }


     }


    public void mapCulturalSitesAtRisk(RemoteRDBStoreClient remoteRDBStoreClient, String slr_uuid, String culturalsiteTable)throws SQLException{

        //check if slr_uuid ID exists in slr_population
        if (!isSLRDataExistsInSLRTable(remoteRDBStoreClient, slr_uuid, culturalsiteTable)) {

            try (Connection connection = remoteRDBStoreClient.getConnection()) {
                String culturalsitesInsertSQL = "INSERT INTO slr_"+ culturalsiteTable +" (slr_uuid, "+ culturalsiteTable +"_uuid)\n" +
                                                "WITH slr AS (\n" +
                                                "    SELECT geom, uuid\n" +
                                                "    FROM \"sealevelprojections\"\n" +
                                                "    WHERE uuid='"+slr_uuid+"'\n" +
                                                "),\n" +
                                                "     "+ culturalsiteTable +" AS (\n" +
                                                "         SELECT uuid, wkb_geometry\n" +
                                                "         FROM "+ culturalsiteTable +"\n" +
                                                "     )\n" +
                                                "SELECT slr.uuid as slr_uuid, "+ culturalsiteTable +".uuid as "+ culturalsiteTable +"_uuid\n" +
                                                "FROM slr,"+ culturalsiteTable +"\n" +
                                                "WHERE ST_INTERSECTS(slr.geom, "+ culturalsiteTable +".wkb_geometry)";

                executeSql(connection, culturalsitesInsertSQL);
            }
            System.out.println("SLR_UUID: "+slr_uuid+" is now mapped with "+culturalsiteTable+" at risk.");
        }
        else {
            System.out.println("SLR_UUID: "+slr_uuid+" has already been mapped with "+culturalsiteTable+" at risk, data skipped.");
        }
    }

    public void mapBuildingAtRisk(RemoteRDBStoreClient remoteRDBStoreClient, String slr_uuid, String buildingTable)throws SQLException{

        //create materialized view if not exists
        if (!isCityDBMaterializedViewExists(remoteRDBStoreClient, buildingTable)){
            createCityDBMaterializedView(remoteRDBStoreClient,buildingTable);
        }

        //check if slr_uuid ID exists in slr_population
        if (!isSLRDataExistsInSLRTable(remoteRDBStoreClient, slr_uuid, buildingTable)) {

            try (Connection connection = remoteRDBStoreClient.getConnection()) {
                String buildingInsertSQL = "INSERT INTO slr_"+ buildingTable +" (slr_uuid, "+ buildingTable +"_uuid)\n" +
                        "WITH slr AS (\n" +
                        "    SELECT geom, uuid\n" +
                        "    FROM \"sealevelprojections\"\n" +
                        "    WHERE uuid='"+slr_uuid+"'\n" +
                        "),\n" +
                        "     "+ buildingTable +" AS (\n" +
                        "         SELECT uuid, ST_TRANSFORM(geometry,4326) as geometry\n" +
                        "         FROM "+ buildingTable +"\n" +
                        "     )\n" +
                        "SELECT slr.uuid as slr_uuid, "+ buildingTable +".uuid as "+ buildingTable +"_uuid\n" +
                        "FROM slr,"+ buildingTable +"\n" +
                        "WHERE ST_INTERSECTS(slr.geom, "+ buildingTable +".geometry)";

                executeSql(connection, buildingInsertSQL);
            }
            System.out.println("SLR_UUID: "+slr_uuid+" is now mapped with "+ buildingTable +" at risk.");
        }
        else {
            System.out.println("SLR_UUID: "+slr_uuid+" has already been mapped with "+ buildingTable +" at risk, data skipped.");
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


    private boolean isSLRDataExistsInSLRTable (RemoteRDBStoreClient remoteRDBStoreClient, String slr_uuid, String tableName) throws SQLException {

        String slrImpactTable = "slr_"+tableName;

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
                Statement statement = connection.createStatement();
                ResultSet resultSet = statement.executeQuery("SELECT 1 FROM "+slrImpactTable+" WHERE slr_uuid ='"+slr_uuid+"'");

                if (resultSet.next()) {
                    //slr_uuid exists in table
                    return true;
                } else {
                    //slr_uuid does not exist in table
                    return false;
                }
        }
    }

    private void createCityDBMaterializedView (RemoteRDBStoreClient remoteRDBStoreClient, String cityDBMaterializedViewName) throws SQLException{

        String createCityDBMaterializedView_sql = "CREATE MATERIALIZED VIEW public."+cityDBMaterializedViewName+" AS\n" +
                                                    "WITH \"uuid_table\" AS (\n" +
                                                    "    SELECT\n" +
                                                    "        \"strval\" AS \"uuid\",\n" +
                                                    "        \"cityobject_id\"\n" +
                                                    "    FROM\n" +
                                                    "        \"citydb\".\"cityobject_genericattrib\"\n" +
                                                    "    WHERE\n" +
                                                    "        \"attrname\" = 'uuid'\n" +
                                                    "),\n" +
                                                    "\"iri_table\" AS (\n" +
                                                    "    SELECT\n" +
                                                    "        \"urival\" AS \"iri\",\n" +
                                                    "        \"cityobject_id\"\n" +
                                                    "    FROM\n" +
                                                    "        \"citydb\".\"cityobject_genericattrib\"\n" +
                                                    "    WHERE\n" +
                                                    "        \"attrname\" = 'iri'\n" +
                                                    ")\n" +
                                                    "SELECT DISTINCT\n" +
                                                    "    \"building\".\"id\" AS \"building_id\",\n" +
                                                    "    COALESCE(\"measured_height\", 100.0) AS \"building_height\",\n" +
                                                    "    \"geometry\",\n" +
                                                    "    \"uuid\",\n" +
                                                    "    \"iri\"\n" +
                                                    "FROM\n" +
                                                    "    \"citydb\".\"building\"\n" +
                                                    "    JOIN \"citydb\".\"surface_geometry\" ON \"citydb\".\"surface_geometry\".\"root_id\" = \"citydb\".\"building\".\"lod0_footprint_id\"\n" +
                                                    "    JOIN \"uuid_table\" ON \"citydb\".\"building\".\"id\" = \"uuid_table\".\"cityobject_id\"\n" +
                                                    "    JOIN \"iri_table\" ON \"citydb\".\"building\".\"id\" = \"iri_table\".\"cityobject_id\"\n" +
                                                    "WHERE\n" +
                                                    "    \"citydb\".\"surface_geometry\".\"geometry\" IS NOT NULL;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, createCityDBMaterializedView_sql);
        }
    }

    private boolean isCityDBMaterializedViewExists (RemoteRDBStoreClient remoteRDBStoreClient, String cityDBMaterializedViewName) throws SQLException {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            DatabaseMetaData metaData = connection.getMetaData();
            ResultSet resultSet = metaData.getTables(null, null, cityDBMaterializedViewName,null);

            if (resultSet.next()) {
                //view exists
                return true;
            } else {
                //slr_uuid does not exist in table
                return false;
            }
        }
    }



}



