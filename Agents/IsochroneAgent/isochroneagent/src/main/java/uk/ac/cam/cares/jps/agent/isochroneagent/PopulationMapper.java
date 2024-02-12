package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class PopulationMapper {

    /**
     * Execute SQL query to calculate population covered within by looping through the list of population tables.
     * @param remoteRDBStoreClient
     * @param populationTables
     */
    public void mapPopulation(RemoteRDBStoreClient remoteRDBStoreClient, ArrayList<String> populationTables){
        
    
        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            for (String populationTable : populationTables) {
                try {
                        String mapPopulation_sql="UPDATE isochrone_aggregated\n" +
                                                "SET "+populationTable+" = subquery.sum\n" +
                                                "FROM (\n" +
                                                "SELECT isochrone_aggregated.geom, SUM((ST_SummaryStats(ST_Clip("+populationTable+".rast, isochrone_aggregated.geom, TRUE))).sum) \n" +
                                                "FROM "+populationTable+", isochrone_aggregated\n" +
                                                "GROUP BY isochrone_aggregated.geom\n" +
                                                ") AS subquery\n" +
                                                "WHERE subquery.geom = isochrone_aggregated.geom;";
                        executeSql(connection, mapPopulation_sql);
                        System.out.println(populationTable+ "'s population sum successfully mapped with isochrone.");
                    
                } catch (Exception e) {
                    System.out.println(populationTable+ " unable to be mapped.");
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Check for each populationtables in config.properties and add them if it doesnt exist
     * @param remoteRDBStoreClient
     * @param populationTables list of population tables
     */
    public void checkAndAddColumns(RemoteRDBStoreClient remoteRDBStoreClient, ArrayList<String> populationTables) {

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            for (String populationTable : populationTables) {
                if (!isColumnExist(connection, populationTable)) {
                    String addColumnSql = "ALTER TABLE isochrone_aggregated"+
                            " ADD COLUMN " + populationTable+ " bigint";
                    executeSql(connection, addColumnSql);
                } else {
                    System.out.println("Column "+ populationTable+ " already exists in table isochrone_aggregated.");
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    public void addPostgisRasterAndGDALDriver(RemoteRDBStoreClient remoteRDBStoreClient, String DBname) throws SQLException {

        String sql= "ALTER DATABASE "+DBname+" SET postgis.enable_outdb_rasters = True;\n" +
                "ALTER DATABASE "+DBname+" SET postgis.gdal_enabled_drivers = 'GTiff';";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            try (Statement statement = connection.createStatement()) {
                statement.execute(sql);
            }
        }

    }

    /**
     * Check if the column columnName exists
     * @param connection PostgreSQL connection object
     * @param columnName name of column to check (populationTable)
     * @return true if column columnName exists, false otherwise
     */
    private boolean isColumnExist(Connection connection,  String columnName)throws SQLException {
        DatabaseMetaData metaData = connection.getMetaData();
        try (ResultSet resultSet = metaData.getColumns(null, null, "isochrone_aggregated", columnName)) {
            return resultSet.next();
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
