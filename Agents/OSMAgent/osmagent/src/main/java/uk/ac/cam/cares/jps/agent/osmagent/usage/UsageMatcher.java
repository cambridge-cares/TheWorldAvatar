package uk.ac.cam.cares.jps.agent.osmagent.usage;

import java.io.*;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import uk.ac.cam.cares.jps.agent.osmagent.FileReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class UsageMatcher {
    private static final String OSM_CSV_PATH = "/resources/osm_tags.csv";

    RemoteRDBStoreClient rdbStoreClient;

    /**
     * @param database database URL
     * @param user username to database
     * @param password password to database
     */
    public UsageMatcher(String database, String user, String password) {
        this.rdbStoreClient  = new RemoteRDBStoreClient(database, user, password);
    }

    /**
     * Matches OSM usage to OntoBuiltEnv:PropertyUsage class and updates pointTable and polygonTable accordingly
     * @param pointTable table containing OSM data with the geometries being points
     * @param polygonTable table containing OSM data with the geometries being polygons
     */
    public void updateOntoBuilt(String pointTable, String polygonTable) {
        List<String> tableNames = Arrays.asList(polygonTable, pointTable);

        for (String tableName : tableNames) {
            try (InputStream input = FileReader.getStream(OSM_CSV_PATH)){
                InputStreamReader inputStreamReader = new InputStreamReader(input);
                CSVReader csvReader = new CSVReaderBuilder(inputStreamReader).withSkipLines(1).build();
                String[] line;

                Map<String, StringBuilder> hashmap = new HashMap<>();

                while ((line = csvReader.readNext()) != null) {
                    String ontobuilt = line[3];
                    String key = line[0];
                    String value = line[1];

                    if (!ontobuilt.equals("NA") && !ontobuilt.isBlank()) {

                        if (!hashmap.containsKey(ontobuilt)) {
                            hashmap.put(ontobuilt, new StringBuilder(
                                    "UPDATE " + tableName + " SET ontobuilt = '" + ontobuilt + "' WHERE "));
                        }

                        if (!(tableName.equals("points") && key.equals("leisure"))) {
                            StringBuilder sqlStatement = hashmap.get(ontobuilt);
                            sqlStatement.append(key).append(" = '").append(value).append("' OR ");
                        }
                    }
                }

                csvReader.close();

                for (Map.Entry<String, StringBuilder> entry : hashmap.entrySet()) {
                    String key = entry.getKey();
                    String sql = entry.getValue().toString();
                    // Remove the trailing "' OR " from the SQL statement
                    sql = sql.substring(0, sql.length() - 4);
                    if (key.equals("RetailFacility")) {
                        sql = sql.concat(" OR shop is NOT NULL");
                    } else if ((tableName.equals("polygons") && key.equals("Office"))) {
                        sql = sql.concat(" OR office IS NOT NULL");
                    }

                    sql = sql.concat(";");
                    System.out.println(sql);

                    // Execute the SQL statement
                    rdbStoreClient.executeUpdate(sql);
                }
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
                throw new JPSRuntimeException("osm_tags.csv file not found");
            }
            catch (IOException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Check and creates the columns building_iri, ontobuilt if they don't exist in pointTable and polygonTable
     * @param pointTable table containing OSM data with the geometries being points
     * @param polygonTable table containing OSM data with the geometries being polygons
     */
    public void checkAndAddColumns(String pointTable, String polygonTable) {
        List<String> tableNames = Arrays.asList(polygonTable, pointTable);

        Map<String, String> columns = new HashMap<>();
        columns.put("building_iri", "TEXT");
        columns.put("ontobuilt", "TEXT");

        try (Connection connection = rdbStoreClient.getConnection()) {
            for (String tableName : tableNames) {
                for (Map.Entry<String, String> entry : columns.entrySet()) {
                    if (!isColumnExist(connection, tableName, entry.getKey())) {
                        String addColumnSql = "ALTER TABLE " + tableName +
                                " ADD COLUMN " + entry.getKey() + " " + entry.getValue();
                        executeSql(connection, addColumnSql);
                    } else {
                        System.out.println("Column " + entry.getKey() + " already exists in " + tableName + ".");
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Intialise usageTable with building_iri, propertyusage_iri, ontobuilt, usageshare columns and copy from pointTable and polygonTable non-null building_iri and ontobuilt values
     * @param pointTable table containing OSM data with the geometries being points
     * @param polygonTable table containing OSM data with the geometries being polygons
     * @param usageTable centralised table to store usage information
     */
    public void copyFromOSM(String pointTable, String polygonTable, String usageTable) {
        String usageSchema = usageTable.split("\\.")[0];
        String initialiseSchema = "CREATE SCHEMA IF NOT EXISTS " + usageSchema;
        String initialiseTable = "CREATE TABLE IF NOT EXISTS " + usageTable;

        initialiseTable += " (building_iri TEXT, propertyusage_iri TEXT, ontobuilt TEXT, usageshare FLOAT)";

        rdbStoreClient.executeUpdate(initialiseSchema);
        rdbStoreClient.executeUpdate(initialiseTable);

        String copyIri = "INSERT INTO " + usageTable + " (building_iri, ontobuilt) " +
                "SELECT building_iri, ontobuilt FROM table WHERE table.building_iri IS NOT NULL AND table.ontobuilt IS NOT NULL";

        rdbStoreClient.executeUpdate(copyIri.replace("table", pointTable));
        rdbStoreClient.executeUpdate(copyIri.replace("table", polygonTable));
    }

    /**
     * Check if the column columnName exists
     * @param connection PostgreSQL connection object
     * @param tableNamewSchema table name with schema prefix
     * @param columnName name of column to check
     * @return true if column columnName exists, false otherwise
     */
    private boolean isColumnExist(Connection connection, String tableNamewSchema, String columnName)
            throws SQLException {
        DatabaseMetaData metaData = connection.getMetaData();

        // Split the tableName string by the dot (.) separator
        String[] tableNameParts = tableNamewSchema.split("\\.");
        String schema = tableNameParts[0];
        String table = tableNameParts[1];


        try (ResultSet resultSet = metaData.getColumns(null, schema, table, columnName)) {
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
