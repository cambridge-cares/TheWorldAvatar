package uk.ac.cam.cares.jps.agent.osmagent.usage;

import java.io.IOException;
import java.io.InputStreamReader;
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
import uk.ac.cam.cares.jps.agent.osmagent.OSMAgent;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.object.GeoObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class UsageMatcher {
    /**
     * Inputs database name, postgis username, postgis password and subsequently update the OSM rows with ontobuilt category by
     * running SQL query to categorize as according to osm_tags.csv
     * @param database
     * @param user
     * @param password
     */
    public static void updateOntoBuilt(String database, String user, String password, String pointTable, String polygonTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        List<String> tableNames = Arrays.asList(polygonTable, pointTable);

        for (String tableName : tableNames) {

            try {

                InputStreamReader inputStreamReader = new InputStreamReader(
                        UsageMatcher.class.getResourceAsStream("/osm_tags.csv"));
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
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Inputs database name, postgis username, postgis password.
     * Check if "building_iri", "propertyusage_iri", "ontobuilt", "usageshare" columns exist, if not create it
     * @param database
     * @param user
     * @param password
     */
    public static void checkAndAddColumns(String database, String user, String password,  String pointTable, String polygonTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

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

    public static void copyFromOSM(String database, String user, String password, String pointTable, String polygonTable, String usageTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

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

    public static void unmatchedBuilding(String database, String user, String password, String usageTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        String subQuery = GeoObject.getQuery();

        String insert = "INSERT INTO " + usageTable + " (building_iri) \n" +
                "SELECT q.urival FROM (\n" +
                subQuery +
                ") as q \n" +
                "LEFT JOIN " + usageTable + " u ON q.urival = u.building_iri \n" +
                "WHERE u.building_iri IS NULL";

        rdbStoreClient.executeUpdate(insert);
    }

    /**
     * Check if the column name exists
     * @param connection
     * @param tableNamewSchema
     * @param columnName
     * @return
     * @throws SQLException
     */
    private static boolean isColumnExist(Connection connection, String tableNamewSchema, String columnName)
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
     * Create connections to remoteStoreClient and execute SQL statements
     * @param connection
     * @param sql
     * @throws SQLException
     */
    private static void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }
}
