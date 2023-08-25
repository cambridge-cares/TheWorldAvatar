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
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class UsageMatcher {
    private static final List<String> tableNames = Arrays.asList(OSMAgent.POLYGON_TABLE, OSMAgent.POINT_TABLE);

    public static void updateOntoBuilt(String database, String user, String password) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        for (String tableName : tableNames) {

            try {

                InputStreamReader inputStreamReader = new InputStreamReader(
                        UsageMatcher.class.getResourceAsStream("/osmbase.csv"));
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

    public static void checkAndAddColumns(String database, String user, String password) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        Map<String, String> columns = new HashMap<>();
        columns.put("building_iri", "TEXT");
        columns.put("propertyusage_iri", "TEXT");
        columns.put("ontobuilt", "TEXT");
        columns.put("usageshare", "FLOAT");

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

    private static boolean isColumnExist(Connection connection, String tableName, String columnName)
            throws SQLException {
        DatabaseMetaData metaData = connection.getMetaData();
        try (ResultSet resultSet = metaData.getColumns(null, null, tableName, columnName)) {
            return resultSet.next();
        }
    }

    private static void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }
}
