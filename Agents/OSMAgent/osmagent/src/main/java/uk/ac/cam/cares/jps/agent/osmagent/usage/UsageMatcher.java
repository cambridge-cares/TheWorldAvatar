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
import uk.ac.cam.cares.jps.agent.osmagent.OSMAgent;
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
        columns.put("area", "DOUBLE PRECISION");
        columns.put("building_iri", "TEXT");
        columns.put("ontobuilt", "TEXT");

        try (Connection connection = rdbStoreClient.getConnection()) {
            for (String tableName : tableNames) {
                for (Map.Entry<String, String> entry : columns.entrySet()) {
                    if (!isColumnExist(connection, tableName, entry.getKey())) {
                        String addColumnSql = "ALTER TABLE " + tableName +
                                " ADD COLUMN " + entry.getKey() + " " + entry.getValue();
                        executeSql(connection, addColumnSql);
                        if (tableName.equals(polygonTable) && entry.getKey().equals("area")) {
                            calculateArea(polygonTable);
                        }
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

    public void calculateArea(String polygonTable) {
        String update = String.format("UPDATE %s SET area = public.ST_Area(\"geometryProperty\"::public.geography)", polygonTable);

        rdbStoreClient.executeUpdate(update);
    }

    /**
     * Intialise usageTable with building_iri, propertyusage_iri, ontobuilt, usageshare columns and copy from pointTable and polygonTable non-null building_iri and ontobuilt values
     * @param pointTable table containing OSM data with the geometries being points
     * @param polygonTable table containing OSM data with the geometries being polygons
     * @param usageTable centralised table to store usage information
     */
    public void copyUsage(String pointTable, String polygonTable, String schema, String usageTable) {
        // initialise schema
        String initialiseSchema = "CREATE SCHEMA IF NOT EXISTS " + schema;
        rdbStoreClient.executeUpdate(initialiseSchema);

        // initialise usage table
        String initialiseUsageTable = "CREATE TABLE IF NOT EXISTS " + usageTable;

        initialiseUsageTable += " (building_iri TEXT, propertyusage_iri TEXT, ontobuilt TEXT, area DOUBLE PRECISION, source TEXT)";

        rdbStoreClient.executeUpdate(initialiseUsageTable);

        // insert usage data from OSM into usage table
        String copyUsage = "INSERT INTO %s (building_iri, ontobuilt, area, source)\n" +
                "SELECT o.building_iri, o.ontobuilt, o.area, %s FROM %s o\n" +
                "LEFT JOIN %s u on u.building_iri = o.building_iri AND u.ontobuilt = o.ontobuilt\n" +
                "WHERE u.building_iri IS NULL AND u.ontobuilt IS NULL\n" +
                "AND o.building_iri IS NOT NULL AND o.ontobuilt IS NOT NULL";

        rdbStoreClient.executeUpdate(String.format(copyUsage, usageTable, "\'osm_points\'", pointTable, usageTable));
        rdbStoreClient.executeUpdate(String.format(copyUsage, usageTable, "\'osm_polygons\'", polygonTable, usageTable));
    }

    public void copyAddress(String pointTable, String polygonTable, String schema, String addressTable) {
        // initialise schema
        String initialiseSchema = "CREATE SCHEMA IF NOT EXISTS " + schema;
        rdbStoreClient.executeUpdate(initialiseSchema);

        // initialise address table
        String initialiseAddressTable = "CREATE TABLE IF NOT EXISTS " + addressTable;

        initialiseAddressTable += " (building_iri TEXT, address_iri TEXT, country TEXT, city TEXT, street TEXT, house_number TEXT, postcode TEXT)";

        rdbStoreClient.executeUpdate(initialiseAddressTable);

        String copyAddress = "INSERT INTO %s (building_iri , country, city, street, house_number, postcode)\n" +
                "SELECT DISTINCT o.building_iri, o.addr_country, o.addr_city, o.addr_street, o.addr_housenumber, o.addr_postcode FROM %s o\n" +
                "WHERE o.building_iri NOT IN (SELECT building_iri FROM %s) AND o.building_iri is NOT null AND\n" +
                "COALESCE(o.addr_country, o.addr_city, o.addr_street, o.addr_housenumber, o.addr_postcode) IS NOT NULL";

        rdbStoreClient.executeUpdate(String.format(copyAddress, addressTable, polygonTable, addressTable));
        rdbStoreClient.executeUpdate(String.format(copyAddress, addressTable, pointTable, addressTable));

        String updateAddressIri = "UPDATE %s\n" +
                "SET address_iri = 'Address_' || uuid_generate_v4()::text\n" +
                "WHERE address_iri IS NULL";

        rdbStoreClient.executeUpdate(String.format(updateAddressIri, addressTable));
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
