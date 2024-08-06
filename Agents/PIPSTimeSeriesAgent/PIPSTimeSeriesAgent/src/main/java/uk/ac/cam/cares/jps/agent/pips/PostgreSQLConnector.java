package uk.ac.cam.cares.jps.agent.pips;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.sql.Timestamp;
import org.json.JSONArray;
import org.json.JSONObject;
import java.time.ZoneOffset;
import java.time.OffsetDateTime;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class PostgreSQLConnector {

    // variables
    private String dbUrl = null;
    private String username = null;
    private String password = null;
    private static final String JSON_RESULT_KEY = "Result";

    private static final String POSTGRESQL_PROP = "POSTGRESQL_PROP";

    // error messages
    private static final String READ_PROPERTIES_ERROR = "Unable to get read PostgreSQL parameters from properties file!";

    /**
     * Constructor for PostgreSQLConnector
     */
    public PostgreSQLConnector() {
        try {
            //read properties filepath from env
            String filePath = System.getenv(POSTGRESQL_PROP);
            //read jdbc url, user, password from properties file
            loadConfigs(filePath);
        } catch (Exception e) {
            throw new JPSRuntimeException(READ_PROPERTIES_ERROR, e);
        }
    }

    /**
     * Establish connection to RDB
     * @return connection object to the RDB
     * @throws SQLException
     */
    public Connection getConnection() throws SQLException{
        try {
            Class.forName("org.postgresql.Driver");
            return DriverManager.getConnection(this.dbUrl, this.username, this.password);
        } catch (ClassNotFoundException e) {
            throw new JPSRuntimeException("Failed to load driver for postgresql", e);
        }
    }

    /**
     * Retrieve timeseries data from a PostgreSQL database
     * @param schema schema in which to retrieve data from
     * @param number number of readings to retrieve
     * @return JSONObject containing the timeseries data
     * @throws SQLException
     */
    public JSONObject retrieveTimeSeries(String schema, int number) throws SQLException {
        JSONObject timeseriesData = new JSONObject();
        try (Connection conn = getConnection()) {
            // Get the list of tables in the schema
            String tableQuery = "SELECT table_name FROM information_schema.tables WHERE table_schema = '" + schema + "'";
            try (PreparedStatement tableStmt = conn.prepareStatement(tableQuery);
                 ResultSet tablerResultSet = tableStmt.executeQuery()) {

                // Step 4: Iterate through the tables and retrieve the latest 10 readings
                List<String> tables = new ArrayList<>();
                while (tablerResultSet.next()) {
                    tables.add(tablerResultSet.getString("table_name"));
                }
                tablerResultSet.close();

                // Iterate over each table and fetch the latest x number of readings
                for (String table : tables) {
                    JSONArray tableArray = new JSONArray();
                    String query = String.format("SELECT * FROM %s.%s ORDER BY timestamp DESC LIMIT %s", schema, table, number);
                    try (PreparedStatement pstmt = conn.prepareStatement(query);
                        ResultSet timeseriesResultSet = pstmt.executeQuery()) {
                            ResultSetMetaData rsMetaData = timeseriesResultSet.getMetaData();
                            int columnCount = rsMetaData.getColumnCount();

                            System.out.println("Latest " + number + " readings from table: " + table);
                            while (timeseriesResultSet.next()) {
                                JSONObject dataForASpecificTS = new JSONObject();
                                for (int i = 1; i <= columnCount; i++) {
                                    String columnName = rsMetaData.getColumnName(i);
                                    // store timestamp as timestamp with timezone in JSONObject
                                    if (columnName.contains("timestamp") && timeseriesResultSet.getObject(i) != null) {
                                        Timestamp timestamp = timeseriesResultSet.getTimestamp(columnName);
                                        OffsetDateTime offsetDateTime = timestamp.toInstant().atOffset(ZoneOffset.UTC);
                                        dataForASpecificTS.put(columnName, offsetDateTime);
                                    } else {
                                        // account for null values
                                        if (timeseriesResultSet.getObject(i) == null) {
                                            dataForASpecificTS.put(columnName, "null");
                                        } else {
                                            dataForASpecificTS.put(columnName,timeseriesResultSet.getObject(i));   
                                        }
                                    }
                                }
                                tableArray.put(dataForASpecificTS);
                            }
                            timeseriesResultSet.close();
                        }
                        timeseriesData.put(table, tableArray);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            if (timeseriesData.isEmpty()) {
                timeseriesData.put(JSON_RESULT_KEY, "No data retrieved...");
            }
            return timeseriesData;
        }
    

    /**
     * Reads the PostgreSQL parameters from a properties file and saves it in fields.
     * @param filepath Path to the properties file
     */
    private void loadConfigs(String filepath) throws IOException{
        File file=new File(filepath);
        if(!file.exists()){
            throw new FileNotFoundException("There was no properties file found in the specified path: "+filepath);
        }
        try(FileInputStream input= new FileInputStream(file)){
            Properties prop=new Properties();
            prop.load(input);
            if (prop.containsKey("db.url")){
                this.dbUrl=prop.getProperty("db.url");
            }else{
                throw new IOException("The properties file is missing \"db.url=<db url>\"");
            }
            if (prop.containsKey("db.username")){
                this.username=prop.getProperty("db.username");
            }else{
                throw new IOException("The properties file is missing \"db.username=<db username>\"");
            }
            if (prop.containsKey("db.password")){
                this.password=prop.getProperty("db.password");
            }else{
                throw new IOException("The properties file is missing \"db.password=<db password>\"");
            }
        }
    }



}
