package uk.ac.cam.cares.jps.agent.pips;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

public class PostgreSQLConnectorTest {

    // JDBC URL, username and password of the database
    private static final String URL = "jdbc:postgresql://test_pips_timeseries_agent_postgres:5432/postgres";
    private static final String USERNAME = "postgres";
    private static final String PASSWORD = "postgres";
    private static final String SCHEMA = "test";
    private static final String TABLE_01 = "table1";
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    /**
     * Insert mock data into postgresql
     * @throws InterruptedException
     */
    @Before
    public void insertMockData() throws InterruptedException {
        TimeUnit.SECONDS.sleep(10);
        Connection connection = null;
        try {
            // Establish the connection
            connection = DriverManager.getConnection(URL, USERNAME, PASSWORD);

            // Create a statement
            Statement stmt = connection.createStatement();

            // SQL statement to create the schema
            String createSchemaSQL = "CREATE SCHEMA IF NOT EXISTS " + SCHEMA +";";
            stmt.executeUpdate(createSchemaSQL);

            // SQL statement to create a table under a specific schema
            String createTableSQL = "CREATE TABLE " + SCHEMA + "." + TABLE_01 + " (" +
                                    "timestamp TIMESTAMP WITH TIME ZONE, " +
                                    "pressure_A DOUBLE PRECISION" +
                                    ");";

            stmt = connection.createStatement();

            // Execute the SQL statement
            stmt.executeUpdate(createTableSQL);

            // SQL statement to insert values
            String insertSQL = "INSERT INTO " + SCHEMA + "." + TABLE_01 + " (timestamp, pressure_A) VALUES (?, ?)";

            // Create a PreparedStatement
            PreparedStatement pstmt = connection.prepareStatement(insertSQL);

            // Set the timestamp and pressure_A values
            String timestamp = "2022-07-11T16:10:00";
            Double pressureA = 10.0;

            pstmt.setObject(1, OffsetDateTime.of(LocalDateTime.parse(timestamp), ZONE_OFFSET));
            pstmt.setDouble(2, pressureA);

            // Execute the insert statement
            pstmt.executeUpdate();

        } catch (SQLException e) {
            System.out.println(e.toString());
        } finally {
            try {
                // Close the connection
                if (connection != null) {
                    connection.close();
                }
            } catch (SQLException ex) {
                System.out.println(ex.toString());
            }
        }
    }

    /**
     * Write to file
     * @param filepath path of file to write to
     * @param text String of characters to write into file
     * @throws IOException
     */
    private void writeToFile(String filepath, List<String> properties) throws IOException {
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }
    /**
     * Test PostgreSQLConnector constructor and getConnection
     * @throws Exception
     */
    @Test
    public void testConstructorAndConnection() throws Exception{
        String propertiesFile = Paths.get(folder.getRoot().toString(), "postgresql_connector.properties").toString();
        writeToFile(propertiesFile, Arrays.asList("db.url=" + URL, 
        "db.username=" + USERNAME, "db.password=" + PASSWORD));
        withEnvironmentVariable("POSTGRESQL_PROP", propertiesFile)
        .execute(() -> {
            PostgreSQLConnector postgreSQLConnector = new PostgreSQLConnector();
            Connection connection = postgreSQLConnector.getConnection();
            Assert.assertTrue(connection.isValid(10));
            }
        );
    }

    /**
     * Test PostgreSQLConnector.retrieveTimeSeries for incorrect schema with no data or non-existent schema
     * @throws Exception
     */
    @Test
    public void testRetrieveTimeSeriesNoDataRetrieved() throws Exception {
        String propertiesFile = Paths.get(folder.getRoot().toString(), "postgresql_connector.properties").toString();
        writeToFile(propertiesFile, Arrays.asList("db.url=" + URL, 
        "db.username=" + USERNAME, "db.password=" + PASSWORD));
        withEnvironmentVariable("POSTGRESQL_PROP", propertiesFile)
        .execute(() -> {
            PostgreSQLConnector postgreSQLConnector = new PostgreSQLConnector();
            JSONObject response = postgreSQLConnector.retrieveTimeSeries("postgres", 1);
            Assert.assertTrue(response.toString().contains("No data retrieved..."));
            }
        );
    }

    /**
     * Test PostgreSQLConnector.retrieveTimeSeries for correct schema with populated tables
     * @throws Exception
     */
    @Test
    public void testRetrieveTimeSeriesSuccess() throws Exception {
        String propertiesFile = Paths.get(folder.getRoot().toString(), "postgresql_connector.properties").toString();
        writeToFile(propertiesFile, Arrays.asList("db.url=" + URL, 
        "db.username=" + USERNAME, "db.password=" + PASSWORD));
        withEnvironmentVariable("POSTGRESQL_PROP", propertiesFile)
        .execute(() -> {
            PostgreSQLConnector postgreSQLConnector = new PostgreSQLConnector();
            JSONObject response = postgreSQLConnector.retrieveTimeSeries(SCHEMA, 1);
            Assert.assertEquals(response.toString(), "{\"table1\":[{\"pressure_a\":10,\"timestamp\":\"2022-07-11T16:10Z\"}]}");
            }
        );
    }
}
