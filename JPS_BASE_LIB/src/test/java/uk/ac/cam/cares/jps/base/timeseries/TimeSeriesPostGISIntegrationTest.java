package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.After;
import org.junit.AfterClass;
import org.postgis.Point;
import org.postgis.Polygon;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class TimeSeriesPostGISIntegrationTest {
    private static final String user = "postgres";
    private static final String password = "postgres";
    // Create Docker container with postgis image from Docker Hub
    private static DockerImageName myImage = DockerImageName.parse("postgis/postgis:14-3.3")
            .asCompatibleSubstituteFor("postgres");
    @Container
    private static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(myImage).withUsername(user)
            .withPassword(password);
    // RDB client
    protected TimeSeriesRDBClientInterface<Integer> tsClient;
    private static RemoteRDBStoreClient rdbStoreClient;

    @BeforeClass
    // Initialise RemoteRDBStoreClient before any test
    public static void initialiseRDBStoreClient() {
        postgres.start();
        // RemoteRDBStoreClient
        rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), user, password);
    }

    @Before
    public void initialiseRDBClient() {
        setRdbClient();
    }

    protected void setRdbClient() {
        tsClient = new TimeSeriesRDBClient<>(Integer.class);
    }

    /**
     * uploading a geometry with the wrong srid will throw an exception
     * the column was initialised with 4326 and this function tries to upload a
     * point with 4325
     */
    @Test
    public void testWrongSRID() throws SQLException {
        // a dummy point
        Point point = new Point();
        point.setX(1);
        point.setY(1);
        point.setSrid(4325);
        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(point));

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                    "http://ts1", 4326, conn);
            TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"),
                    values);
            // upload to database
            JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.addTimeSeriesData(Arrays.asList(tsUpload), conn));
            Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
        }
    }

    /**
     * uploading the wrong geometry type will throw an exception
     * 
     * @throws SQLException
     */
    @Test
    public void testWrongGeometry() throws SQLException {
        Polygon polygon = new Polygon("POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))");
        polygon.setSrid(4326);

        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(polygon));

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                    "http://ts1", 4326, conn);
            TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"),
                    values);
            // upload to database
            JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.addTimeSeriesData(Arrays.asList(tsUpload), conn));
            Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
        }
    }

    /**
     * uploads dummy data, queries it and checks if it's the same
     */
    @Test
    public void testAddTimeSeriesData() throws SQLException {

        Point point = new Point();
        point.setX(1);
        point.setY(1);
        point.setSrid(4326);
        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(point));

        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                    "http://ts1", 4326, conn);
            // upload data
            TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"),
                    values);
            tsClient.addTimeSeriesData(Arrays.asList(tsUpload), conn);

            // query and check if it's the same
            Point queriedPoint = tsClient.getTimeSeries(Arrays.asList("http://data1"), conn)
                    .getValuesAsPoint("http://data1").get(0);
            Assert.assertTrue(queriedPoint.equals(point));
        }
    }

    @After
    // Clear all tables after each test to ensure clean slate
    public void clearDatabase() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.deleteAll(conn);
        }
    }
}
