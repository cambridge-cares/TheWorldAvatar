package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.postgis.Point;
import org.postgis.Polygon;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.containers.PostgreSQLContainer;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/*
 * this class contains tests for methods that do not have a connection object in its argument
 */
public class TimeSeriesPostGISIntegrationWithoutConnTest {
    // Will create two Docker containers for Blazegraph and postgreSQL
    // NOTE: requires access to the docker.cmclinnovations.com registry from the
    // machine the test is run on

    // Create Docker container with postgis image from Docker Hub
    private static DockerImageName myImage = DockerImageName.parse("postgis/postgis:14-3.3")
            .asCompatibleSubstituteFor("postgres");
    @Container
    private static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(myImage);

    // RDB client
    protected TimeSeriesRDBClientInterface<Integer> tsClient;

    @Before
    public void initialiseRDBClientAndTable() {
        postgres.start();
        setRdbClient();
        tsClient.setRdbURL(postgres.getJdbcUrl());
        tsClient.setRdbUser(postgres.getUsername());
        tsClient.setRdbPassword(postgres.getPassword());
    }

    protected void setRdbClient() {
        tsClient = new TimeSeriesRDBClient<>(Integer.class);
    }

    @After
    public void clearDatabase() throws SQLException {
        tsClient.deleteAll();
    }

    /**
     * uploading a geometry with the wrong srid will throw an exception
     * the column was initialised with 4326 and this function tries to upload a
     * point with 4325
     */
    @Test
    public void testWrongSRID() {
        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                "http://ts1", 4326);
        // a dummy point
        Point point = new Point();
        point.setX(1);
        point.setY(1);
        point.setSrid(4325);
        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(point));

        TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"), values);

        // upload to database
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> tsClient.addTimeSeriesData(Arrays.asList(tsUpload)));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    /**
     * uploading the wrong geometry type will throw an exception
     * 
     * @throws SQLException
     */
    @Test
    public void testWrongGeometry() throws SQLException {
        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                "http://ts1", 4326);
        Polygon polygon = new Polygon("POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))");
        polygon.setSrid(4326);

        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(polygon));

        TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"), values);
        // upload to database
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> tsClient.addTimeSeriesData(Arrays.asList(tsUpload)));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    /**
     * uploads dummy data, queries it and checks if it's the same
     */
    @Test
    public void testAddTimeSeriesData() {
        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                "http://ts1", 4326);
        Point point = new Point();
        point.setX(1);
        point.setY(1);
        point.setSrid(4326);
        List<List<?>> values = new ArrayList<>();
        values.add(Arrays.asList(point));

        // upload data
        TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"), values);
        tsClient.addTimeSeriesData(Arrays.asList(tsUpload));

        // query and check if it's the same
        Point queriedPoint = tsClient.getTimeSeries(Arrays.asList("http://data1")).getValuesAsPoint("http://data1")
                .get(0);
        Assert.assertTrue(queriedPoint.equals(point));
    }

    @Test
    public void testAddTimeSeriesDataWithArray() {
        Point point1 = new Point();
        point1.setX(1);
        point1.setY(1);
        point1.setSrid(4326);

        Point point2 = new Point();
        point2.setX(1);
        point2.setY(1);
        point2.setSrid(4326);

        List<List<?>> values = new ArrayList<>();

        List<Point[]> value = new ArrayList<>();
        value.add(new Point[] { point1, point2 });
        value.add(new Point[] { point2, point1 });

        values.add(value);

        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point[].class), "http://ts1", 4326);
        // upload data
        TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1, 2), Arrays.asList("http://data1"),
                values);
        tsClient.addTimeSeriesData(Arrays.asList(tsUpload));

        // query and check if it's the same
        TimeSeries<Integer> queriedTimeSeries = tsClient.getTimeSeries(Arrays.asList("http://data1"));

        List<Integer> times = queriedTimeSeries.getTimes();
        List<Point[]> results = queriedTimeSeries.getValuesAsPointsArray("http://data1");

        Assert.assertEquals(1, (int) times.get(0));
        Assert.assertTrue(point1.equals(results.get(0)[0]));
        Assert.assertTrue(point2.equals(results.get(0)[1]));

        Assert.assertEquals(2, (int) times.get(1));
        Assert.assertTrue(point2.equals(results.get(0)[0]));
        Assert.assertTrue(point1.equals(results.get(0)[1]));

    }
}
