package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.jooq.SQLDialect;
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
    DockerImageName myImage = DockerImageName.parse("postgis/postgis:14-3.3").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(myImage);

    // RDB client
    private TimeSeriesRDBClient<Integer> tsClient;

    @Before
    public void initialiseRDBClientAndTable() {
        postgres.start();
        tsClient = new TimeSeriesRDBClient<>(Integer.class);
        tsClient.setRdbURL(postgres.getJdbcUrl());
        tsClient.setRdbUser(postgres.getUsername());
        tsClient.setRdbPassword(postgres.getPassword());

        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class),
                "http://ts1", 4326);
    }

    @After
    public void clearDatabase() throws SQLException {
        try (Connection conn = tsClient.getConnection()) {
            tsClient.deleteAll(conn);
        }
    }

    /**
     * simple test that checks the number of columns is correct
     * 
     * @throws SQLException
     */
    @Test
    public void testInitTimeSeriesTable() throws SQLException {
        // 1 for time column and 1 for the geometry column
        try (Connection conn = tsClient.getConnection()) {
            DSLContext context = DSL.using(tsClient.getConnection(), SQLDialect.POSTGRES);

            // including time and tsIRI columns
            Assert.assertEquals(3, context.meta().getTables("time_series_data").get(0).fields().length);
        }
    }

    /**
     * uploading a geometry with the wrong srid will throw an exception
     * the column was initialised with 4326 and this function tries to upload a
     * point with 4325
     */
    @Test
    public void testWrongSRID() {
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
}
