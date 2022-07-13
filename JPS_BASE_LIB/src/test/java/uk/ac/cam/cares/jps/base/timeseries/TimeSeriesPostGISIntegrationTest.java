package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.postgis.Point;
import org.postgis.Polygon;

public class TimeSeriesPostGISIntegrationTest {
	// Using special testcontainers URL that will spin up a Docker container when accessed by a driver
	// (see: https://www.testcontainers.org/modules/databases/jdbc/). Note: requires Docker to be installed!
	private static final String dbURL = "jdbc:tc:postgis:14-3.2:///timeseries";
	private static final String user = "postgres";
	private static final String password = "postgres";
	
	private static Connection conn;
    private static DSLContext context;

	// RDB client
	private TimeSeriesRDBClient<Integer> tsClient;

    @BeforeClass
	// Connect to the database before any test (will spin up the Docker container for the database)
	public static void connect() throws SQLException, ClassNotFoundException {
		// Load required driver
		Class.forName("org.postgresql.Driver");
		// Connect to DB
		conn = DriverManager.getConnection(dbURL, user, password);
		context = DSL.using(conn, SQLDialect.POSTGRES);
	}

    @Before
	public void initialiseRDBClient() {
    	tsClient = new TimeSeriesRDBClient<>(Integer.class);
    	tsClient.setRdbURL(dbURL);
		tsClient.setRdbUser(user);
		tsClient.setRdbPassword(password);
	}	

    @After
    public void clearDatabase() {
        tsClient.deleteAll();
    }
    /**
     * this checks that the column type is set correctly for any postgis geometry classes
     * only Point and Polygon are included here but any should work
     */
    @Test
    public void testInitTimeSeriesTable() {
        String tableName = tsClient.initTimeSeriesTable(Arrays.asList("http://data1", "http://data2"), Arrays.asList(Point.class, Polygon.class), "http://ts1");
        
        // ignoring time column, all columns should be set to "geometry"
        Assert.assertTrue(context.meta().getTables(tableName).get(0).fieldStream().filter(f -> !f.getName().contentEquals("time"))
        .allMatch(f -> f.getDataType().getTypeName().contentEquals("geometry")));
    }

    /**
     * uploads dummy data to postgis, queries it and ensure they're the same
     */
    @Test
    public void testAddTimeSeriesData() {
        tsClient.initTimeSeriesTable(Arrays.asList("http://data1"), Arrays.asList(Point.class), "http://ts1");

        // a dummy point
        Point point = new Point();
		point.setX(1);
		point.setY(1);
		point.setSrid(4326);
		List<List<?>> values = new ArrayList<>();
		values.add(Arrays.asList(point));
		TimeSeries<Integer> tsUpload = new TimeSeries<Integer>(Arrays.asList(1), Arrays.asList("http://data1"), values);

        // upload to database
		tsClient.addTimeSeriesData(Arrays.asList(tsUpload));

        // query from database
        TimeSeries<Integer> tsQueried = tsClient.getTimeSeries(Arrays.asList("http://data1"));
        
        // check it's the same geometry
        Assert.assertTrue(tsQueried.getValuesAsPoint("http://data1").get(0).equals(point));
    }
}
