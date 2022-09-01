package uk.ac.cam.cares.jps.agent.weather;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.Record;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.exception.DataAccessException;
import org.jooq.impl.DSL;

/**
 * mainly to create a new database, there is a weird driver issue using PostGISClient from stack-clients
 */
public class WeatherPostGISClient {
    private Connection conn = null;
    private DSLContext context;
	private Table<Record> table = DSL.table(DSL.name(Config.LAYERNAME));
    private static final Logger LOGGER = LogManager.getLogger(WeatherPostGISClient.class);
	private String dburl;
	private String dbuser;
	private String dbpassword;

	WeatherPostGISClient(String dburl, String dbuser, String dbpassword) {
		this.dburl = dburl;
		this.dbuser = dbuser;
		this.dbpassword = dbpassword;
	}

    /**
	 * Establish connection to RDB and set DSL context
	 */
	protected void connect() {
		try {
			if (this.conn == null || this.conn.isClosed()) {
				// Load required driver
				Class.forName("org.postgresql.Driver");
				// Connect to DB (using static connection and context properties)
	        	this.conn = DriverManager.getConnection(this.dburl, this.dbuser, this.dbpassword);
	        	this.context = DSL.using(this.conn, SQLDialect.POSTGRES); 
	        	System.out.println("Connecting successful: " + Config.dburl); 
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println("Connecting failed: " + Config.dburl);
			throw new RuntimeException("Establishing database connection failed");
		}
    }

    boolean checkTableExists(String table) {
        connect();
        try {
			String condition = String.format("table_name = '%s'", table);
            return context.select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 1;
        } catch (DataAccessException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

	boolean checkPointExists(double lat, double lon) {
		connect();
		try {
			String sql = String.format("SELECT ST_Equals(wkb_geometry, ST_SetSRID(ST_POINT(%f,%f),4326)) from %s",lon,lat, Config.LAYERNAME);
			Statement stmt = conn.createStatement();
			ResultSet result = stmt.executeQuery(sql);
			boolean pointExists = false;
			while (result.next()) {
				if(result.getBoolean("st_equals")) {
					pointExists = true;
					break;
				}
			}
			return pointExists;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e);
		}
	}

	void deleteRow(String iri) {
		connect();
		context.deleteFrom(table).where(DSL.field("iri").eq(iri)).execute();
	}
}
