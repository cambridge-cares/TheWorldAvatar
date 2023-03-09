package uk.ac.cam.cares.jps.agent.weather;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
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

	Connection getConnection() throws SQLException {
		try {
			Class.forName("org.postgresql.Driver");
		} catch (ClassNotFoundException e) {
			LOGGER.error(e.getMessage());
			LOGGER.error("Failed to find postgre driver");
		}
		return DriverManager.getConnection(this.dburl, this.dbuser, this.dbpassword);
	}

    boolean checkTableExists(String table, Connection conn) {
        try {
			String condition = String.format("table_name = '%s'", table);
            return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 1;
        } catch (DataAccessException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

	boolean checkPointExists(double lat, double lon, Connection conn) {
		String sql = String.format("SELECT ST_Equals(wkb_geometry, ST_SetSRID(ST_POINT(%f,%f),4326)) from %s",lon,lat, Config.LAYERNAME);
		try (Statement stmt = conn.createStatement()) {
			ResultSet result = stmt.executeQuery(sql);
			boolean pointExists = false;
			while (result.next()) {
				if(result.getBoolean("st_equals")) {
					pointExists = true;
					break;
				}
			}
			return pointExists;
		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
			return true; // to stop the code from proceeding
		}
	}

	void deleteRow(String iri, Connection conn) {
		getContext(conn).deleteFrom(table).where(DSL.field("iri").eq(iri)).execute();
	}

	DSLContext getContext(Connection conn) {
		return DSL.using(conn, SQLDialect.POSTGRES);
	}
}
