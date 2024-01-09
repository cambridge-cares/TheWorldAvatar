package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import java.sql.Connection;
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

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

/**
 * mainly to create a new database, there is a weird driver issue using PostGISClient from stack-clients
 */
public class WeatherPostGISClient {
	/**
     * Logger for reporting info/errors.
     */

	private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

	private Table<Record> table = DSL.table(DSL.name(CARESWeatherStationInputAgentLauncher.LAYERNAME));

	private RemoteRDBStoreClient remoteRDBStoreClient;

	WeatherPostGISClient(String dburl, String dbuser, String dbpassword) {
		remoteRDBStoreClient = new RemoteRDBStoreClient(dburl, dbuser, dbpassword);
	}

	Connection getConnection() throws SQLException {
		return remoteRDBStoreClient.getConnection();
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
		String sql = String.format("SELECT ST_Equals(wkb_geometry, ST_SetSRID(ST_POINT(%f,%f),4326)) from %s",lon,lat, CARESWeatherStationInputAgentLauncher.LAYERNAME);
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
