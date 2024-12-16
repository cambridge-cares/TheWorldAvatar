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

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

/**
 * Client to interact with postGIS and carry out custom queries
 */
public class WeatherPostGISClient extends ContainerClient {
	/**
     * Logger for reporting info/errors.
     */

	private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

	private Table<Record> table = DSL.table(DSL.name(CARESWeatherStationInputAgentLauncher.LAYERNAME));

	private RemoteRDBStoreClient remoteRDBStoreClient;
	private PostGISEndpointConfig postGISEndpointConfig = null;

	/**
	 * Weather postGIS client constructor
	 * @param dburl url of postGIS database
	 * @param dbuser username to access postGIS database
	 * @param dbpassword password to access postGIS database
	 */
	WeatherPostGISClient() {
		//geolocation data will be instantiated in the stack's postgres database
        postGISEndpointConfig = this.readEndpointConfig("postgis",PostGISEndpointConfig.class);
		remoteRDBStoreClient = new RemoteRDBStoreClient(postGISEndpointConfig.getJdbcURL(CARESWeatherStationInputAgentLauncher.DATABASE), postGISEndpointConfig.getUsername(), postGISEndpointConfig.getPassword());
	}

	/**
	 * To create a connection session with a database
	 * @return connection session
	 * @throws SQLException
	 */
	Connection getConnection() throws SQLException {
		return remoteRDBStoreClient.getConnection();
	}

	/**
	 * Check whether a table exist in a database
	 * @param table name of table
	 * @param conn connection session
	 * @return True or False
	 */
    boolean checkTableExists(String table, Connection conn) {
        try {
			String condition = String.format("table_name = '%s'", table);
            return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 1;
        } catch (DataAccessException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

	/**
	 * Check whether a (long, lat) point exist in the database
	 * @param lat latitude
	 * @param lon longitude
	 * @param conn connection session
	 * @return True or False
	 */
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
	
	DSLContext getContext(Connection conn) {
		return DSL.using(conn, SQLDialect.POSTGRES);
	}
}
