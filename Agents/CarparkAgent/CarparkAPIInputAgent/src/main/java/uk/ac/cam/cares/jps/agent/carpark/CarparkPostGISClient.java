package uk.ac.cam.cares.jps.agent.carpark;

import java.sql.Connection;
import java.sql.SQLException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.exception.DataAccessException;
import org.jooq.impl.DSL;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

/**
 * Client to interact with postGIS and carry out custom queries
 */
public class CarparkPostGISClient extends ContainerClient {
	/**
     * Logger for reporting info/errors.
     */
	private static final Logger LOGGER = LogManager.getLogger(CarparkPostGISClient.class);

	private RemoteRDBStoreClient remoteRDBStoreClient;
	private PostGISEndpointConfig postGISEndpointConfig = null;

	/**
	 * Carpark postGIS client constructor
	 */
	CarparkPostGISClient() {
		//geolocation data will be instantiated in the stack's postgres database
        postGISEndpointConfig = this.readEndpointConfig("postgis",PostGISEndpointConfig.class);
		remoteRDBStoreClient = new RemoteRDBStoreClient(postGISEndpointConfig.getJdbcURL(CarparkAgent.DATABASE), postGISEndpointConfig.getUsername(), postGISEndpointConfig.getPassword());
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
    public boolean checkTableExists(String table, Connection conn) {
        try {
			String condition = String.format("table_name = '%s'", table);
            return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 1;
        } catch (DataAccessException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

	/**
	 * Check whether a carpark instance already exist in a table
	 * @param carparkIRI carpark IRI to check for
	 * @param conn connection sesssion
	 * @return True or False
	 */
	public boolean checkCarparkExists(String carparkIRI, Connection conn) {
		String condition = String.format("carpark_iri = '%s'", carparkIRI);
            return getContext(conn).select(DSL.count()).from(CarparkAgent.LAYERNAME).where(condition).fetchOne(0, int.class) > 0;
	}

	DSLContext getContext(Connection conn) {
		return DSL.using(conn, SQLDialect.POSTGRES);
	}
}
