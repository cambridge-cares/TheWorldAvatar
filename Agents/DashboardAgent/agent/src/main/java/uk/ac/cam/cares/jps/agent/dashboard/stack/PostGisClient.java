package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * A client that provides method to interact and store information from the PostGIS container within a stack.
 *
 * @author qhouyee
 */
public class PostGisClient {
    private final String STACK_JDBC_URL;
    private final String STACK_POSTGIS_USER;
    private final String STACK_POSTGIS_PASSWORD;
    private final List<String> DATABASE_LIST = new ArrayList<>();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param jdbcUrl The JDBC url of the stack's PostGIS container, excluding the database name.
     * @param user    The username to access the stack's PostGIS container.
     * @param pass    The password to access the stack's PostGIS container.
     */
    public PostGisClient(String jdbcUrl, String user, String pass) {
        // Set up the required information for further interactions
        this.STACK_JDBC_URL = jdbcUrl;
        this.STACK_POSTGIS_USER = user;
        this.STACK_POSTGIS_PASSWORD = pass;
        // Retrieve all available custom database so that methods can perform federated query on each
        // Note that to retrieve all databases, this method must connect to any default database, which is postgres in this case
        try (Connection conn = connect(this.getJdbc("postgres"), user, pass)) {
            this.retrieveAllDatabaseNames(conn);
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to database: " + e);
            throw new JPSRuntimeException("Error connecting to database: " + e);
        }
    }

    /**
     * Get the list of database names that is available in this stack. This method is accessible for the stack client's usage.
     */
    public List<String> getDatabaseNames() {
        return this.DATABASE_LIST;
    }

    /**
     * Get the PostGIS username.
     */
    public String getUsername() {
        return this.STACK_POSTGIS_USER;
    }

    /**
     * Get the PostGIS password.
     */
    public String getPassword() {
        return this.STACK_POSTGIS_PASSWORD;
    }

    /**
     * Get the JDBC URL of a specified database within this stack.
     *
     * @param database The database of interest in the stack's RDB.
     * @return The JDBC URL of the database specified.
     */
    private String getJdbc(String database) {
        return this.STACK_JDBC_URL + database;
    }

    /**
     * Connect to the specified PostgreSQL database.
     *
     * @param jdbcUrl  The database JDBC url.
     * @param user     The database's username.
     * @param password The database's password.
     * @return the database Connection object.
     */
    private Connection connect(String jdbcUrl, String user, String password) throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            LOGGER.fatal(e);
            throw new JPSRuntimeException(e);
        }
        return DriverManager.getConnection(jdbcUrl, user, password);
    }

    /**
     * Retrieves all the database names from the default 'postgres' database, excluding the defaults.
     * The results will be stored in this client and can be retrieved using getDatabaseNames().
     */
    private void retrieveAllDatabaseNames(Connection conn) {
        try (Statement stmt = conn.createStatement()) {
            // Retrieve all available database names
            String listDatabaseQuery = "SELECT datname FROM pg_database\n" +
                    // Excluding default databases that does not store data in our workflow and are irrelevant
                    "WHERE datname NOT IN ('postgres', 'template_postgis', 'template1', 'template0');";
            ResultSet columnsResultSet = stmt.executeQuery(listDatabaseQuery);
            // When there is a next row available,
            while (columnsResultSet.next()) {
                // Append each row's value for their database name
                this.DATABASE_LIST.add(columnsResultSet.getString(1));
            }
        } catch (SQLException e) {
            LOGGER.fatal("Failed to retrieve all database names available! " + e);
            throw new JPSRuntimeException("Failed to retrieve all database names available! " + e);
        }
    }
}
