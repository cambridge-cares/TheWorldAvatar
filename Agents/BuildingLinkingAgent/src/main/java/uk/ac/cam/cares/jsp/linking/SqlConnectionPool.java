package uk.ac.cam.cares.jsp.linking;

import java.sql.Connection;
import java.sql.SQLException;

import com.zaxxer.hikari.HikariDataSource;

public class SqlConnectionPool {
    private static HikariDataSource sourceDs;
    private static HikariDataSource targetDs;
    private static String[] configs;
    private static final String JDBC_DRIVER = "org.postgresql.Driver";
    private static final int MAX_POOL_SIZE = 1;

    /**
     * Standard Constructor creating a pool of connections to the SQL database.
     *
     * @param config An array containing the SQL credentials for both databases. It should contain six items in sequence of source JDBC url, user, password, and target JDBC url, user, password.
     */
    public SqlConnectionPool(String[] config) {
        // Set up source connection
        sourceDs = new HikariDataSource();
        sourceDs.setDriverClassName(JDBC_DRIVER);
        sourceDs.setMaximumPoolSize(MAX_POOL_SIZE);
        sourceDs.setJdbcUrl(config[0]);
        sourceDs.setUsername(config[1]);
        sourceDs.setPassword(config[2]);
        configs = config;
    }

    /**
     * Gets a source SQL database connection from the connection pool.
     *
     * @return The source database connection.
     */
    public Connection getSourceConnection() throws SQLException {
        return sourceDs.getConnection();
    }

    /**
     * Gets a target SQL database connection from the connection pool.
     *
     * @return The target database connection.
     */
    public Connection getTargetConnection() throws SQLException {
        return targetDs.getConnection();
    }

    public String[] getConfigs() {
        return configs;
    }
}
