package uk.ac.cam.cares.jsp.integration;

import com.zaxxer.hikari.HikariDataSource;

import java.sql.Connection;
import java.sql.SQLException;

public class SqlConnectionPool {
    private static HikariDataSource db2D;
    private static HikariDataSource db3D;
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
        db2D = new HikariDataSource();
        db2D.setDriverClassName(JDBC_DRIVER);
        db2D.setMaximumPoolSize(MAX_POOL_SIZE);
        db2D.setJdbcUrl(config[4]);
        db2D.setUsername(config[1]);
        db2D.setPassword(config[2]);

        db3D = new HikariDataSource();
        db3D.setDriverClassName(JDBC_DRIVER);
        db3D.setMaximumPoolSize(MAX_POOL_SIZE);
        db3D.setJdbcUrl(config[3]);
        db3D.setUsername(config[1]);
        db3D.setPassword(config[2]);

        configs = config;
    }

    /**
     * Gets a source SQL database connection from the connection pool.
     *
     * @return The source database connection.
     */
    public Connection get2DConnection() throws SQLException {
        return db2D.getConnection();
    }

    /**
     * Gets a target SQL database connection from the connection pool.
     *
     * @return The target database connection.
     */
    public Connection get3DConnection() throws SQLException {
        return db3D.getConnection();
    }

    public String[] getConfigs() {
        return configs;
    }
}
