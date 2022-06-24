package com.cmclinnovations.clients.postgis;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import com.cmclinnovations.clients.docker.ContainerClient;

public class PostGISClient extends ContainerClient {

    private final PostGISEndpointConfig postgreSQLEndpoint;

    public PostGISClient() {
        postgreSQLEndpoint = readEndpointConfig("postgis", PostGISEndpointConfig.class);
    }

    private Connection getDefaultConnection() throws SQLException {
        return getConnection("");
    }

    private Connection getConnection(String database) throws SQLException {
        return DriverManager.getConnection(
                postgreSQLEndpoint.getJdbcURL(database),
                postgreSQLEndpoint.getUsername(),
                postgreSQLEndpoint.getPassword());
    }

    public void createDatabase(String databaseName) {
        try (Connection conn = getDefaultConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE DATABASE " + databaseName + " WITH TEMPLATE = template_postgis";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("42P04".equals(ex.getSQLState())) {
                // Database already exists error
            } else {
                throw new RuntimeException("Failed to create database '" + databaseName
                        + "' on the server with JDBC URL '" + postgreSQLEndpoint.getJdbcURL("postgres") + "'.", ex);
            }
        }
    }

    public void removeDatabase(String databaseName) {
        try (Connection conn = getDefaultConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "DROP DATABASE " + databaseName;
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("3D000".equals(ex.getSQLState())) {
                // Database doesn't exist error
            } else {
                throw new RuntimeException("Failed to drop database '" + databaseName
                        + "' on the server with JDBC URL '" + postgreSQLEndpoint.getJdbcURL("postgres") + "'.", ex);
            }
        }
    }
}
