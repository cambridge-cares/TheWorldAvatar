package com.cmclinnovations.stack.clients.postgis;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class PostGISClient extends ClientWithEndpoint<PostGISEndpointConfig> {

    public static final String DEFAULT_DATABASE_NAME = "postgres";

    public static final String DEFAULT_SCHEMA_NAME = "public";

    private static final Logger logger = LoggerFactory.getLogger(PostGISClient.class);

    private static PostGISClient instance = null;

    public static PostGISClient getInstance() {
        if (null == instance) {
            instance = new PostGISClient();
        }
        return instance;
    }

    protected PostGISClient() {
        super(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
    }

    private Connection getDefaultConnection() throws SQLException {
        return getRemoteStoreClient().getConnection();
    }

    public void createDatabase(String database) {
        try (Connection conn = getDefaultConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE DATABASE \"" + database + "\" WITH TEMPLATE = template_postgis";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("42P04".equals(ex.getSQLState())) {
                // Database already exists error
            } else {
                throw new RuntimeException("Failed to create database '" + database
                        + "' on the server with JDBC URL '" + readEndpointConfig().getJdbcURL(DEFAULT_DATABASE_NAME)
                        + "'.", ex);
            }
        }
        createDefaultExtensions(database);
    }

    private void createDefaultExtensions(String database) {
        try (Connection conn = getRemoteStoreClient(database).getConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE EXTENSION IF NOT EXISTS postgis; "
                    + "CREATE EXTENSION IF NOT EXISTS postgis_topology; "
                    + "CREATE EXTENSION IF NOT EXISTS fuzzystrmatch; ";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            throw new RuntimeException("Failed to create extensions in database '" + database
                    + "' on the server with JDBC URL '" + readEndpointConfig().getJdbcURL("postgres") + "'.", ex);
        }
    }

    public void removeDatabase(String database) {
        try (Connection conn = getDefaultConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "DROP DATABASE \"" + database + "\"";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("3D000".equals(ex.getSQLState())) {
                // Database doesn't exist error
            } else {
                throw new RuntimeException("Failed to drop database '" + database
                        + "' on the server with JDBC URL '" + readEndpointConfig().getJdbcURL(DEFAULT_DATABASE_NAME)
                        + "'.", ex);
            }
        }
    }

    public void createSchema(String database, String schemaName) {
        RemoteRDBStoreClient remoteStoreClient = getRemoteStoreClient(database);
        try (Connection conn = remoteStoreClient.getConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE SCHEMA IF NOT EXISTS \"" + schemaName + "\"";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            throw new RuntimeException("Failed to create schema '" + schemaName
                    + "' in database with JDBC URL '"
                    + remoteStoreClient.getRdbURL() + "'.", ex);
        }
    }

    public void removeSchema(String database, String schemaName) {
        RemoteRDBStoreClient remoteStoreClient = getRemoteStoreClient(database);
        try (Connection conn = remoteStoreClient.getConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "DROP SCHEMA IF EXISTS " + schemaName;
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            throw new RuntimeException("Failed to drop schema '" + schemaName
                    + "' from database with JDBC URL '"
                    + remoteStoreClient.getRdbURL() + "'.", ex);
        }
    }

    public void executeUpdate(String database, String sql) {
        RemoteRDBStoreClient remoteStoreClient = getRemoteStoreClient(database);
        try (Connection conn = remoteStoreClient.getConnection();
                Statement stmt = conn.createStatement()) {
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("3D000".equals(ex.getSQLState())) {
                // Database doesn't exist error
            } else {
                throw new RuntimeException("Failed to run SQL update '" + sql + "' on the server with JDBC URL '"
                        + remoteStoreClient.getRdbURL() + "'.", ex);
            }
        }
    }

    public RemoteRDBStoreClient getRemoteStoreClient() {
        return getRemoteStoreClient(DEFAULT_DATABASE_NAME);
    }

    public RemoteRDBStoreClient getRemoteStoreClient(String database) {
        PostGISEndpointConfig endpoint = readEndpointConfig();
        return new RemoteRDBStoreClient(endpoint.getJdbcURL(database),
                endpoint.getUsername(),
                endpoint.getPassword());
    }

    public void resetSchema(String database) {
        try (InputStream is = PostGISClient.class.getResourceAsStream("postgis_reset_schema.sql")) {
            String sqlQuery = new String(is.readAllBytes()).replace("{database}", database);
            PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(sqlQuery);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read resource file 'postgis_reset_schema.sql'.", ex);
        }
    }

    public void addProjectionsToPostgis(String database, String proj4String,
            String wktString, String authName, String srid) {
        String execId;

        String postGISContainerId = getContainerId("postgis");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        execId = createComplexCommand(postGISContainerId,
                "psql", "-U", readEndpointConfig().getUsername(), "-d", database, "-w")
                .withHereDocument(
                        "INSERT INTO spatial_ref_sys (srid, auth_name, auth_srid, srtext, proj4text) VALUES ("
                                + srid + ",'"
                                + authName + "'," + srid + ",'" + wktString + "','" + proj4String + "');")
                .withErrorStream(errorStream)
                .withOutputStream(outputStream)
                .exec(); // will throw error if EPSG exists in table due to constraint
                         // "spatial_ref_system_pkey".
        handleErrors(errorStream, execId, logger);
    }
}
