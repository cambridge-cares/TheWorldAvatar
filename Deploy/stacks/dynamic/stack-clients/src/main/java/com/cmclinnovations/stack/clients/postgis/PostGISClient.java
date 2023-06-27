package com.cmclinnovations.stack.clients.postgis;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.utils.TempDir;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class PostGISClient extends ContainerClient implements ClientWithEndpoint {

    public static final String DEFAULT_SCHEMA_NAME = "public";

    private static final Logger logger = LoggerFactory.getLogger(PostGISClient.class);

    private final PostGISEndpointConfig postgreSQLEndpoint;

    private static PostGISClient instance = null;

    public static PostGISClient getInstance() {
        if (null == instance) {
            instance = new PostGISClient();
        }
        return instance;
    }

    private PostGISClient() {
        postgreSQLEndpoint = readEndpointConfig(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
    }

    @Override
    public PostGISEndpointConfig getEndpoint() {
        return postgreSQLEndpoint;
    }

    private Connection getDefaultConnection() throws SQLException {
        return getRemoteStoreClient().getConnection();
    }

    public void createDatabase(String databaseName) {
        try (Connection conn = getDefaultConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE EXTENSION IF NOT EXISTS postgis; CREATE DATABASE " + databaseName
                    + " WITH TEMPLATE = template_postgis";
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

    public RemoteRDBStoreClient getRemoteStoreClient() {
        return getRemoteStoreClient("");
    }

    public RemoteRDBStoreClient getRemoteStoreClient(String database) {
        return new RemoteRDBStoreClient(postgreSQLEndpoint.getJdbcURL(database),
                postgreSQLEndpoint.getUsername(),
                postgreSQLEndpoint.getPassword());
    }

    public void uploadRoutingFilesToPostGIS(String database, String sourceDirectory) {
        Stream<Path> allFilesStream;
        try (Stream<Path> dirsStream = Files.walk(Path.of(sourceDirectory))) {
            allFilesStream = dirsStream.filter(file -> !Files.isDirectory(file));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to walk directory '" + sourceDirectory + ".'", ex);
        }
        Stream<Path> osmFilesStream = allFilesStream.filter(file -> hasFileExtension(file, "osm"));
        if (osmFilesStream.count() < 1) {
            throw new RuntimeException("No osm file in routing data directory '" + sourceDirectory + "'.");
        }
        Stream<Path> configsStream = allFilesStream.filter(file -> hasFileExtension(file, "xml"));
        if (configsStream.count() > 1) {
            throw new RuntimeException(
                    "Too many xml config files (" + configsStream.count() + ") in routing data directory '"
                            + sourceDirectory
                            + "'.");
        }
        Path configPath = configsStream.findFirst()
                .orElseThrow(() -> new RuntimeException(
                        "No xml config files found in routing data directory '" + sourceDirectory + "'."));

        osmFilesStream
                .forEach(osmPath -> uploadRoutingFileToPostGIS(database, osmPath, configPath));
    }

    private boolean hasFileExtension(Path file, String extension) {
        int i = file.toString().lastIndexOf(".");
        if (i > 0) {
            return file.toString().substring(i + 1).equals(extension);
        } else {
            return false;
        }
    }

    public void uploadRoutingFileToPostGIS(String database, Path osmFilePath, Path configFilePath) {
        // TODO: only create one temp for config file
        try (TempDir tmpDir = makeLocalTempDir()) {
            tmpDir.copyFrom(osmFilePath);
            tmpDir.copyFrom(configFilePath);
            uploadRoutingToPostGIS(database, tmpDir.getPath().resolve(osmFilePath.getFileName()),
                    tmpDir.getPath().resolve(configFilePath.getFileName()));
        }
    }

    public void uploadRoutingToPostGIS(String database, Path osmFilePath, Path configFilePath) {
        String containerId = getContainerId("postgis");
        ensurePostGISRoutingSupportEnabled(database, containerId);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId, "osm2pgrouting", "--f", osmFilePath.toString(), "--conf",
                configFilePath.toString(), "--dbname", database, "--username", postgreSQLEndpoint.getUsername(),
                "--password", postgreSQLEndpoint.getPassword(), "--clean") // TODO: add more options to this command
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(300)
                .exec();

        handleErrors(errorStream, execId, logger);
    }

    private void ensurePostGISRoutingSupportEnabled(String database, String postGISContainerId) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS pg_routing;")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
    }
}
