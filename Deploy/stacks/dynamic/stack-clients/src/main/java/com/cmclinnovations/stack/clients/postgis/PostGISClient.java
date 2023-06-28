package com.cmclinnovations.stack.clients.postgis;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.Options;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.utils.FileUtils;
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
            String sql = "CREATE DATABASE " + databaseName;
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            if ("42P04".equals(ex.getSQLState())) {
                // Database already exists error
            } else {
                throw new RuntimeException("Failed to create database '" + databaseName
                        + "' on the server with JDBC URL '" + postgreSQLEndpoint.getJdbcURL("postgres") + "'.", ex);
            }
        }
        createDefaultExtensions(databaseName);
    }

    private void createDefaultExtensions(String databaseName) {
        try (Connection conn = getRemoteStoreClient(databaseName).getConnection();
                Statement stmt = conn.createStatement()) {
            String sql = "CREATE EXTENSION IF NOT EXISTS postgis; "
                    + "CREATE EXTENSION IF NOT EXISTS postgis_topology; "
                    + "CREATE EXTENSION IF NOT EXISTS fuzzystrmatch; ";
            stmt.executeUpdate(sql);
        } catch (SQLException ex) {
            throw new RuntimeException("Failed to create extensions in database '" + databaseName
                    + "' on the server with JDBC URL '" + postgreSQLEndpoint.getJdbcURL("postgres") + "'.", ex);
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

    public void uploadRoutingDataDirectoryToPostGIS(String database, String sourceDirectory, Options options,
            boolean append) {
        List<Path> allFilesList;
        try (Stream<Path> dirsStream = Files.walk(Path.of(sourceDirectory))) {
            allFilesList = dirsStream.filter(file -> !Files.isDirectory(file)).collect(Collectors.toList());
        } catch (IOException ex) {
            throw new RuntimeException("Failed to walk directory '" + sourceDirectory + "'.", ex);
        }
        List<Path> osmFilesList = allFilesList.stream().filter(file -> FileUtils.hasFileExtension(file, "osm"))
                .collect(Collectors.toList());
        if (osmFilesList.isEmpty()) {
            throw new RuntimeException("No osm file in routing data directory '" + sourceDirectory + "'.");
        }
        List<Path> configsList = allFilesList.stream().filter(file -> FileUtils.hasFileExtension(file, "xml"))
                .collect(Collectors.toList());
        if (configsList.size() > 1) {
            throw new RuntimeException(
                    "Too many xml config files (" + osmFilesList.size() + ") in routing data directory '"
                            + sourceDirectory + "'.");
        } else if (configsList.isEmpty()) {
            throw new RuntimeException(
                    "No xml config files found in routing data directory '" + sourceDirectory + "'.");
        }
        uploadRoutingFilesToPostGIS(database, configsList.get(0), osmFilesList, options, append);
    }

    public void uploadRoutingFilesToPostGIS(String database, Path configFilePath, List<Path> osmFilesList,
            Options options, boolean append) {
        try (TempDir tmpDir = makeLocalTempDir()) {
            tmpDir.copyFrom(configFilePath);
            osmFilesList.stream().forEach(tmpDir::copyFrom);
            osmFilesList.stream().findFirst().ifPresent(
                    osmFile -> uploadRoutingToPostGIS(database, tmpDir.getPath().resolve(osmFile.getFileName()),
                            tmpDir.getPath().resolve(configFilePath.getFileName()), options, append));
            osmFilesList.stream().skip(1).forEach(
                    osmFile -> uploadRoutingToPostGIS(database, tmpDir.getPath().resolve(osmFile.getFileName()),
                            tmpDir.getPath().resolve(configFilePath.getFileName()), options, true));
        }
    }

    public void uploadRoutingToPostGIS(String database, Path osmFilePath, Path configFilePath, Options options,
            boolean append) {
        String containerId = getContainerId("postgis");
        ensurePostGISRoutingSupportEnabled(database, containerId);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

        String execId = createComplexCommand(containerId,
                constructOSM2PGRoutingCommand(osmFilePath.toString(), configFilePath.toString(), database, options,
                        append))
                .withOutputStream(outputStream)
                .withErrorStream(errorStream)
                .withEvaluationTimeout(300)
                .exec();

        handleErrors(errorStream, execId, logger);
    }

    private String[] constructOSM2PGRoutingCommand(String osmFile, String configFile, String database, Options options,
            boolean append) {
        List<String> command = Arrays.asList("osm2pgrouting", "--f", osmFile, "--conf",
                configFile, "--dbname", database, "--username", postgreSQLEndpoint.getUsername(),
                "--password", postgreSQLEndpoint.getPassword());
        if (!append) {
            command.add("--clean");
        }
        command.addAll(options.getOptionsList());

        return command.toArray(String[]::new);
    }

    private void ensurePostGISRoutingSupportEnabled(String database, String postGISContainerId) {
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        String execId = createComplexCommand(postGISContainerId,
                "psql", "-U", postgreSQLEndpoint.getUsername(), "-d", database, "-w")
                .withHereDocument("CREATE EXTENSION IF NOT EXISTS pgrouting CASCADE;")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
    }
}
