package com.cmclinnovations.stack.clients.geoserver;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import it.geosolutions.geoserver.rest.GeoServerRESTManager;
import it.geosolutions.geoserver.rest.Util;
import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.GSResourceEncoder;
import it.geosolutions.geoserver.rest.encoder.GSResourceEncoder.ProjectionPolicy;
import it.geosolutions.geoserver.rest.encoder.coverage.GSImageMosaicEncoder;
import it.geosolutions.geoserver.rest.encoder.datastore.GSPostGISDatastoreEncoder;
import it.geosolutions.geoserver.rest.encoder.feature.GSFeatureTypeEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class GeoServerClient extends ContainerClient {

    private static final Logger logger = LoggerFactory.getLogger(GeoServerClient.class);
    private final GeoServerRESTManager manager;

    private final PostGISEndpointConfig postgreSQLEndpoint;

    private static GeoServerClient instance = null;

    public static final Path SERVING_DIRECTORY = Path.of("/opt/geoserver_data/www");
    private static final Path STATIC_DATA_DIRECTORY = SERVING_DIRECTORY.resolve("static_data");
    private static final Path ICONS_DIRECTORY = SERVING_DIRECTORY.resolve("icons");
    private static final String GEOSERVER_RASTER_INDEX_DATABASE_SUFFIX = "_geoserver_indices";

    public static GeoServerClient getInstance() {
        if (null == instance) {
            instance = new GeoServerClient();
        }
        return instance;
    }

    private GeoServerClient() {
        this(null, null, null);
    }

    public GeoServerClient(URL restURL, String username, String password) {
        if (null == restURL || null == username || null == password) {
            RESTEndpointConfig geoserverEndpointConfig = readEndpointConfig("geoserver", RESTEndpointConfig.class);
            if (null == restURL) {
                restURL = geoserverEndpointConfig.getUrl();
            }
            if (null == username) {
                username = geoserverEndpointConfig.getUserName();
            }
            if (null == password) {
                password = geoserverEndpointConfig.getPassword();
            }
        }

        manager = new GeoServerRESTManager(restURL, username, password);

        postgreSQLEndpoint = readEndpointConfig(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
    }

    public void createWorkspace(String workspaceName) {
        if (manager.getReader().existsWorkspace(workspaceName)) {
            logger.info("GeoServer workspace '{}' already exists.", workspaceName);
        } else {
            if (manager.getPublisher().createWorkspace(workspaceName)) {
                logger.info("GeoServer workspace '{}' created.", workspaceName);
            } else {
                throw new RuntimeException(
                        "GeoServer workspace '" + workspaceName + "' does not exist and could not be created.");
            }
        }
    }

    public void deleteWorkspace(String workspaceName) {
        if (!manager.getReader().existsWorkspace(workspaceName)) {
            logger.info("GeoServer workspace '{}' does not exists and cannot be deleted.", workspaceName);
        } else {
            if (manager.getPublisher().removeWorkspace(workspaceName, true)) {
                logger.info("GeoServer workspace '{}'' removed", workspaceName);
            } else {
                throw new RuntimeException("GeoServer workspace " + workspaceName + "' could not be deleted.");
            }
        }
    }

    public void reload() {
        manager.getPublisher().reload();
    }

    public void loadStyle(GeoServerStyle style, String workspaceName) {
        String name = style.getName();
        if (manager.getReader().existsStyle(workspaceName, name)) {
            logger.info("GeoServer style '{}:{}' already exists.", workspaceName, name);
        } else {
            if (manager.getPublisher().publishStyleInWorkspace(workspaceName,
                    new File("/inputs/config", style.getFile()), name)) {
                logger.info("GeoServer style '{}:{}' created.", workspaceName, name);
            } else {
                throw new RuntimeException("GeoServer style '" + workspaceName + ":" + name
                        + "' does not exist and could not be created.");
            }
        }
    }

    public void loadOtherFiles(Path baseDirectory, List<GeoserverOtherStaticFile> files) {
        files.forEach(file -> {
            loadStaticFile(baseDirectory, file);
        });

    }

    private void loadStaticFile(Path baseDirectory, GeoserverOtherStaticFile file) {
        Path filePath = baseDirectory.resolve(file.getSource());
        Path sourceParentDir = filePath.getParent();
        Path fileName = filePath.getFileName();
        Path absTargetDir = STATIC_DATA_DIRECTORY.resolve(file.getTarget());

        String containerId = getContainerId("geoserver");

        if (!Files.exists(filePath)) {
            throw new RuntimeException(
                    "Static GeoServer data '" + filePath.toString() + "' does not exist and could not be loaded.");
        } else if (Files.isDirectory(filePath)) {
            sendFolder(containerId, filePath.toString(), absTargetDir.resolve(fileName).toString());
        } else {
            sendFiles(containerId, sourceParentDir.toString(), List.of(fileName.toString()), absTargetDir.toString());
        }
    }

    public void loadIcons(Path baseDirectory, String iconDir) {
        if (!Files.exists(baseDirectory.resolve(iconDir))) {
            throw new RuntimeException(
                    "Static GeoServer data '" + baseDirectory.resolve(iconDir)
                            + "' does not exist and could not be loaded.");
        } else if (Files.isDirectory(baseDirectory.resolve(iconDir))) {
            sendFolder(getContainerId("geoserver"), baseDirectory.resolve(iconDir).toString(),
                    ICONS_DIRECTORY.toString());
        } else {
            throw new RuntimeException("Geoserver icon directory " + iconDir + "does not exist or is not a directory.");
        }
    }

    public void createPostGISDataStore(String workspaceName, String name, String database, String schema) {
        if (manager.getReader().existsDatastore(workspaceName, name)) {
            logger.info("GeoServer datastore '{}' already exists.", name);
        } else {

            GSPostGISDatastoreEncoder encoder = new GSPostGISDatastoreEncoder(name);
            encoder.setHost(postgreSQLEndpoint.getHostName());
            encoder.setPort(Integer.parseInt(postgreSQLEndpoint.getPort()));
            encoder.setUser(postgreSQLEndpoint.getUsername());
            encoder.setPassword(postgreSQLEndpoint.getPassword());
            encoder.setDatabase(database);
            encoder.setSchema(schema);
            encoder.setValidateConnections(true);

            if (manager.getStoreManager().create(workspaceName, encoder)) {
                logger.info("GeoServer datastore '{}' created.", name);
            } else {
                throw new RuntimeException(
                        "GeoServer datastore '" + name + "' does not exist and could not be created.");
            }
        }
    }

    public void createPostGISLayer(String workspaceName, String database, String layerName,
            GeoServerVectorSettings geoServerSettings) {
        String storeName = database;

        createPostGISDataStore(workspaceName, storeName, database, PostGISClient.DEFAULT_SCHEMA_NAME);

        // Need to include the "Util.DEFAULT_QUIET_ON_NOT_FOUND" argument because the
        // 2-arg version of "existsLayer" incorrectly calls the 3-arg version of the
        // "existsLayerGroup" method.
        if (manager.getReader().existsLayer(workspaceName, layerName, Util.DEFAULT_QUIET_ON_NOT_FOUND)) {
            logger.info("GeoServer database layer '{}' already exists.", database);
        } else {
            GSFeatureTypeEncoder fte = new GSFeatureTypeEncoder();
            fte.setProjectionPolicy(ProjectionPolicy.NONE);
            fte.addKeyword("KEYWORD");
            fte.setTitle(layerName);
            fte.setName(layerName);

            GSVirtualTableEncoder virtualTable = geoServerSettings.getVirtualTable();
            if (null != virtualTable) {
                virtualTable.setName(layerName + "_" + virtualTable.getName());
                fte.setNativeName(virtualTable.getName());
                fte.setMetadataVirtualTable(virtualTable);
            }

            processDimensions(geoServerSettings, fte);

            if (manager.getPublisher().publishDBLayer(workspaceName,
                    storeName,
                    fte, geoServerSettings)) {
                logger.info("GeoServer database layer '{}' created.", layerName);
            } else {
                throw new RuntimeException(
                        "GeoServer database layer '" + layerName + "' does not exist and could not be created.");
            }
        }
    }

    public void createGeoTiffLayer(String workspaceName, String name, String database, String schema,
            GeoServerRasterSettings geoServerSettings, MultidimSettings mdimSettings) {

        if (manager.getReader().existsCoveragestore(workspaceName, name)) {
            logger.info("GeoServer coverage store '{}' already exists.", name);
        } else {
            String geoserverRasterIndexDatabaseName = database + GEOSERVER_RASTER_INDEX_DATABASE_SUFFIX;
            PostGISClient.getInstance().createDatabase(geoserverRasterIndexDatabaseName);

            String containerId = getContainerId("geoserver");

            Properties datastoreProperties = new Properties();
            datastoreProperties.putIfAbsent("SPI", "org.geotools.data.postgis.PostgisNGDataStoreFactory");
            datastoreProperties.putIfAbsent("host", postgreSQLEndpoint.getHostName());
            datastoreProperties.putIfAbsent("port", postgreSQLEndpoint.getPort());
            datastoreProperties.putIfAbsent("database", geoserverRasterIndexDatabaseName);
            datastoreProperties.putIfAbsent("schema", schema);
            datastoreProperties.putIfAbsent("user", postgreSQLEndpoint.getUsername());
            datastoreProperties.putIfAbsent("passwd", postgreSQLEndpoint.getPassword());
            datastoreProperties.putIfAbsent("Loose\\ bbox", "true");
            datastoreProperties.putIfAbsent("Estimated\\ extends", "false");
            datastoreProperties.putIfAbsent("validate\\ connections", "true");
            datastoreProperties.putIfAbsent("Connection\\ timeout", "10");
            datastoreProperties.putIfAbsent("preparedStatements", "true");
            StringWriter stringWriter = new StringWriter();

            Path geotiffDir = Path.of(StackClient.GEOTIFFS_DIR, database, schema, name);
            try {

                Map<String, byte[]> files = new HashMap<>();

                datastoreProperties.store(stringWriter, "");
                files.put("datastore.properties", stringWriter.toString().getBytes());
                String indexerProperties = "Schema=location:String,*the_geom:Polygon\nPropertyCollectors=";

                if (null != mdimSettings) {
                    TimeOptions timeOptions = mdimSettings.getTimeOptions();
                    if (null != timeOptions) {
                        String regex = timeOptions.getRegex();
                        String format = timeOptions.getFormat();

                        indexerProperties = "TimeAttribute=time\nSchema=location:String,time:java.util.Date,*the_geom:Polygon\nPropertyCollectors=TimestampFileNameExtractorSPI[timeregex](time)";
                        files.put("timeregex.properties", ("regex=" + regex + ",format=" + format).getBytes());

                    }
                }

                files.put("indexer.properties",
                        indexerProperties.getBytes());

                sendFilesContent(containerId, files,
                        geotiffDir.toString());
            } catch (IOException ex) {
                throw new RuntimeException(
                        "The 'datastore.properties' and 'indexer.properties' files for the GeoServer coverage datastore '"
                                + name + "' could not be created.",
                        ex);
            }
            try {
                GSImageMosaicEncoder storeEncoder = geoServerSettings.getDataStoreSettings();
                if (null == storeEncoder.getName()) {
                    storeEncoder.setName(name);
                }
                storeEncoder.setAllowMultithreading(true);

                GSLayerEncoder layerEncoder = geoServerSettings.getLayerSettings();
                if (null == layerEncoder) {
                    // TODO Work out how to set the layer title/display name
                }

                processDimensions(geoServerSettings, storeEncoder);

                if (manager.getPublisher().publishExternalMosaic(
                        workspaceName, name,
                        geotiffDir.toFile(),
                        storeEncoder, layerEncoder)) {
                    logger.info("GeoServer coverage (datastore and layer) '{}' created.", name);
                } else {
                    throw new RuntimeException(
                            "GeoServer coverage datastore and/or layer '" + name
                                    + "' does not exist and could not be created.");
                }
            } catch (FileNotFoundException ex) {
                throw new RuntimeException(
                        "GeoServer coverage datastore '" + name + "' does not exist and could not be created.", ex);
            }
        }
    }

    private void processDimensions(GeoServerDimensionSettings dimensionSettings, GSResourceEncoder resourceEncoder) {
        Map<String, UpdatedGSFeatureDimensionInfoEncoder> dimensions = dimensionSettings.getDimensions();
         if (null != dimensions) {
        dimensions.entrySet().forEach(entry -> resourceEncoder.setMetadataDimension(entry.getKey(), entry.getValue()));
         }
    }

    public void addProjectionsToGeoserver(String geoserverContainerID, String wktString, String srid) {
        String execId;
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        execId = createComplexCommand(geoserverContainerID, "mkdir", "-p",
                "/opt/geoserver_data/user_projections")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);
        outputStream.reset();
        errorStream.reset();

        execId = createComplexCommand(geoserverContainerID, "bash", "-c",
                "echo > /opt/geoserver_data/user_projections/epsg.properties <<EOF " + srid + "=" + wktString
                        + "\nEOF")
                .withErrorStream(errorStream)
                .exec();
        handleErrors(errorStream, execId, logger);

        GeoServerClient.getInstance().reload();
        handleErrors(errorStream, execId, logger);
    }
}
