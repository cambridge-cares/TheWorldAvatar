package com.cmclinnovations.stack.clients.geoserver;

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
import java.util.NoSuchElementException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.JsonHelper;

import it.geosolutions.geoserver.rest.GeoServerRESTManager;
import it.geosolutions.geoserver.rest.Util;
import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.GSResourceEncoder;
import it.geosolutions.geoserver.rest.encoder.GSResourceEncoder.ProjectionPolicy;
import it.geosolutions.geoserver.rest.encoder.coverage.GSImageMosaicEncoder;
import it.geosolutions.geoserver.rest.encoder.datastore.GSPostGISDatastoreEncoder;
import it.geosolutions.geoserver.rest.encoder.feature.GSFeatureTypeEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class GeoServerClient extends ClientWithEndpoint<RESTEndpointConfig> {

    private static final Logger logger = LoggerFactory.getLogger(GeoServerClient.class);
    private final GeoServerRESTManager manager;

    private final PostGISEndpointConfig postgreSQLEndpoint;

    private static GeoServerClient instance = null;

    public static final Path SERVING_DIRECTORY = Path.of("/opt/geoserver_data/www");
    private static final Path STATIC_DATA_DIRECTORY = SERVING_DIRECTORY.resolve("static_data");
    private static final Path ICONS_DIRECTORY = SERVING_DIRECTORY.resolve("icons");
    private static final String GEOSERVER_RASTER_INDEX_DATABASE_SUFFIX = "_geoserver_indices";
    private static final String DIM_PREFIX = "dim_";

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
        super(EndpointNames.GEOSERVER, RESTEndpointConfig.class);
        if (null == restURL || null == username || null == password) {
            RESTEndpointConfig geoserverEndpointConfig = readEndpointConfig();
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

    public void removeWorkspace(String workspaceName) {
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
                    Path.of("/inputs/config").resolve(style.getFile()).toFile(), name)) {
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

    private void loadStaticFile(Path baseDirectory, GeoserverOtherStaticFile file) throws NoSuchElementException {
        Path absSourcePath = baseDirectory.resolve(file.getSource());
        Path absTargetPath = STATIC_DATA_DIRECTORY.resolve(file.getTarget());

        String containerId = getContainerId(EndpointNames.GEOSERVER);

        if (!Files.exists(absSourcePath)) {
            throw new RuntimeException(
                    "Static GeoServer data '" + absSourcePath.toString() + "' does not exist and could not be loaded.");
        } else if (Files.isDirectory(absSourcePath)) {
            sendFolder(containerId, absSourcePath.toString(), absTargetPath.toString());
        } else {
            try {
                sendFilesContent(containerId,
                        Map.of(file.getTarget(),
                                Files.readAllBytes(baseDirectory.resolve(file.getSource()))),
                        STATIC_DATA_DIRECTORY.toString());
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to serialise file '" + absSourcePath.toString() + "'.");
            }

        }
    }

    public void loadIcons(Path baseDirectory, String iconDir) throws NoSuchElementException {
        if (!Files.exists(baseDirectory.resolve(iconDir))) {
            throw new RuntimeException(
                    "Static GeoServer data '" + baseDirectory.resolve(iconDir)
                            + "' does not exist and could not be loaded.");
        } else if (Files.isDirectory(baseDirectory.resolve(iconDir))) {
            sendFolder(getContainerId(EndpointNames.GEOSERVER), baseDirectory.resolve(iconDir).toString(),
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

    public void createPostGISLayer(String workspaceName, String database, String schema, String layerName,
            GeoServerVectorSettings geoServerSettings) {
        String storeName = database;

        // Need to include the "Util.DEFAULT_QUIET_ON_NOT_FOUND" argument because the
        // 2-arg version of "existsLayer" incorrectly calls the 3-arg version of the
        // "existsLayerGroup" method.
        if (manager.getReader().existsLayer(workspaceName, layerName, Util.DEFAULT_QUIET_ON_NOT_FOUND)) {
            logger.info("GeoServer database layer '{}' already exists.", layerName);
        } else {
            createPostGISDataStore(workspaceName, storeName, database, schema);

            GSFeatureTypeEncoder fte = geoServerSettings.getFeatureTypeSettings();
            if (fte.getName() == null) {
                fte.setName(layerName);
            }
            if (!GeoServerElementUtils.nodeIsSet(fte, "projectionPolicy")) {
                fte.setProjectionPolicy(ProjectionPolicy.NONE);
            }
            if (!GeoServerElementUtils.nodeIsSet(fte, "title")) {
                fte.setTitle(layerName);
            }

            GSVirtualTableEncoder virtualTable = geoServerSettings.getVirtualTable();
            configureVirtualTable(layerName, fte, virtualTable);

            processDimensions(geoServerSettings, fte);

            if (manager.getPublisher().publishDBLayer(workspaceName, storeName, fte,
                    geoServerSettings.getLayerSettings())) {
                logger.info("GeoServer database layer '{}' created.", layerName);
            } else {
                throw new RuntimeException(
                        "GeoServer database layer '" + layerName + "' does not exist and could not be created.");
            }
        }
    }

    private void configureVirtualTable(String layerName, GSFeatureTypeEncoder fte, GSVirtualTableEncoder virtualTable) {
        if (null != virtualTable) {
            // Append the layer name to the table name
            virtualTable.setName(layerName + "_" + virtualTable.getName());
            // Handle sql stored in a separate file
            virtualTable.setSql(JsonHelper.handleFileValues(virtualTable.getSql()));
            fte.setNativeName(virtualTable.getName());
            fte.setMetadataVirtualTable(virtualTable);
        }
    }

    public void createGeoTiffLayer(String workspaceName, String name, String database, String schema,
            GeoServerRasterSettings geoServerSettings, MultidimSettings mdimSettings) throws NoSuchElementException {

        if (manager.getReader().existsCoveragestore(workspaceName, name)) {
            logger.info("GeoServer coverage store '{}' already exists.", name);
        } else {
            String geoserverRasterIndexDatabaseName = database + GEOSERVER_RASTER_INDEX_DATABASE_SUFFIX;
            PostGISClient postgisClient = PostGISClient.getInstance();
            postgisClient.createDatabase(geoserverRasterIndexDatabaseName);
            postgisClient.createSchema(geoserverRasterIndexDatabaseName, schema);

            String containerId = getContainerId(EndpointNames.GEOSERVER);

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

                        if (null != format) {
                            indexerProperties = "TimeAttribute=time\nSchema=location:String,time:java.util.Date,*the_geom:Polygon\nPropertyCollectors=TimestampFileNameExtractorSPI[timeregex](time)";
                            files.put("timeregex.properties", ("regex=" + regex + ",format=" + format).getBytes());
                        } else {
                            indexerProperties = "AdditionalDomainAttributes=time_index(time_index)\nSchema=location:String,time_index:String,*the_geom:Polygon\nPropertyCollectors=StringFileNameExtractorSPI[timeregex](time_index)";
                            files.put("timeregex.properties", ("regex=" + regex).getBytes());
                        }
                    }
                }

                files.put("indexer.properties",
                        indexerProperties.getBytes());

                sendFilesContent(containerId, files, geotiffDir.toString());
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

                if (manager.getPublisher().publishExternalMosaic(workspaceName, name, geotiffDir.toFile(),
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
            dimensions.forEach((dimName, value) -> {
                if (!dimName.startsWith(DIM_PREFIX) && !dimName.equals("time")
                        && !dimName.equals("elevation")) {
                    throw new RuntimeException(
                            "When using a GeoServer custom dimension (i.e. not `time` or `elevation) the name `"
                                    + dimName + "` must begin with the prefix `dim_`.");
                }
                resourceEncoder.setMetadataDimension(dimName, value);
            });
        }
    }

    public void addProjectionsToGeoserver(String wktString, String srid) throws NoSuchElementException {

        String geoserverContainerId = getContainerId("geoserver");
        DockerClient dockerClient = DockerClient.getInstance();

        dockerClient.makeDir(geoserverContainerId, "/opt/geoserver_data/user_projections");

        dockerClient.sendFilesContent(geoserverContainerId,
                Map.of("epsg.properties",
                        (srid + "=" + wktString + "\n").getBytes()),
                "/opt/geoserver_data/user_projections/");

        GeoServerClient.getInstance().reload();
    }
}
