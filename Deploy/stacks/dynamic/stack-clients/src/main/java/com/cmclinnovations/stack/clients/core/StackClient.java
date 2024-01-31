package com.cmclinnovations.stack.clients.core;

import java.nio.file.Path;
import java.util.Map;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public final class StackClient {

    public static final String EXECUTABLE_KEY = "EXECUTABLE";
    public static final String STACK_NAME_KEY = "STACK_NAME";
    private static final String STACK_BASE_DIR_KEY = "STACK_BASE_DIR";
    public static final String STACK_NAME_LABEL = "com.docker.stack.namespace";
    public static final String PROJECT_NAME_LABEL = "com.docker.compose.project";
    public static final String SCRATCH_DIR = "/stack_scratch";
    public static final String GEOTIFFS_DIR = "/geotiffs";
    public static final String MULTIDIM_GEOSPATIAL_DIR = "/multidim_geospatial";
    public static final Path STACK_CONFIG_DIR = Path.of("/inputs/config");

    private static final String stackName;

    private static final Map<String, String> stackNameLabelMap;

    private static boolean inStack = true;

    static {
        String envVarStackName = System.getenv(StackClient.STACK_NAME_KEY);
        stackName = (null != envVarStackName) ? envVarStackName : "Test_Stack";

        stackNameLabelMap = Map.of(STACK_NAME_LABEL, stackName, PROJECT_NAME_LABEL, stackName);
    }

    private StackClient() {
    }

    public static String getStackName() {
        return stackName;
    }

    public static String getStackNameForRegex() {
        return stackName.replace("_", "(?:-|_)");
    }

    public static String prependStackName(String name) {
        return stackName + "_" + name;
    }

    public static String removeStackName(String name) {
        return name.replaceFirst("^/?" + StackClient.getStackNameForRegex() + "(?:-|_)", "");
    }

    public static Map<String, String> getStackNameLabelMap() {
        return stackNameLabelMap;
    }

    public static boolean isInStack() {
        return inStack;
    }

    public static void setInStack(boolean inStack) {
        StackClient.inStack = inStack;
    }

    public static String getContainerEngineName() {
        return System.getenv().getOrDefault(EXECUTABLE_KEY, "docker");
    }

    private static Path getStackBaseDir() {
        return Path.of(System.getenv(STACK_BASE_DIR_KEY));
    }

    public static Path getAbsDataPath() {
        return getStackBaseDir().resolve("inputs").resolve("data");
    }

    /**
     * Get a RemoteRDBStoreClient for the named Postgres RDB running in this stack.
     *
     * @param database the name of the Postgres database, if the database doesn't
     *                 already exist it will be created
     * @return a RemoteRDBStoreClient attached to the named Postgres RDB running in
     *         this stack
     */
    public static RemoteRDBStoreClient getRemoteRDBStoreClient(String database) {
        PostGISClient postgisClient = PostGISClient.getInstance();
        postgisClient.createDatabase(database);
        return postgisClient.getRemoteStoreClient(database);
    }

    /**
     * Get a RemoteRDBStoreClient for the default Postgres RDB running in this
     * stack.
     *
     * @return a RemoteRDBStoreClient attached to the default Postgres RDB running
     *         in this stack
     */
    public static RemoteRDBStoreClient getRemoteRDBStoreClient() {
        return PostGISClient.getInstance().getRemoteStoreClient();
    }

    /**
     * Get a RemoteStoreClient for the Blazegraph triplestore running in this stack.
     * 
     * @param namespace the name of the Blazegraph namespace, if the namespace
     *                  doesn't already exist it will be created with the default
     *                  properties
     * @param database  the name of the Postgres database, if the database doesn't
     *                  already exist it will be created
     * @param timeClass see description from this
     *                  {@link TimeSeriesClient#TimeSeriesClient(TripleStoreClientInterface, Class, String, String, String)
     *                  TimeSeriesClient} constructor
     * @return a RemoteStoreClient attached to the Blazegraph triplestore running in
     *         this stack
     */
    public static <T> TimeSeriesClient<T> getTimeSeriesClient(String namespace, String database, Class<T> timeClass) {
        BlazegraphClient blazegraphClient = BlazegraphClient.getInstance();
        blazegraphClient.createNamespace(namespace);
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);

        PostGISClient postgisClient = PostGISClient.getInstance();
        postgisClient.createDatabase(database);
        PostGISEndpointConfig postgisConfig = postgisClient.getEndpoint();

        return new TimeSeriesClient<>(remoteStoreClient, timeClass,
                postgisConfig.getJdbcURL(database), postgisConfig.getUsername(), postgisConfig.getPassword());
    }

}
