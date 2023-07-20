package uk.ac.cam.cares.jps.agent.cea;

import com.cmclinnovations.stack.clients.core.StackClient;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.data.CEAInputData;
import uk.ac.cam.cares.jps.agent.cea.utils.*;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.*;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.EndpointConfig;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.RouteHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.input.*;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.BuildingHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.tasks.*;

import org.json.JSONArray;

import org.locationtech.jts.geom.Coordinate;

import org.json.JSONObject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.net.*;
import java.time.*;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import java.util.concurrent.*;
import java.util.stream.Stream;


@WebServlet(
        urlPatterns = {
                CEAAgent.URI_ACTION,
                CEAAgent.URI_UPDATE,
                CEAAgent.URI_QUERY
        })
public class CEAAgent extends JPSAgent {
    public static final String KEY_REQ_METHOD = "method";
    public static final String URI_ACTION = "/run";
    public static final String URI_UPDATE = "/update";
    public static final String URI_QUERY = "/query";
    public static final String KEY_REQ_URL = "requestUrl";
    public static final String KEY_TARGET_URL = "targetUrl";
    public static final String KEY_IRI = "iris";
    public static final String KEY_GEOMETRY = "geometryEndpoint";
    public static final String KEY_USAGE = "usageEndpoint";
    public static final String KEY_WEATHER = "weatherEndpoint";
    public static final String KEY_TERRAIN_DB = "terrainDatabase";
    public static final String KEY_TERRAIN_TABLE = "terrainTable";
    public static final String KEY_CEA = "ceaEndpoint";
    public static final String KEY_GRAPH = "graphName";

    private String targetUrl = "http://localhost:8084/cea-agent" + URI_UPDATE;

    public static final String KEY_TIMES = "times";
    public static final String CEA_OUTPUTS = "ceaOutputs";
    public final int NUM_CEA_THREADS = 1;
    private final ThreadPoolExecutor CEAExecutor = (ThreadPoolExecutor) Executors.newFixedThreadPool(NUM_CEA_THREADS);
    
    private OntologyURIHelper ontologyUriHelper;
    private GeometryQueryHelper geometryQueryHelper;

    public static final String STACK_NAME = "<STACK NAME>";
    private String stackName;
    private EndpointConfig endpointConfig = new EndpointConfig();
    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;
    private String stackAccessAgentBase;
    private String defaultCeaLabel;
    private String defaultUsageLabel;
    private String dbUser;
    private String dbPassword;
    private String tsDb;
    private String tsUrl;
    private String defaultTerrainDb;
    private String defaultTerrainTable;
    private String requestUrl;
    private String geometryRoute;
    private String usageRoute;
    private String weatherRoute;
    private String defaultWeatherLabel;
    private String ceaRoute;
    private String namedGraph;
    private String openmeteoagentUrl;

    private Map<String, String> accessAgentRoutes = new HashMap<>();

    public CEAAgent() {
        readConfig();
        ontologyUriHelper = new OntologyURIHelper("CEAAgentConfig");
        geometryQueryHelper = new GeometryQueryHelper(ontologyUriHelper);
        stackName = StackClient.getStackName();
        stackAccessAgentBase =  stackAccessAgentBase.replace(STACK_NAME, stackName);
        openmeteoagentUrl = openmeteoagentUrl.replace(STACK_NAME, stackName);
        tsUrl = endpointConfig.getDbUrl(tsDb);
        dbUser = endpointConfig.getDbUser();
        dbPassword = endpointConfig.getDbPassword();
        rdbStoreClient = new RemoteRDBStoreClient(tsUrl, dbUser, dbPassword);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            requestUrl = requestParams.getString(KEY_REQ_URL);
            String uriArrayString = requestParams.get(KEY_IRI).toString();
            JSONArray uriArray = new JSONArray(uriArrayString);

            if (requestUrl.contains(URI_UPDATE) || requestUrl.contains(URI_ACTION)) {

                if (requestUrl.contains(URI_UPDATE)) {
                    DataManager dataManager = new DataManager(ontologyUriHelper);

                    // parse times
                    List<OffsetDateTime> times = DataParser.getTimesList(requestParams, KEY_TIMES);
                    TimeSeriesHelper tsHelper = new TimeSeriesHelper(storeClient, rdbStoreClient);

                    // parse times series data;
                    List<List<List<?>>> timeSeries = new ArrayList<>();
                    for (int i = 0; i < uriArray.length(); i++) {
                        List<List<?>> iriList = new ArrayList<>();
                        for(String ts: CEAConstants.TIME_SERIES) {
                            iriList.add(DataParser.getTimeSeriesList(requestParams, ts, i));
                        }
                        timeSeries.add(iriList);
                    }

                    LinkedHashMap<String, List<String>> scalars = new LinkedHashMap<>();

                    // parse PV area data
                    for(String scalar: CEAConstants.SCALARS){
                        scalars.put(scalar, DataParser.getList(requestParams, scalar));
                    }

                    for (int i = 0; i < uriArray.length(); i++) {
                        LinkedHashMap<String,String> tsIris = new LinkedHashMap<>();
                        LinkedHashMap<String,String> scalarIris = new LinkedHashMap<>();

                        String uri = uriArray.getString(i);

                        String building = dataManager.checkBuildingInitialised(uri, ceaRoute);
                        if(building.equals("")){
                            // Check if bot:Building IRI has already been created in another endpoint
                            building = dataManager.checkBuildingInitialised(uri, geometryRoute);
                            building = dataManager.initialiseBuilding(uri, building, ceaRoute, namedGraph);
                        }
                        if(!dataManager.checkDataInitialised(building, tsIris, scalarIris, ceaRoute)) {
                            tsHelper.createTimeSeries(tsIris, namedGraph, ontologyUriHelper);
                            dataManager.initialiseData(i, scalars, building, tsIris, scalarIris, ceaRoute, namedGraph);
                        }
                        else{
                            dataManager.updateScalars(ceaRoute, scalarIris, scalars, i, namedGraph);
                        }
                        tsHelper.addDataToTimeSeries(timeSeries.get(i), times, tsIris);
                    }
                }
                else if (requestUrl.contains(URI_ACTION)) {
                    ArrayList<CEAInputData> testData = new ArrayList<>();
                    ArrayList<String> uriStringArray = new ArrayList<>();
                    List<String> uniqueSurrounding = new ArrayList<>();
                    List<Coordinate> surroundingCoordinates = new ArrayList<>();
                    String crs = new String();
                    String terrainDb = defaultTerrainDb;
                    String terrainTable = defaultTerrainTable;
                    BuildingUsageHelper usageHelper = new BuildingUsageHelper(ontologyUriHelper);
                    SurroundingsHelper surroundingsHelper = new SurroundingsHelper(ontologyUriHelper);
                    WeatherHelper weatherHelper = null;

                    for (int i = 0; i < uriArray.length(); i++) {
                        String uri = uriArray.getString(i);
                        uniqueSurrounding.add(uri);

                        // Only set route once - assuming all iris passed in same namespace
                        // Will not be necessary if namespace is passed in request params
                        if (i == 0) {
                            // if KEY_GEOMETRY is not specified in requestParams, geometryRoute defaults to TheWorldAvatar Blazegraph
                            geometryRoute = requestParams.has(KEY_GEOMETRY) ? requestParams.getString(KEY_GEOMETRY) : getRoute(uri);
                            // if KEY_USAGE is not specified in requestParams, geometryRoute defaults to TheWorldAvatar Blazegraph
                            usageRoute = requestParams.has(KEY_USAGE) ? requestParams.getString(KEY_USAGE) : stackAccessAgentBase + defaultUsageLabel;
                            weatherRoute = requestParams.has(KEY_WEATHER) ? requestParams.getString(KEY_WEATHER) : stackAccessAgentBase + defaultWeatherLabel;
                            terrainDb = requestParams.has(KEY_TERRAIN_DB) ? requestParams.getString(KEY_TERRAIN_TABLE) : terrainDb;
                            terrainTable = requestParams.has(KEY_TERRAIN_TABLE) ? requestParams.getString(KEY_TERRAIN_TABLE) : terrainTable;

                            if (!requestParams.has(KEY_CEA)) {
                                // if KEY_CEA is not specified in requestParams, set ceaRoute to stack Blazegraph
                                ceaRoute = stackAccessAgentBase + defaultCeaLabel;
                                namedGraph = requestParams.has(KEY_GRAPH) ? requestParams.getString(KEY_GRAPH) : "";

                            } else {
                                ceaRoute = requestParams.getString(KEY_CEA);
                                // if KEY_CEA is specified, assume no graph if KEY_GRAPH is not specified in requestParams
                                if (requestParams.has(KEY_GRAPH)) {
                                    namedGraph = requestParams.getString(KEY_GRAPH);
                                    // ensures that graph ends with /
                                    if (!namedGraph.endsWith("/")) {
                                        namedGraph = namedGraph + "/";
                                    }
                                } else {
                                    namedGraph = "";
                                }
                            }

                            // check if ceaRoute has quads enabled for querying and updating with graphs
                            if (!namedGraph.isEmpty()) {
                                if (!RouteHelper.checkQuadsEnabled(ceaRoute)) {
                                    throw new JPSRuntimeException("ceaEndpoint does not support graph");
                                }
                            }
                            List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                            storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                            weatherHelper = new WeatherHelper(openmeteoagentUrl, dbUser, dbPassword, weatherRoute, ontologyUriHelper);
                        }

                        uriStringArray.add(uri);

                        String height = geometryQueryHelper.getBuildingGeometry(uri, geometryRoute, "height");

                        // Get footprint from ground thematic surface or find from surface geometries depending on data
                        String footprint = geometryQueryHelper.getBuildingGeometry(uri, geometryRoute, "footprint");

                        // Get building usage, set default usage of MULTI_RES if not available in knowledge graph
                        Map<String, Double> usage = usageHelper.getBuildingUsages(uri, usageRoute);

                        ArrayList<CEAInputData> surrounding = surroundingsHelper.getSurroundings(uri, geometryRoute, uniqueSurrounding, surroundingCoordinates);

                        //just get crs once - assuming all iris in same namespace
                        if (i == 0) {
                            crs = geometryQueryHelper.getBuildingGeometry(uri, geometryRoute, "crs");
                            if (crs.isEmpty()) {
                                crs = BuildingHelper.getNamespace(uri).split("EPSG").length == 2 ? BuildingHelper.getNamespace(uri).split("EPSG")[1].split("/")[0] : "27700";
                            }
                        }

                        List<Object> weather = new ArrayList<>();

                        if (weatherHelper.getWeather(uri, geometryRoute, weatherRoute, crs, weather)) {
                            testData.add(new CEAInputData(footprint, height, usage, surrounding, (List<OffsetDateTime>) weather.get(0), (Map<String, List<Double>>) weather.get(1), (List<Double>) weather.get(2)));
                        }
                        else{
                            testData.add(new CEAInputData(footprint, height, usage, surrounding, null, null, null));
                        }
                    }
                    String terrainUrl = endpointConfig.getDbUrl(terrainDb);
                    TerrainHelper terrainHelper = new TerrainHelper(terrainUrl, dbUser, dbPassword);
                    byte[] terrain = terrainHelper.getTerrain(uriStringArray.get(0), geometryRoute, crs, surroundingCoordinates, terrainTable, ontologyUriHelper);
                    // Manually set thread number to 0 - multiple threads not working so needs investigating
                    // Potentially issue is CEA is already multi-threaded
                    runCEA(testData, uriStringArray, 0, crs, terrain);
                }
            }
            else if (requestUrl.contains(URI_QUERY)) {
                DataManager dataManager = new DataManager(ontologyUriHelper);
                DataRetriever dataRetriever = new DataRetriever(ontologyUriHelper);

                for (int i = 0; i < uriArray.length(); i++) {
                    String uri = uriArray.getString(i);

                    // Only set route once - assuming all iris passed in same namespace
                    if(i==0) {
                        if (!requestParams.has(KEY_CEA)){
                            // if KEY_CEA is not specified in requestParams, set ceaRoute to stack Blazegraph
                            ceaRoute = stackAccessAgentBase + defaultCeaLabel;
                            namedGraph = requestParams.has(KEY_GRAPH) ? requestParams.getString(KEY_GRAPH) : "";
                        }
                        else{
                            ceaRoute = requestParams.getString(KEY_CEA);
                            // if KEY_CEA is specified, assume no graph if KEY_GRAPH is not specified in requestParams
                            if (requestParams.has(KEY_GRAPH)){
                                namedGraph =  requestParams.getString(KEY_GRAPH);
                                // ensures that graph ends with /
                                if (!namedGraph.endsWith("/")) {namedGraph = namedGraph + "/";}
                            }
                            else{
                                namedGraph = "";
                            }
                            // check if ceaRoute has quads enabled for querying and updating with graphs
                            if (!namedGraph.isEmpty() && RouteHelper.checkEndpoint(ceaRoute)){
                                if (!RouteHelper.checkQuadsEnabled(ceaRoute)) {
                                    throw new JPSRuntimeException("ceaEndpoint does not support graph");
                                }
                            }
                        }
                        List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                        storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                    }
                    String building = dataManager.checkBuildingInitialised(uri, ceaRoute);
                    if(building.equals("")){
                        return requestParams;
                    }
                    JSONObject data = new JSONObject();
                    List<String> allMeasures = new ArrayList<>();
                    Stream.of(CEAConstants.TIME_SERIES, CEAConstants.SCALARS).forEach(allMeasures::addAll);
                    for (String measurement: allMeasures) {
                        ArrayList<String> result = dataRetriever.getDataIRI(building, measurement, ceaRoute);
                        if (!result.isEmpty()) {
                            String value;
                            if (CEAConstants.TIME_SERIES.contains(measurement)) {
                                value = DataParser.calculateAnnual(TimeSeriesHelper.retrieveData(result.get(0), storeClient, rdbStoreClient, OffsetDateTime.class), result.get(0));
                                if (measurement.contains("ESupply")) {
                                    // PVT annual electricity supply
                                    measurement = "Annual "+ measurement.split("ESupply")[0] + " Electricity Supply";
                                }
                                else if (measurement.contains("QSupply")) {
                                    // PVT annual heat supply
                                    measurement = "Annual "+ measurement.split("QSupply")[0] + " Heat Supply";
                                }
                                else {
                                    if (measurement.contains("Thermal")) {
                                        // solar collector annual heat supply
                                        measurement = "Annual "+ measurement.split("Supply")[0] + " Heat Supply";
                                    }
                                    else if (measurement.contains("PV")) {
                                        // PV annual electricity supply
                                        measurement = "Annual "+ measurement.split("Supply")[0] + " Electricity Supply";
                                    }
                                    else {
                                        // annual energy consumption
                                        measurement = "Annual " + measurement;
                                    }
                                }
                            } else {
                                value = dataRetriever.getNumericalValue(result.get(0), ceaRoute, namedGraph);
                            }
                            // Return non-zero values
                            if(!(value.equals("0") || value.equals("0.0"))){
                                value += " " + dataRetriever.getUnit(result.get(1));
                                data.put(measurement, value);
                            }
                        }
                    }
                    requestParams.append(CEA_OUTPUTS, data);
                }
            }

        }
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean error = true;

        if (!requestParams.isEmpty()) {
            Set<String> keys = requestParams.keySet();
            if (keys.contains(KEY_REQ_METHOD) && keys.contains(KEY_REQ_URL) && keys.contains(KEY_IRI)) {
                if (requestParams.get(KEY_REQ_METHOD).equals(HttpMethod.POST)) {
                    try {
                        URL reqUrl = new URL(requestParams.getString(KEY_REQ_URL));
                        if (reqUrl.getPath().contains(URI_UPDATE)) {
                            error = validateUpdateInput(requestParams);
                        } else if (reqUrl.getPath().contains(URI_ACTION)) {
                            error = validateActionInput(requestParams);
                        } else if (reqUrl.getPath().contains(URI_QUERY)) {
                            error = validateQueryInput(requestParams);
                        }
                    } catch (Exception e) {
                        throw new BadRequestException();
                    }
                }
            }
        }

        if (error) {
            throw new BadRequestException();
        }

        return true;
    }

    /**
     * Validates input specific to requests coming to URI_UPDATE
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateUpdateInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty() ||
                requestParams.get(KEY_TARGET_URL).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_GRID_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_ELECTRICITY_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_HEATING_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_COOLING_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PV_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PV_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PV_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PV_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PV_WALL_WEST_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_ROOF_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_ROOF_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_ROOF_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_ROOF_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_PLATE_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_TUBE_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(CEAConstants.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_TIMES).toString().isEmpty();
        return error;
    }

    /**
     * Validates input specific to requests coming to URI_ACTION
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateActionInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty();

        if (requestParams.has(KEY_GEOMETRY)) {error = error || requestParams.get(KEY_GEOMETRY).toString().isEmpty();}
        if (requestParams.has(KEY_USAGE)) {error = error || requestParams.get(KEY_USAGE).toString().isEmpty();}
        if (requestParams.has(KEY_CEA)) {error = error || requestParams.get(KEY_CEA).toString().isEmpty();}
        if (requestParams.has(KEY_GRAPH)) {error = error || requestParams.get(KEY_GRAPH).toString().isEmpty();}

        return error;
    }

    /**
     * Validates input specific to requests coming to URI_QUERY
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateQueryInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty();

        if (requestParams.has(KEY_CEA)) {error = error || requestParams.get(KEY_CEA).toString().isEmpty();}
        if (requestParams.has(KEY_GRAPH)) {error = error || requestParams.get(KEY_GRAPH).toString().isEmpty();}
        return error;
    }

    /**
     * Gets variables from config
     */
    private void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("CEAAgentConfig");
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/berlin/sparql/", config.getString("berlin.targetresourceid"));
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/", config.getString("singaporeEPSG24500.targetresourceid"));
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG4326/sparql/", config.getString("singaporeEPSG4326.targetresourceid"));
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG3857/sparql/", config.getString("kingslynnEPSG3857.targetresourceid"));
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/", config.getString("kingslynnEPSG27700.targetresourceid"));
        accessAgentRoutes.put("http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/", config.getString("pirmasensEPSG32633.targetresourceid"));
        stackAccessAgentBase = config.getString("access.url");
        defaultCeaLabel = config.getString("cea.label");
        defaultUsageLabel = config.getString("usage.label");
        defaultWeatherLabel = config.getString("weather.label");
        openmeteoagentUrl = config.getString("url.openmeteoagent");
        defaultTerrainDb = config.getString("postgis.database");
        defaultTerrainTable = config.getString("postgis.table");
        tsDb = config.getString("cea.database");
    }

    /**
     * Runs CEATask on CEAInputData and returns CEAOutputData
     * @param buildingData input data on building footprint, height, usage, surrounding and weather
     * @param uris list of input uris
     * @param threadNumber int tracking thread that is running
     * @param crs coordinate reference system
     * @param terrain input data on terrain
     */
    private void runCEA(ArrayList<CEAInputData> buildingData, ArrayList<String> uris, Integer threadNumber, String crs, byte[] terrain) {
        try {
            RunCEATask task = new RunCEATask(buildingData, new URI(targetUrl), uris, threadNumber, crs, terrain);
            CEAExecutor.execute(task);
        }
        catch(URISyntaxException e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Returns route for use with AccessAgent
     * @param iriString iri of object to be queried
     * @return route of endpoint that iri belongs to
     */
    private String getRoute(String iriString) {
        String namespaceEndpoint = BuildingHelper.getNamespace(iriString);
        String route = accessAgentRoutes.get(namespaceEndpoint);
        return route;
    }
}
