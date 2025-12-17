package uk.ac.cam.cares.jps.agent.cea;

import com.cmclinnovations.stack.clients.core.StackClient;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAMetaData;
import uk.ac.cam.cares.jps.agent.cea.utils.AnnualValueHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAExecutionInput;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.*;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.*;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.*;
import uk.ac.cam.cares.jps.agent.cea.utils.input.*;
import uk.ac.cam.cares.jps.agent.cea.tasks.RunCEATask;
import uk.ac.cam.cares.jps.agent.cea.tasks.CEAOutputUpdater;

import org.json.JSONArray;
import org.json.JSONObject;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.*;
import java.time.OffsetDateTime;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.locationtech.jts.geom.Geometry;

import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

@WebServlet(urlPatterns = {
        CEAAgent.URI_ACTION,
        CEAAgent.URI_EXECUTE,
        CEAAgent.URI_UPDATE,
        CEAAgent.URI_QUERY
})
public class CEAAgent extends JPSAgent {
    public static final String KEY_REQ_METHOD = "method";
    public static final String URI_ACTION = "/run";
    public static final String URI_EXECUTE = "/execute";
    public static final String URI_UPDATE = "/update";
    public static final String URI_QUERY = "/query";
    public static final String KEY_REQ_URL = "requestUrl";
    public static final String KEY_TARGET_URL = "targetUrl";
    public static final String KEY_CALLER_URL = "callerUrl";
    public static final String KEY_CEA_PAYLOAD = "ceaPayload";
    public static final String KEY_IRI = "iris";
    public static final String KEY_GEOMETRY = "geometryEndpoint";
    public static final String KEY_USAGE = "usageEndpoint";
    public static final String KEY_WEATHER = "weatherEndpoint";
    public static final String KEY_TERRAIN_DB = "terrainDatabase";
    public static final String KEY_TERRAIN_TABLE = "terrainTable";
    public static final String KEY_CEA = "ceaEndpoint";
    public static final String KEY_SOLAR = "solarProperties";
    public static final Set<String> SOLAR_PARAMETERS = new HashSet<>(Arrays.asList("annual-radiation-threshold", "max-roof-coverage", "panel-tilt-angle", "t-in-sc", "t-in-pvt"));

    public static final String KEY_TIMES = "times";
    public static final String CEA_OUTPUTS = "ceaOutputs";
    public final int NUM_CEA_THREADS = 1;
    private final ThreadPoolExecutor CEAExecutor = (ThreadPoolExecutor) Executors.newFixedThreadPool(NUM_CEA_THREADS);

    private static final String PROPERTIES_PATH = "/resources/CEAAgentConfig.properties";

    public static final String STACK_NAME = "<STACK NAME>";
    private String stackName;
    private EndpointConfig endpointConfig = new EndpointConfig();
    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;
    private String stackAccessAgentBase;
    private String defaultCeaLabel;
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
    private String openmeteoagentUrl;
    private String ontopUrl;
    private List<String> ceaDb;
    private CEAOutputUpdater updater;

    private Gson ceaGsonBuilder = new GsonBuilder()
                        .registerTypeAdapter(Geometry.class, new GeometryTypeAdapter()) 
                        .create();

    public CEAAgent() {
        readConfig();
        ontopUrl = endpointConfig.getOntopUrl();
        stackName = StackClient.getStackName();
        stackAccessAgentBase = stackAccessAgentBase.replace(STACK_NAME, stackName);
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
            String uriArrayString = "[]";

            if (!requestUrl.contains(URI_EXECUTE)) {
                uriArrayString = requestParams.get(KEY_IRI).toString();
            }

            JSONArray uriArray = new JSONArray(uriArrayString);

            if (requestUrl.contains(URI_ACTION)) {
                ArrayList<CEABuildingData> buildingData = new ArrayList<>();
                ArrayList<String> uriStringArray = IntStream.range(0, uriArray.length()).mapToObj(uriArray::getString)
                        .collect(Collectors.toCollection(ArrayList::new));
                String crs = "";
                String terrainDb = defaultTerrainDb;
                String terrainTable = defaultTerrainTable;
                WeatherHelper weatherHelper = null;

                JSONObject solar = requestParams.has(KEY_SOLAR) ? requestParams.getJSONObject(KEY_SOLAR) : null;

                if (solar != null) {
                    Set<String> invalidKeys = new HashSet<>();

                    Iterator<String> keys = solar.keys();
                    while (keys.hasNext()) {
                        String key = keys.next();
                        if (!SOLAR_PARAMETERS.contains(key)) {
                            invalidKeys.add(key);
                        }
                    }
                    if (!invalidKeys.isEmpty()) {
                        JSONObject err = new JSONObject();
                        err.put("error", "Invalid solar parameter(s) provided.");
                        err.put("invalid_parameters", invalidKeys);
                        err.put("valid_parameters", SOLAR_PARAMETERS);
                        return err;
                    }
                }

                // if KEY_GEOMETRY is not specified in requestParams, geometryRoute defaults to
                // ontop endpoint
                geometryRoute = requestParams.has(KEY_GEOMETRY) ? requestParams.getString(KEY_GEOMETRY) : ontopUrl;
                // if KEY_USAGE is not specified in requestParams, geometryRoute defaults to
                // ontop endpoint
                usageRoute = requestParams.has(KEY_USAGE) ? requestParams.getString(KEY_USAGE) : ontopUrl;
                weatherRoute = requestParams.has(KEY_WEATHER) ? requestParams.getString(KEY_WEATHER)
                        : stackAccessAgentBase + defaultWeatherLabel;
                ceaRoute = requestParams.has(KEY_CEA) ? requestParams.getString(KEY_CEA)
                        : stackAccessAgentBase + defaultCeaLabel;
                terrainDb = requestParams.has(KEY_TERRAIN_DB) ? requestParams.getString(KEY_TERRAIN_TABLE) : terrainDb;
                terrainTable = requestParams.has(KEY_TERRAIN_TABLE) ? requestParams.getString(KEY_TERRAIN_TABLE)
                        : terrainTable;

                if (!RouteHelper.checkEndpoint(ceaRoute)) {
                    throw new JPSRuntimeException("ceaEndpoint not accessible");
                }

                List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                weatherHelper = new WeatherHelper(openmeteoagentUrl, dbUser, dbPassword, weatherRoute);
                updater = new CEAOutputUpdater(storeClient, rdbStoreClient, tsDb, ceaRoute);

                // Get footprint from building endpoints
                List<CEAGeometryData> listFootprint = GeometryQueryHelper.bulkGetBuildingGeometry(uriStringArray,
                        geometryRoute);

                try {
                    GeometryHandler.ensureSameCRS(listFootprint);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                List<Map<String, Double>> listUsage = BuildingUsageHelper.bulkGetBuildingUsages(uriStringArray,
                        usageRoute);

                for (int i = 0; i < uriStringArray.size(); i++) {
                    buildingData.add(new CEABuildingData(listFootprint.get(i), listUsage.get(i)));
                }

                crs = buildingData.get(0).getGeometry().getCrs();

                List<CEAGeometryData> surrounding = SurroundingsHelper.getSurroundings(buildingData, uriStringArray,
                        geometryRoute);

                try {
                    GeometryHandler.ensureSameCRS(surrounding);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                List<Object> weather = new ArrayList<>();

                String terrainUrl = endpointConfig.getDbUrl(terrainDb);

                TerrainHelper terrainHelper = new TerrainHelper(terrainUrl, dbUser, dbPassword);

                byte[] terrain = terrainHelper.getTerrain(buildingData, surrounding, terrainTable);

                CEAMetaData ceaMetaData;

                if (weatherHelper.getWeather(buildingData.get(0).getGeometry(), surrounding, weatherRoute, crs,
                        weather)) {
                    ceaMetaData = new CEAMetaData(surrounding, (List<OffsetDateTime>) weather.get(0),
                            (Map<String, List<Double>>) weather.get(1), (List<Double>) weather.get(2), terrain);
                } else {
                    ceaMetaData = new CEAMetaData(surrounding, null, null, null, terrain);
                }

                String ceaDatabase = ceaDb.get(0);

                String tempS = GeometryQueryHelper
                        .getCountry(buildingData.get(0).getGeometry().getFootprint().get(0).getCoordinate());

                tempS = tempS.toUpperCase();

                if (ceaDb.contains(tempS)) {
                    ceaDatabase = tempS;
                }

                // either run locally or delegate CEA simulation to external CEA agent

                String callerUrl = requestParams.has(KEY_CALLER_URL) ? requestParams.getString(KEY_CALLER_URL) : null;
                String targetUrl = requestParams.has(KEY_TARGET_URL) ? requestParams.getString(KEY_TARGET_URL) : null;

                if ((callerUrl!=null)&&(targetUrl!=null)) {

                    // delegate CEA simulation to external CEA agent

                    CEAExecutionInput inputData = new CEAExecutionInput(buildingData, ceaMetaData, uriStringArray, 0, crs, ceaDatabase, solar);

                    String jsonInput;
                    try {
                        jsonInput = ceaGsonBuilder.toJson(inputData);
                    } catch (Exception e) {
                        // Handle serialization error using your JPSRuntimeException pattern
                        System.err.println("Error packaging CEA inputs to JSON: " + e.getMessage());
                        throw new JPSRuntimeException("cannot package CEA inputs to JSON.", e);
                    }

                    String encodedJsonPayload = URLEncoder.encode(jsonInput, StandardCharsets.UTF_8);
                    String encodedCallerUrl = URLEncoder.encode(callerUrl, StandardCharsets.UTF_8);

                    String requestBody = KEY_CEA_PAYLOAD + "=" + encodedJsonPayload + "&" + KEY_CALLER_URL + "=" + encodedCallerUrl;

                    HttpClient client = HttpClient.newHttpClient();
                    String fullExecutionUrl = targetUrl + URI_EXECUTE;

                    CompletableFuture<HttpResponse<String>> futureResponse = client.sendAsync(
                        HttpRequest.newBuilder()
                            .uri(URI.create(fullExecutionUrl))
                            .header("Content-Type", "application/x-www-form-urlencoded")
                            // Pass the properly formatted and encoded string as the body
                            .POST(HttpRequest.BodyPublishers.ofString(requestBody, StandardCharsets.UTF_8))
                            .build(),
                        HttpResponse.BodyHandlers.ofString()
                    );

                    // Use thenAccept/exceptionally to log the result without blocking the current thread
                    futureResponse.thenAccept(response -> {
                        if (response.statusCode() >= 200 && response.statusCode() < 300) {
                            System.out.println("CEA delegation request sent successfully (status: " + response.statusCode() + ").");
                        } else {
                            System.err.println("Remote server responded with error status: " + response.statusCode() + ", Body: " + response.body());
                        }
                    }).exceptionally(e -> {
                        System.err.println("Network connection error during CEA delegation: " + e.getMessage());
                        return null;
                    });


                } else {

                    if (targetUrl!=null) {
                        System.out.println("Missing callerUrl, assume local execution");
                    };

                    if (callerUrl!=null) {
                        System.out.println("Missing targetUrl, assume local execution");
                    };

                    runCEA(buildingData, ceaMetaData, uriStringArray, 0, crs, ceaDatabase, solar, null);

                }

            } else if (requestUrl.contains(URI_EXECUTE)) {

                String rawBodyString = requestParams.getString("body");

                JSONObject trueParams;
                try {
                    trueParams = parseFormEncodedString(rawBodyString); 

                } catch (Exception e) {
                    System.err.println("Failed to parse form-encoded request body: " + e.getMessage());
                    throw new JPSRuntimeException("Could not parse request body into parameters.", e);
                }

                String callerUrl = trueParams.has(KEY_CALLER_URL) ? trueParams.getString(KEY_CALLER_URL) : null;
                
                // Retrieve the entire JSON string payload from the single key
                String jsonPayload = trueParams.has(KEY_CEA_PAYLOAD) ? trueParams.getString(KEY_CEA_PAYLOAD) : null;

                if (jsonPayload == null) {
                    System.err.println("Error: Missing CEA payload in request.");
                    // Handle error response (e.g., return an error status/JSON)
                    return requestParams; 
                }

                // unpack input data from caller's request

                CEAExecutionInput inputData;
                try {
                    inputData = ceaGsonBuilder.fromJson(jsonPayload, CEAExecutionInput.class);
                } catch (Exception e) {
                    throw new JPSRuntimeException("cannot read CEA payload.");
                }

                ArrayList<CEABuildingData> buildingData = inputData.getBuildingData();
                CEAMetaData ceaMetaData = inputData.getCeaMetaData();
                ArrayList<String> uris = inputData.getUris();
                Integer threadNumber = inputData.getThreadNumber();
                String crs = inputData.getCrs();
                String ceaDatabase = inputData.getCeaDatabase();
                JSONObject solar = inputData.getSolar();

                runCEA(buildingData, ceaMetaData, uris, threadNumber, crs, ceaDatabase, solar, callerUrl);

            } else if (requestUrl.contains(URI_UPDATE)) {

                // unpack data from request

                // parse times
                List<OffsetDateTime> times = DataParser.getTimesList(requestParams, KEY_TIMES);

                // parse times series data
                List<List<List<?>>> timeSeries = new ArrayList<>();
                for (int i = 0; i < uriArray.length(); i++) {
                    List<List<?>> iriList = new ArrayList<>();
                    for (String ts : CEAConstants.TIME_SERIES) {
                        iriList.add(DataParser.getTimeSeriesList(requestParams, ts, i));
                    }
                    timeSeries.add(iriList);
                }

                // parse scalar output

                LinkedHashMap<String, List<Double>> scalars = new LinkedHashMap<>();

                for (String scalar : CEAConstants.SCALARS) {
                    scalars.put(scalar, DataParser.getList(requestParams, scalar));
                }

                // update triplestore and relational database

                updater.updateCEA(uriArray, times, timeSeries, scalars);

            } else if (requestUrl.contains(URI_QUERY)) {
                for (int i = 0; i < uriArray.length(); i++) {

                    // Only set route once - assuming all iris passed in same namespace
                    if (i == 0) {
                        ceaRoute = requestParams.has(KEY_CEA) ? requestParams.getString(KEY_CEA)
                                : stackAccessAgentBase + defaultCeaLabel;

                        if (!RouteHelper.checkEndpoint(ceaRoute)) {
                            throw new JPSRuntimeException("ceaEndpoint not accessible");
                        }

                        List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                        storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                    }

                    String uri = uriArray.getString(i);
                    JSONObject data = new JSONObject(); // container of outputs of a building

                    if (!DataManager.checkBuildingInitialised(uri, ceaRoute)) {
                        // building in question has not been initialised,  continue to the next building
                        requestParams.append(CEA_OUTPUTS, data);
                        continue;
                    }

                    LinkedHashMap<String, String> tsIris = new LinkedHashMap<>();
                    LinkedHashMap<String, String> scalarIris = new LinkedHashMap<>();
                    LinkedHashMap<String, JSONObject> outputMap = new LinkedHashMap<>();

                    if (!DataManager.checkDataInitialised(uri, tsIris, scalarIris, outputMap, ceaRoute)) {
                        // building in question does not have complete outputs, continue to the next building
                        requestParams.append(CEA_OUTPUTS, data);
                        continue;
                    }

                    // retrieve scalar values
                    for (String scalar : CEAConstants.SCALARS) {
                        JSONObject scalarOutput = outputMap.get(scalarIris.get(scalar));
                        String value = scalarOutput.getString("value") + " "
                                + DataRetriever.getUnit(scalarOutput.getString("unit"));
                        data.put(scalar, value);
                    }

                    // retrieve annual values (summary of time series output)

                    List<String> dataIRIList = new ArrayList<>(tsIris.values());
                    Map<String, JSONObject> annualObjectMap = AnnualValueHelper.bulkCheckAnnualObject(dataIRIList,
                            ceaRoute);
                    for (String measurement : CEAConstants.TIME_SERIES) {
                        String tsDataIRI = tsIris.get(measurement);
                        JSONObject annualObject = annualObjectMap.get(tsDataIRI);
                        String value = annualObject.get("numericalValue").toString() + " "
                                + DataRetriever.getUnit(annualObject.getString("unit"));
                        // more human-friendly label
                        if (measurement.contains("ESupply")) {
                            // PVT annual electricity supply
                            measurement = measurement.split("ESupply")[0] + " Electricity Supply";
                        } else if (measurement.contains("QSupply")) {
                            // PVT annual heat supply
                            measurement = measurement.split("QSupply")[0] + " Heat Supply";
                        } else if (measurement.contains("Thermal")) {
                            // solar collector annual heat supply
                            measurement = measurement.split("Supply")[0] + " Heat Supply";
                        } else if (measurement.contains("PV")) {
                            // PV annual electricity supply
                            measurement = measurement.split("Supply")[0] + " Electricity Supply";
                        }
                        // annual energy consumption
                        measurement = "Annual " + measurement;
                        data.put(measurement, value);
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
            if (keys.contains(KEY_REQ_METHOD) && keys.contains(KEY_REQ_URL)) {
                if (requestParams.get(KEY_REQ_METHOD).equals(HttpMethod.POST)) {
                    try {
                        URL reqUrl = new URL(requestParams.getString(KEY_REQ_URL));
                        if (reqUrl.getPath().contains(URI_UPDATE) && keys.contains(KEY_IRI)) {
                            error = validateUpdateInput(requestParams);
                        } else if (reqUrl.getPath().contains(URI_ACTION) && keys.contains(KEY_IRI)) {
                            error = validateActionInput(requestParams);
                        } else if (reqUrl.getPath().contains(URI_QUERY) && keys.contains(KEY_IRI)) {
                            error = validateQueryInput(requestParams);
                        } else if (reqUrl.getPath().contains(URI_EXECUTE)) { // execute route does not need IRI
                            error = validateExecuteInput(requestParams);
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
     * 
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateUpdateInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty() ||
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
     * 
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateActionInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty();

        if (requestParams.has(KEY_GEOMETRY)) {
            error = error || requestParams.get(KEY_GEOMETRY).toString().isEmpty();
        }
        if (requestParams.has(KEY_USAGE)) {
            error = error || requestParams.get(KEY_USAGE).toString().isEmpty();
        }
        if (requestParams.has(KEY_CEA)) {
            error = error || requestParams.get(KEY_CEA).toString().isEmpty();
        }

        return error;
    }

    /**
     * Validates input specific to requests coming to URI_QUERY
     * 
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateQueryInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty();

        if (requestParams.has(KEY_CEA)) {
            error = error || requestParams.get(KEY_CEA).toString().isEmpty();
        }
        return error;
    }

    /**
     * Validates input specific to requests coming to URI_EXECUTE
     * 
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateExecuteInput(JSONObject requestParams) {

        String rawBodyString = requestParams.getString("body");

        JSONObject trueParams;
        try {
            trueParams = parseFormEncodedString(rawBodyString); 
        } catch (Exception e) {
            System.err.println("Failed to parse form-encoded request body: " + e.getMessage());
            return true;
        }

        boolean error = trueParams.get(KEY_CEA_PAYLOAD).toString().isEmpty() || trueParams.get(KEY_CALLER_URL).toString().isEmpty();

        return error;
    }

    /**
     * Gets variables from config
     */
    private void readConfig() {
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties config = new Properties();
            config.load(input);
            stackAccessAgentBase = config.getProperty("access.url");
            defaultCeaLabel = config.getProperty("cea.label");
            defaultWeatherLabel = config.getProperty("weather.label");
            openmeteoagentUrl = config.getProperty("url.openmeteoagent");
            defaultTerrainDb = config.getProperty("terrain.database");
            defaultTerrainTable = config.getProperty("terrain.table");
            tsDb = config.getProperty("cea.database");
            ceaDb = Arrays.asList(config.getProperty("cea.defined.databases").split(","));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Runs CEATask on CEAInputData, which will send request to the update endpoint
     * with the extracted CEAOutputData after running CEA simulations
     * 
     * @param buildingData input data on building footprint, height, usage,
     *                     surrounding and weather
     * @param uris         list of input uris
     * @param threadNumber int tracking thread that is running
     * @param crs          coordinate reference system
     */
    private void runCEA(ArrayList<CEABuildingData> buildingData, CEAMetaData ceaMetaData, ArrayList<String> uris, Integer threadNumber, String crs, String ceaDatabase, JSONObject solar, String callerUrl) {
        
        String updateUrl = null;
        if (callerUrl!=null) {
            try {
                updateUrl = callerUrl + URI_UPDATE;
                new URI(updateUrl);
            } catch (URISyntaxException e) {
                System.err.println("callerUrl is invalid: " + callerUrl);
                new JPSRuntimeException(e);
            }
        }
        
        try {
            RunCEATask task = new RunCEATask(buildingData, ceaMetaData, uris, threadNumber, crs, ceaDatabase, updater, solar, updateUrl);
            CEAExecutor.execute(task);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    public static JSONObject parseFormEncodedString(String formString) throws UnsupportedEncodingException {
        JSONObject result = new JSONObject();
        if (formString == null || formString.trim().isEmpty()) {
            return result;
        }
        
        // Split the string by the '&' separator
        String[] pairs = formString.split("&");
        
        for (String pair : pairs) {
            // Find the position of the '=' sign
            int idx = pair.indexOf("=");
            
            if (idx > 0) {
                String key = pair.substring(0, idx);
                String encodedValue = pair.substring(idx + 1);
                
                // Decode the value using UTF-8
                String value = URLDecoder.decode(encodedValue, StandardCharsets.UTF_8.toString());
                
                // Put the decoded key-value pair into the JSONObject
                result.put(key, value);
            }
        }
        return result;
    }

}
