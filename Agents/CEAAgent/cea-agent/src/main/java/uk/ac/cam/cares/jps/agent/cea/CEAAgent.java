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
import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.*;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.*;
import uk.ac.cam.cares.jps.agent.cea.utils.input.*;
import uk.ac.cam.cares.jps.agent.cea.tasks.RunCEATask;

import org.json.JSONArray;
import org.json.JSONObject;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.*;
import java.time.OffsetDateTime;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.Executors;
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

    private String targetUrl = "http://localhost:8084/cea-agent" + URI_UPDATE;

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

    public CEAAgent() {
        readConfig();
        ontopUrl = endpointConfig.getOntopUrl();
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

            if (requestUrl.contains(URI_ACTION)) {
                ArrayList<CEABuildingData> buildingData = new ArrayList<>();
                ArrayList<String> uriStringArray = new ArrayList<>();
                String crs = new String();
                String terrainDb = defaultTerrainDb;
                String terrainTable = defaultTerrainTable;
                WeatherHelper weatherHelper = null;

                for (int i = 0; i < uriArray.length(); i++) {
                    String uri = uriArray.getString(i);

                    // Only set route once - assuming all iris passed in same namespace
                    // Will not be necessary if namespace is passed in request params
                    if (i == 0) {
                        // if KEY_GEOMETRY is not specified in requestParams, geometryRoute defaults to TheWorldAvatar Blazegraph
                        geometryRoute = requestParams.has(KEY_GEOMETRY) ? requestParams.getString(KEY_GEOMETRY) : ontopUrl;
                        // if KEY_USAGE is not specified in requestParams, geometryRoute defaults to TheWorldAvatar Blazegraph
                        usageRoute = requestParams.has(KEY_USAGE) ? requestParams.getString(KEY_USAGE) : ontopUrl;
                        weatherRoute = requestParams.has(KEY_WEATHER) ? requestParams.getString(KEY_WEATHER) : stackAccessAgentBase + defaultWeatherLabel;
                        ceaRoute = requestParams.has(KEY_CEA) ? requestParams.getString(KEY_CEA) : stackAccessAgentBase + defaultCeaLabel;
                        terrainDb = requestParams.has(KEY_TERRAIN_DB) ? requestParams.getString(KEY_TERRAIN_TABLE) : terrainDb;
                        terrainTable = requestParams.has(KEY_TERRAIN_TABLE) ? requestParams.getString(KEY_TERRAIN_TABLE) : terrainTable;

                        if (!RouteHelper.checkEndpoint(ceaRoute)){
                            throw new JPSRuntimeException("ceaEndpoint not accessible");
                        }

                        List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                        storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                        weatherHelper = new WeatherHelper(openmeteoagentUrl, dbUser, dbPassword, weatherRoute);
                    }

                    uriStringArray.add(uri);

                    // Get footprint from ground thematic surface or find from surface geometries depending on data
                    CEAGeometryData footprint = GeometryQueryHelper.getBuildingGeometry(uri, ontopUrl, true);

                    // Get building usage, set default usage of MULTI_RES if not available in knowledge graph
                    Map<String, Double> usage = BuildingUsageHelper.getBuildingUsages(uri, usageRoute);
                    buildingData.add(new CEABuildingData(footprint, usage));
                }

                crs = buildingData.get(0).getGeometry().getCrs();

                List<CEAGeometryData> surrounding = SurroundingsHelper.getSurroundings(buildingData, uriStringArray, ontopUrl);

                List<Object> weather = new ArrayList<>();

                String terrainUrl = endpointConfig.getDbUrl(terrainDb);

                TerrainHelper terrainHelper = new TerrainHelper(terrainUrl, dbUser, dbPassword);

                byte[] terrain = terrainHelper.getTerrain(buildingData, surrounding, terrainTable);

                CEAMetaData ceaMetaData;

                if (weatherHelper.getWeather(buildingData.get(0).getGeometry(), surrounding, weatherRoute, crs, weather)) {
                    ceaMetaData = new CEAMetaData(surrounding, (List<OffsetDateTime>) weather.get(0), (Map<String, List<Double>>) weather.get(1), (List<Double>) weather.get(2), terrain);
                }
                else {
                    ceaMetaData = new CEAMetaData(surrounding, null, null, null, terrain);
                }

                // Manually set thread number to 0 - multiple threads not working so needs investigating
                // Potentially issue is CEA is already multi-threaded
                runCEA(buildingData, ceaMetaData, uriStringArray, 0, crs);
            }
            else if (requestUrl.contains(URI_UPDATE)) {
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

                LinkedHashMap<String, List<Double>> scalars = new LinkedHashMap<>();

                // parse PV area data
                for(String scalar: CEAConstants.SCALARS){
                    scalars.put(scalar, DataParser.getList(requestParams, scalar));
                }

                for (int i = 0; i < uriArray.length(); i++) {
                    LinkedHashMap<String,String> tsIris = new LinkedHashMap<>();
                    LinkedHashMap<String,String> scalarIris = new LinkedHashMap<>();

                    String uri = uriArray.getString(i);

                    if (!DataManager.checkBuildingInitialised(uri, ceaRoute)) {
                        DataManager.initialiseBuilding(uri, ceaRoute);
                    }

                    if(!DataManager.checkDataInitialised(uri, tsIris, scalarIris, ceaRoute)) {
                        tsHelper.createTimeSeries(tsIris);
                        DataManager.initialiseData(i, scalars, uri, tsIris, scalarIris, ceaRoute);
                    }
                    else{
                        DataManager.updateScalars(ceaRoute, scalarIris, scalars, i);
                    }
                    tsHelper.addDataToTimeSeries(timeSeries.get(i), times, tsIris);
                    AnnualValueHelper.instantiateAnnual(timeSeries.get(i), tsIris, ceaRoute);
                }
            }
            else if (requestUrl.contains(URI_QUERY)) {
                for (int i = 0; i < uriArray.length(); i++) {
                    String uri = uriArray.getString(i);

                    // Only set route once - assuming all iris passed in same namespace
                    if(i==0) {
                        ceaRoute = requestParams.has(KEY_CEA) ? requestParams.getString(KEY_CEA) : stackAccessAgentBase + defaultCeaLabel;

                        if (!RouteHelper.checkEndpoint(ceaRoute)){
                            throw new JPSRuntimeException("ceaEndpoint not accessible");
                        }

                        List<String> routeEndpoints = RouteHelper.getRouteEndpoints(ceaRoute);
                        storeClient = new RemoteStoreClient(routeEndpoints.get(0), routeEndpoints.get(1));
                    }

                    if(!DataManager.checkBuildingInitialised(uri, ceaRoute)){
                        return requestParams;
                    }

                    JSONObject data = new JSONObject();

                    // retrieve scalar values
                    for (String scalar : CEAConstants.SCALARS) {
                        ArrayList<String> result = DataRetriever.getDataIRI(uri, scalar, ceaRoute);
                        if (!result.isEmpty()) {
                            String value = DataRetriever.getNumericalValue(result.get(0), ceaRoute);
                            if(!(value.equals("0") || value.equals("0.0"))){
                                value += " " + DataRetriever.getUnit(result.get(1));
                                data.put(scalar, value);
                            }
                        }
                    }

                    for (String measurement : CEAConstants.TIME_SERIES) {
                        ArrayList<String> result = DataRetriever.getDataIRI(uri, measurement, ceaRoute);
                        String attachedIri = AnnualValueHelper.getInfo(result.get(0), measurement, ceaRoute);
                        String energyType = AnnualValueHelper.getType(result.get(0), ceaRoute);
                        String value = AnnualValueHelper.retrieveAnnualValue(attachedIri, energyType, ceaRoute);
                        if (!value.isEmpty()) {
                            if (CEAConstants.TIME_SERIES.contains(measurement)) {
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

                                // Return non-zero values
                                if(!(value.equals("0") || value.equals("0.0"))){
                                    value += " " + DataRetriever.getUnit(result.get(1));
                                    data.put(measurement, value);
                                }
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
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        }
        catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Runs CEATask on CEAInputData, which will send request to the update endpoint with the extracted CEAOutputData after running CEA simulations
     * @param buildingData input data on building footprint, height, usage, surrounding and weather
     * @param uris list of input uris
     * @param threadNumber int tracking thread that is running
     * @param crs coordinate reference system
     */
    private void runCEA(ArrayList<CEABuildingData> buildingData, CEAMetaData ceaMetaData, ArrayList<String> uris, Integer threadNumber, String crs) {
        try {
            RunCEATask task = new RunCEATask(buildingData, ceaMetaData, new URI(targetUrl), uris, threadNumber, crs);
            CEAExecutor.execute(task);
        }
        catch(URISyntaxException e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }
}
