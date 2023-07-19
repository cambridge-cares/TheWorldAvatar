package uk.ac.cam.cares.jps.agent.cea;

import com.cmclinnovations.stack.clients.core.StackClient;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.ceatasks.*;

import kong.unirest.Unirest;
import kong.unirest.HttpResponse;
import kong.unirest.UnirestException;
import org.apache.commons.lang.StringUtils;
import org.apache.http.protocol.HTTP;
import org.json.JSONArray;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.arq.querybuilder.handlers.WhereHandler;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;
import org.jooq.exception.DataAccessException;

import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;

import org.cts.op.CoordinateOperation;
import org.cts.op.CoordinateOperationFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
import org.cts.CRSFactory;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.crs.GeodeticCRS;

import org.json.JSONObject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.io.*;
import java.net.*;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import java.util.concurrent.*;
import java.util.stream.Stream;
import java.sql.Connection;
import java.sql.SQLException;


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

    public static final String CITY_OBJECT = "cityobject";
    public static final String ENERGY_PROFILE = "energyprofile";
    public static final String KEY_GRID_CONSUMPTION = "GridConsumption";
    public static final String KEY_ELECTRICITY_CONSUMPTION = "ElectricityConsumption";
    public static final String KEY_HEATING_CONSUMPTION = "HeatingConsumption";
    public static final String KEY_COOLING_CONSUMPTION = "CoolingConsumption";
    public static final String KEY_ROOF_SOLAR_SUITABLE_AREA = "RoofSolarSuitableArea";
    public static final String KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA = "SouthWallSolarSuitableArea";
    public static final String KEY_NORTH_WALL_SOLAR_SUITABLE_AREA = "NorthWallSolarSuitableArea";
    public static final String KEY_EAST_WALL_SOLAR_SUITABLE_AREA = "EastWallSolarSuitableArea";
    public static final String KEY_WEST_WALL_SOLAR_SUITABLE_AREA = "WestWallSolarSuitableArea";
    public static final String KEY_PV_ROOF_SUPPLY= "PVRoofSupply";
    public static final String KEY_PV_WALL_SOUTH_SUPPLY = "PVWallSouthSupply";
    public static final String KEY_PV_WALL_NORTH_SUPPLY = "PVWallNorthSupply";
    public static final String KEY_PV_WALL_EAST_SUPPLY = "PVWallEastSupply";
    public static final String KEY_PV_WALL_WEST_SUPPLY = "PVWallWestSupply";
    public static final String KEY_PVT_PLATE_ROOF_E_SUPPLY = "PVTPlateRoofESupply";
    public static final String KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY = "PVTPlateWallSouthESupply";
    public static final String KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY = "PVTPlateWallNorthESupply";
    public static final String KEY_PVT_PLATE_WALL_EAST_E_SUPPLY = "PVTPlateWallEastESupply";
    public static final String KEY_PVT_PLATE_WALL_WEST_E_SUPPLY = "PVTPlateWallWestESupply";
    public static final String KEY_PVT_PLATE_ROOF_Q_SUPPLY = "PVTPlateRoofQSupply";
    public static final String KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY = "PVTPlateWallSouthQSupply";
    public static final String KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY = "PVTPlateWallNorthQSupply";
    public static final String KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY = "PVTPlateWallEastQSupply";
    public static final String KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY = "PVTPlateWallWestQSupply";
    public static final String KEY_PVT_TUBE_ROOF_E_SUPPLY = "PVTTubeRoofESupply";
    public static final String KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY = "PVTTubeWallSouthESupply";
    public static final String KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY = "PVTTubeWallNorthESupply";
    public static final String KEY_PVT_TUBE_WALL_EAST_E_SUPPLY = "PVTTubeWallEastESupply";
    public static final String KEY_PVT_TUBE_WALL_WEST_E_SUPPLY = "PVTTubeWallWestESupply";
    public static final String KEY_PVT_TUBE_ROOF_Q_SUPPLY = "PVTTubeRoofQSupply";
    public static final String KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY = "PVTTubeWallSouthQSupply";
    public static final String KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY = "PVTTubeWallNorthQSupply";
    public static final String KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY = "PVTTubeWallEastQSupply";
    public static final String KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY = "PVTTubeWallWestQSupply";
    public static final String KEY_THERMAL_PLATE_ROOF_SUPPLY= "ThermalPlateRoofSupply";
    public static final String KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY = "ThermalPlateWallSouthSupply";
    public static final String KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY = "ThermalPlateWallNorthSupply";
    public static final String KEY_THERMAL_PLATE_WALL_EAST_SUPPLY = "ThermalPlateWallEastSupply";
    public static final String KEY_THERMAL_PLATE_WALL_WEST_SUPPLY = "ThermalPlateWallWestSupply";
    public static final String KEY_THERMAL_TUBE_ROOF_SUPPLY= "ThermalTubeRoofSupply";
    public static final String KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY = "ThermalTubeWallSouthSupply";
    public static final String KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY = "ThermalTubeWallNorthSupply";
    public static final String KEY_THERMAL_TUBE_WALL_EAST_SUPPLY = "ThermalTubeWallEastSupply";
    public static final String KEY_THERMAL_TUBE_WALL_WEST_SUPPLY = "ThermalTubeWallWestSupply";
    public static final String KEY_TIMES = "times";
    public static final String CEA_OUTPUTS = "ceaOutputs";
    private static final String CRS_4326 = "EPSG:4326";
    public String customDataType = "<http://localhost/blazegraph/literals/POLYGON-3-15>";
    public String customField = "X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4";

    public static final String STORE_CLIENT = "RemoteStoreClient";
    public static final String RDB_CLIENT = "RemoteRDBStoreClient";

    public static final String CTYPE_JSON = "application/json";
    public static final String OPENMETEO_ROUTE = "route";
    public static final String OPENMETEO_LAT = "latitude";
    public static final String OPENMETEO_LON = "longitude";
    public static final String OPENMETEO_START = "start_date";
    public static final String OPENMETEO_END = "end_date";
    public static final String OPENMETEO_ENDPOINT_RUN = "run";
    public static final String OPENMETEO_STATION = "stationIRI";

    public static final String API_URL = "https://archive-api.open-meteo.com/v1/archive";
    public static final String API_LAT = "latitude";
    public static final String API_LON = "longitude";
    public static final String API_START = "start_date";
    public static final String API_END = "end_date";
    private static final String API_TIMEZONE = "timezone";
    private static final String API_OFFSET = "utc_offset_seconds";

    public List<String> TIME_SERIES = Arrays.asList(KEY_GRID_CONSUMPTION,KEY_ELECTRICITY_CONSUMPTION,KEY_HEATING_CONSUMPTION,KEY_COOLING_CONSUMPTION, KEY_PV_ROOF_SUPPLY, KEY_PV_WALL_NORTH_SUPPLY, KEY_PV_WALL_SOUTH_SUPPLY, KEY_PV_WALL_EAST_SUPPLY, KEY_PV_WALL_WEST_SUPPLY, KEY_PVT_PLATE_ROOF_E_SUPPLY, KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, KEY_PVT_PLATE_ROOF_Q_SUPPLY, KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, KEY_PVT_TUBE_ROOF_E_SUPPLY, KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, KEY_PVT_TUBE_ROOF_Q_SUPPLY, KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, KEY_THERMAL_PLATE_ROOF_SUPPLY, KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, KEY_THERMAL_TUBE_ROOF_SUPPLY, KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, KEY_THERMAL_TUBE_WALL_WEST_SUPPLY);
    public List<String> SCALARS = Arrays.asList(KEY_ROOF_SOLAR_SUITABLE_AREA, KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA, KEY_NORTH_WALL_SOLAR_SUITABLE_AREA, KEY_EAST_WALL_SOLAR_SUITABLE_AREA, KEY_WEST_WALL_SOLAR_SUITABLE_AREA);

    public final int NUM_CEA_THREADS = 1;
    private final ThreadPoolExecutor CEAExecutor = (ThreadPoolExecutor) Executors.newFixedThreadPool(NUM_CEA_THREADS);

    private TimeSeriesClient<OffsetDateTime> tsClient;
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    
    private OntologyURIHelper ontologyUriHelper;

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
                    // parse times
                    List<OffsetDateTime> times = getTimesList(requestParams, KEY_TIMES);

                    // parse times series data;
                    List<List<List<?>>> timeSeries = new ArrayList<>();
                    for (int i = 0; i < uriArray.length(); i++) {
                        List<List<?>> iriList = new ArrayList<>();
                        for(String ts: TIME_SERIES) {
                            iriList.add(getTimeSeriesList(requestParams, ts, i));
                        }
                        timeSeries.add(iriList);
                    }

                    LinkedHashMap<String, List<String>> scalars = new LinkedHashMap<>();

                    // parse PV area data
                    for(String scalar: SCALARS){
                        scalars.put(scalar, getList(requestParams, scalar));
                    }

                    for (int i = 0; i < uriArray.length(); i++) {
                        LinkedHashMap<String,String> tsIris = new LinkedHashMap<>();
                        LinkedHashMap<String,String> scalarIris = new LinkedHashMap<>();

                        String uri = uriArray.getString(i);

                        String building = checkBuildingInitialised(uri, ceaRoute);
                        if(building.equals("")){
                            // Check if bot:Building IRI has already been created in another endpoint
                            building = checkBuildingInitialised(uri, geometryRoute);
                            building = initialiseBuilding(uri, building, ceaRoute, namedGraph);
                        }
                        if(!checkDataInitialised(building, tsIris, scalarIris, ceaRoute, namedGraph)) {
                            createTimeSeries(tsIris, namedGraph);
                            initialiseData(i, scalars, building, tsIris, scalarIris, ceaRoute, namedGraph);
                        }
                        else{
                            updateScalars(ceaRoute, scalarIris, scalars, i, namedGraph);
                        }
                        addDataToTimeSeries(timeSeries.get(i), times, tsIris);
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
                    BuildingUsageHelper usageQueryHelper = new BuildingUsageHelper(ontologyUriHelper);

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
                        }

                        uriStringArray.add(uri);

                        String height = getBuildingGeometry(uri, geometryRoute, "height");

                        // Get footprint from ground thematic surface or find from surface geometries depending on data
                        String footprint = getBuildingGeometry(uri, geometryRoute, "footprint");

                        // Get building usage, set default usage of MULTI_RES if not available in knowledge graph
                        Map<String, Double> usage = usageQueryHelper.getBuildingUsages(uri, usageRoute);

                        ArrayList<CEAInputData> surrounding = getSurroundings(uri, geometryRoute, uniqueSurrounding, surroundingCoordinates);

                        //just get crs once - assuming all iris in same namespace
                        if (i == 0) {
                            crs = getBuildingGeometry(uri, geometryRoute, "crs");
                            if (crs.isEmpty()) {
                                crs = BuildingHelper.getNamespace(uri).split("EPSG").length == 2 ? BuildingHelper.getNamespace(uri).split("EPSG")[1].split("/")[0] : "27700";
                            }
                        }

                        List<Object> weather = new ArrayList<>();

                        if (getWeather(uri, geometryRoute, weatherRoute, crs, weather)) {
                            testData.add(new CEAInputData(footprint, height, usage, surrounding, (List<OffsetDateTime>) weather.get(0), (Map<String, List<Double>>) weather.get(1), (List<Double>) weather.get(2)));
                        }
                        else{
                            testData.add(new CEAInputData(footprint, height, usage, surrounding, null, null, null));
                        }
                    }
                    byte[] terrain = getTerrain(uriStringArray.get(0), geometryRoute, crs, surroundingCoordinates, terrainDb, terrainTable);
                    // Manually set thread number to 0 - multiple threads not working so needs investigating
                    // Potentially issue is CEA is already multi-threaded
                    runCEA(testData, uriStringArray, 0, crs, terrain);
                }
            }
            else if (requestUrl.contains(URI_QUERY)) {

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
                    String building = checkBuildingInitialised(uri, ceaRoute);
                    if(building.equals("")){
                        return requestParams;
                    }
                    JSONObject data = new JSONObject();
                    List<String> allMeasures = new ArrayList<>();
                    Stream.of(TIME_SERIES, SCALARS).forEach(allMeasures::addAll);
                    for (String measurement: allMeasures) {
                        ArrayList<String> result = getDataIRI(building, measurement, ceaRoute);
                        if (!result.isEmpty()) {
                            String value;
                            if (TIME_SERIES.contains(measurement)) {
                                value = calculateAnnual(retrieveData(result.get(0), storeClient, rdbStoreClient, OffsetDateTime.class), result.get(0));
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
                                value = getNumericalValue(result.get(0), ceaRoute, namedGraph);
                            }
                            // Return non-zero values
                            if(!(value.equals("0") || value.equals("0.0"))){
                                value += " " + getUnit(result.get(1));
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
     * Parses input JSONObject into a list of strings
     * @param requestParams - request body in JSON format
     * @param key - requested data
     * @return List of data
     */
    private List<String> getList (JSONObject requestParams, String key) {
        JSONArray array = (JSONArray) requestParams.get(key);
        List<String> list = new ArrayList<>();
        for (int j = 0; j < array.length(); j++) {
            list.add(array.getString(j));
        }
        return list;
    }

    /**
     * Parses input JSONObject into a list of time series data
     * @param requestParams - request body in JSON format
     * @param key - requested data
     * @return List of data
     */
    private List<Double> getTimeSeriesList (JSONObject requestParams, String key, Integer index) {
        List<Double> timeSeriesList = new ArrayList<>();

        if (requestParams.has(key)) {
            JSONArray array = (JSONArray) requestParams.get(key);
            JSONArray timeDataArray = (JSONArray) array.get(index);

            for (int i = 0; i < timeDataArray.length(); i++) {
                timeSeriesList.add(Double.valueOf(timeDataArray.getString(i)));
            }
        }
        return timeSeriesList;
    }

    /**
     * Parses input JSONObject into a list of times
     * @param requestParams - request body in JSON format
     * @param key - requested data
     * @return List of times
     */
    private List<OffsetDateTime> getTimesList (JSONObject requestParams, String key) {
        JSONArray array = (JSONArray) requestParams.get(key);
        List<OffsetDateTime> list = new ArrayList<>();
        for (int j = 0; j < array.length(); j++) {
            OffsetDateTime odt = OffsetDateTime.parse(array.getString(j));
            list.add(odt);
        }
        return list;
    }

    /**
     * Validates input specific to requests coming to URI_UPDATE
     * @param requestParams - request body in JSON format
     * @return boolean saying if request is valid or not
     */
    private boolean validateUpdateInput(JSONObject requestParams) {
        boolean error = requestParams.get(KEY_IRI).toString().isEmpty() ||
                requestParams.get(KEY_TARGET_URL).toString().isEmpty() ||
                requestParams.get(KEY_GRID_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(KEY_ELECTRICITY_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(KEY_HEATING_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(KEY_COOLING_CONSUMPTION).toString().isEmpty() ||
                requestParams.get(KEY_PV_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PV_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PV_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PV_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PV_WALL_WEST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_ROOF_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_ROOF_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_EAST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_WEST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_ROOF_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_ROOF_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_EAST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_WEST_E_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_PLATE_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_PLATE_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_PLATE_WALL_WEST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_TUBE_ROOF_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_TUBE_WALL_EAST_SUPPLY).toString().isEmpty() ||
                requestParams.get(KEY_THERMAL_TUBE_WALL_WEST_SUPPLY).toString().isEmpty() ||
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
     * Creates and initialises a time series using the time series client
     * @param fixedIris map containing time series iris mapped to measurement type
     */
    private void createTimeSeries(LinkedHashMap<String,String> fixedIris, String graph) {
        tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);

        // Create a iri for each measurement
        List<String> iris = new ArrayList<>();
        for(String measurement: TIME_SERIES){
            String iri = measurement+"_"+UUID.randomUUID()+ "/";
            iri = !graph.isEmpty() ? graph + iri : ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + iri ;
            iris.add(iri);
            fixedIris.put(measurement, iri);
        }

        // Check whether IRIs have a time series linked and if not initialize the corresponding time series
        if(!timeSeriesExist(iris)) {
            // All values are doubles
            List<Class<?>> classes =  new ArrayList<>();
            for(int i=0; i<iris.size(); i++){
                classes.add(Double.class);
            }
            try (Connection conn = rdbStoreClient.getConnection()) {
                // Initialize the time series
                tsClient.initTimeSeries(iris, classes, timeUnit, conn, TimeSeriesClient.Type.STEPWISECUMULATIVE, null, null);
                //LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
            }
            catch (SQLException e) {
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Adds new data to time series
     * @param values output CEA data
     * @param times times for output time series data
     * @param iriMap iri map containing time series iris
     */
    private void addDataToTimeSeries(List<List<?>> values, List<OffsetDateTime> times, LinkedHashMap<String,String> iriMap) {
        List<String> iris = new ArrayList<>();
        for (String iri : iriMap.values()){
            iris.add(iri);
        }
        // If CreateTimeSeries has not been run, get time series client
        if(tsClient==null){
            tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);
        }
        TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);

        try (Connection conn = rdbStoreClient.getConnection()) {
            OffsetDateTime endDataTime = tsClient.getMaxTime(currentTimeSeries.getDataIRIs().get(0), conn);
            OffsetDateTime beginDataTime = tsClient.getMinTime(currentTimeSeries.getDataIRIs().get(0), conn);

            // Delete old data if exists
            if (endDataTime != null) {
                for (Integer i = 0; i < currentTimeSeries.getDataIRIs().size(); i++) {
                    tsClient.deleteTimeSeriesHistory(currentTimeSeries.getDataIRIs().get(i), beginDataTime, endDataTime, conn);
                }
            }
            // Add New data
            tsClient.addTimeSeriesData(currentTimeSeries, conn);
        }
        catch (SQLException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Checks whether a time series exists by checking whether any of the IRIs that should be attached to
     * the time series is not initialised in the central RDB lookup table using the time series client.
     * @param iris The IRIs that should be attached to the same time series provided as list of strings.
     * @return True if all IRIs have a time series attached, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for(String iri: iris) {
            try {
                try (Connection conn = rdbStoreClient.getConnection()) {
                    if (!tsClient.checkDataHasTimeSeries(iri, conn)) {
                        return false;
                    }
                }
                catch (SQLException e) {
                    throw new JPSRuntimeException(e);
                }
                // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return false;
                }
                else {
                    throw e;
                }
            }
        }
        return true;
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

    /**
     * Executes query on SPARQL endpoint and retrieves requested value of building
     * @param uriString city object id
     * @param value building value requested
     * @param route route to pass to access agent
     * @return geometry as string
     */
    private String getValue(String uriString, String value, String route)  {

        String result = "";

        GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(ontologyUriHelper);

        Query q = geometryQueryHelper.getQuery(uriString, value);

        //Use access agent
        JSONArray queryResultArray = this.queryStore(route, q.toString());

        if(!queryResultArray.isEmpty()){
            if (value.equals("Lod0FootprintId") || value.equals("FootprintThematicSurface")) {
                result = GeometryHelper.extractFootprint(queryResultArray);
            }
            else if (value.equals("FootprintSurfaceGeom")) {
                result = GeometryHelper.extractFootprint(GeometryHelper.getGroundGeometry(queryResultArray));
            }
            else{
                result = queryResultArray.getJSONObject(0).get(value).toString();
            }
        }
        return result;
    }

    /**
     * Queries for building geometry related information
     * @param uriString city object id
     * @param route route to pass to access agent
     * @param type type of building geometry related information to be queried
     * @return building geometry related information
     */
    private String getBuildingGeometry(String uriString, String route, String type) {
        String result;

        switch(type) {
            case "height":
                // Set default value of 10m if height can not be obtained from knowledge graph
                // Will only require one height query if height is represented in data consistently
                result = getValue(uriString, "HeightMeasuredHeigh", route);
                result = result.length() == 0 ? getValue(uriString, "HeightMeasuredHeight", route) : result;
                result = result.length() == 0 ? getValue(uriString, "HeightGenAttr", route) : result;
                result = result.length() == 0 ? "10.0" : result;
                
            case "footprint":
                // Get footprint from ground thematic surface or find from surface geometries depending on data
                result = getValue(uriString, "Lod0FootprintId", route);
                result = result.length() == 0 ? getValue(uriString, "FootprintThematicSurface", route) : result;
                result = result.length() == 0 ? getValue(uriString, "FootprintSurfaceGeom", route) : result;
            
            case "crs":
                result = getValue(uriString, "CRS", route);
                result = result.isEmpty() ? getValue(uriString, "DatabasesrsCRS", route) : result;

            default:
                result = "";
        }

        return result;
    }

    /**
     * Builds a SPARQL geospatial query for city object id of buildings whose envelope are within lowerBounds and upperBounds
     * @param uriString city object id of the target building
     * @param lowerBounds coordinates of customFieldsLowerBounds as a string
     * @param upperBounds coordinates of customFieldsUpperBounds as a string
     * @return returns a query string
     */
    private Query getBuildingsWithinBoundsQuery(String uriString, String lowerBounds, String upperBounds) throws ParseException {
        // where clause for geospatial search
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addWhere("?cityObject", "geo:predicate", "ocgml:EnvelopeType")
                .addWhere("?cityObject", "geo:searchDatatype", customDataType)
                .addWhere("?cityObject", "geo:customFields", customField)
                // PLACEHOLDER because lowerBounds and upperBounds would be otherwise added as doubles, not strings
                .addWhere("?cityObject", "geo:customFieldsLowerBounds", "PLACEHOLDER" + lowerBounds)
                .addWhere("?cityObject", "geo:customFieldsUpperBounds", "PLACEHOLDER" + upperBounds);

        // where clause to check that the city object is a building
        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addWhere("?cityObject", "ocgml:objectClassId", "?id")
                .addFilter("?id=26");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo) + "search", wb.build().getQueryPattern()));
        body.addElement(wb2.build().getQueryPattern());
        query.setQueryPattern(body);

        WhereHandler wh = new WhereHandler(query.cloneQuery());

        // add city object graph
        WhereHandler wh2 = new WhereHandler(sb.build());
        wh2.addGraph(NodeFactory.createURI(BuildingHelper.getGraph(uriString,CITY_OBJECT)), wh);

        return wh2.getQuery();
    }

    /**
     * Retrieves the surrounding buildings
     * @param uriString city object id
     * @param route route to pass to access agent
     * @param unique array list of unique surrounding buildings
     * @param surroundingCoordinates list of coordinates that form bounding box for surrounding query, used for terrain calculation
     * @return the surrounding buildings as an ArrayList of CEAInputData
     */
    private ArrayList<CEAInputData> getSurroundings(String uriString, String route, List<String> unique, List<Coordinate> surroundingCoordinates) {
        try {
            CEAInputData temp;
            String uri;
            ArrayList<CEAInputData> surroundings = new ArrayList<>();
            String envelopeCoordinates = getValue(uriString, "envelope", route);

            Double buffer = 100.00;

            Polygon envelopePolygon = (Polygon) GeometryHelper.toPolygon(envelopeCoordinates);

            Geometry boundingBoxGeometry = ((Polygon) GeometryHelper.inflatePolygon(envelopePolygon, buffer)).getExteriorRing();

            Coordinate[] boundingBoxCoordinates = boundingBoxGeometry.getCoordinates();

            String boundingBox = GeometryHelper.coordinatesToString(boundingBoxCoordinates);

            String[] points = boundingBox.split("#");

            String lowerPoints= points[0] + "#" + points[1] + "#" + 0 + "#";

            String lowerBounds = lowerPoints + lowerPoints + lowerPoints + lowerPoints + lowerPoints;
            lowerBounds = lowerBounds.substring(0, lowerBounds.length() - 1 );

            Double maxZ;

            if (points[8].equals("NaN")) {
                // highest elevation on Earth is 8848
                maxZ = 8850.0;
            }
            else{
                maxZ = Double.parseDouble(points[8])+200;
            }

            String upperPoints = points[6] + "#" + points[7] + "#" + maxZ + "#";

            String upperBounds = upperPoints + upperPoints + upperPoints + upperPoints + upperPoints;
            upperBounds = upperBounds.substring(0, upperBounds.length() - 1);

            Query query = getBuildingsWithinBoundsQuery(uriString, lowerBounds, upperBounds);

            String queryString = query.toString().replace("PLACEHOLDER", "");

            JSONArray queryResultArray = this.queryStore(route, queryString);

            for (int i = 0; i < queryResultArray.length(); i++) {
                uri = queryResultArray.getJSONObject(i).get("cityObject").toString();

                if (!unique.contains(uri)) {
                    String height = getBuildingGeometry(uri, route, "height");
                    String footprint = getBuildingGeometry(uri, route, "footprint");

                    temp = new CEAInputData(footprint, height, null, null, null, null, null);
                    unique.add(uri);
                    surroundings.add(temp);
                }
            }
            surroundingCoordinates.addAll(Arrays.asList(boundingBoxCoordinates));
            return surroundings;
        }
        catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Retrieves weather data
     * @param uriString city object id
     * @param route route to city object geometry data
     * @param weatherRoute route to weather data
     * @param crs CRS of city object geometry
     * @param result list to add the retrieved weather data to
     * @return true if weather data retrieved, false otherwise
     */
    private boolean getWeather(String uriString, String route, String weatherRoute, String crs, List<Object> result) {
        String envelopeCoordinates = getValue(uriString, "envelope", route);

        Polygon envelopePolygon = (Polygon) GeometryHelper.toPolygon(envelopeCoordinates);

        Double elevation = envelopePolygon.getCoordinate().getZ();

        Point center = envelopePolygon.getCentroid();

        Coordinate centerCoordinate = center.getCoordinate();

        crs = StringUtils.isNumeric(crs) ? "EPSG:" + crs : crs;

        try {
            // coordinate in (longitude, latitude) format
            Coordinate transformedCoordinate = GeometryHelper.transformCoordinate(centerCoordinate, crs, CRS_4326);

            // coordinate in (latitude, longitude) format
            Coordinate coordinate = new Coordinate(transformedCoordinate.getY(), transformedCoordinate.getX(), transformedCoordinate.getZ());

            String stationIRI = getWeatherStation(coordinate, 2.0, weatherRoute);

            // if no nearby weather station, send request to OpenMeteoAgent to instantiate weather data
            if (stationIRI.isEmpty()) {
                stationIRI = runOpenMeteoAgent(String.valueOf(coordinate.getX()), String.valueOf(coordinate.getY()), weatherRoute);

                // if request fails
                if (stationIRI.isEmpty()) {return false;}
            }

            Map<String, List<String>> weatherMap = getWeatherIRI(stationIRI, weatherRoute);

            List<Double> lat_lon = getStationCoordinate(stationIRI, weatherRoute);
            Double latitude;
            Double longitude;

            if (!lat_lon.isEmpty()) {
                latitude = lat_lon.get(0);
                longitude = lat_lon.get(1);
            }
            else {
                latitude = coordinate.getX();
                longitude = coordinate.getY();
            }

            // if the timestamps of the instantiated weather data does not meet CEA requirements,
            // send request to OpenMeteoAgent to update weather data with timestamps that meet CEA requirements
            if (!parseWeather(weatherMap, result, latitude, longitude)) {
                // if request fails
                if (runOpenMeteoAgent(String.valueOf(latitude), String.valueOf(longitude), weatherRoute).isEmpty()) {return false;}

                parseWeather(weatherMap, result, latitude, longitude);
            }

            String stationElevation = getStationElevation(stationIRI, weatherRoute);

            if (!stationElevation.isEmpty()) {
                elevation = Double.parseDouble(stationElevation);
            }

            List<OffsetDateTime> times = (List<OffsetDateTime>) result.get(0);

            ZoneOffset zoneOffset = times.get(0).getOffset();

            Integer offset = zoneOffset.getTotalSeconds();

            // store offset in hours
            result.add(Arrays.asList(latitude, longitude, elevation, offset / 60.0 / 60.0));

            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Send request to OpenMeteoAgent to instantiate historical weather data over a year
     * @param latitude latitude of the weather station
     * @param longitude longitude of the weather station
     * @param route route to instantiate weather data
     * @return the instantiated weather station IRI
     */
    public String runOpenMeteoAgent(String latitude, String longitude, String route) {
        String url = openmeteoagentUrl + OPENMETEO_ENDPOINT_RUN;

        JSONObject json = new JSONObject()
                .put(OPENMETEO_ROUTE, route)
                .put(OPENMETEO_LAT, latitude)
                .put(OPENMETEO_LON, longitude);

        DateTimeFormatter format = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate currentFirstDate = LocalDate.now().withMonth(1).withDayOfMonth(1);
        String startDate = currentFirstDate.minusYears(1).format(format);
        String endDate = currentFirstDate.format(format);

        json.put(OPENMETEO_START, startDate)
                .put(OPENMETEO_END, endDate);

        try {
            HttpResponse<String> response = Unirest.post(url)
                    .header(HTTP.CONTENT_TYPE, CTYPE_JSON)
                    .body(json.toString())
                    .socketTimeout(300000)
                    .asString();
            int responseStatus = response.getStatus();

            if (responseStatus == HttpURLConnection.HTTP_OK) {
                JSONObject responseBody = new JSONObject(response.getBody());

                return responseBody.getJSONArray(OPENMETEO_STATION).getString(0);
            } else {
                return "";
            }
        }
        catch (UnirestException e) {
            return "";
        }
    }

    /**
     * Queries for and returns the IRI of weather station located within {radius} kilometers of center
     * @param center center of the search circle
     * @param radius radius of the search circle
     * @param route endpoint of the weather station query
     * @return IRI of a weather station located within {radius} kilometers of center
     */
    private String getWeatherStation(Coordinate center, Double radius, String route) {
        String result = "";
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addPrefix("geoliteral", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geoliteral))
                .addPrefix("ontoems", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoems));

        wb.addWhere("?station", "geo:search", "inCircle")
                .addWhere("?station", "geo:searchDatatype", "geoliteral:lat-lon")
                .addWhere("?station", "geo:predicate", "ontoems:hasObservationLocation")
                // PLACEHOLDER because the coordinate will be treated as doubles instead of string otherwise
                .addWhere("?station", "geo:spatialCircleCenter", center.getX() + "PLACEHOLDER" + center.getY())
                .addWhere("?station", "geo:spatialCircleRadius", radius);

        SelectBuilder sb = new SelectBuilder()
                .addVar("?station");

        Query query = sb.build();

        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo) + "search", wb.build().getQueryPattern()));
        query.setQueryPattern(body);

        String queryString = query.toString().replace("PLACEHOLDER", "#");

        JSONArray queryResultArray = this.queryStore(route, queryString);

        if (!queryResultArray.isEmpty()) {
            result = queryResultArray.getJSONObject(0).getString("station");
        }

        return result;
    }

    /**
     * Queries for and returns the elevation of a weather station
     * @param stationIRI IRI of weather station
     * @param route endpoint of the weather station query
     * @return elevation of the weather station
     */
    private String getStationElevation(String stationIRI, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoEMS", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf));

        wb.addWhere(NodeFactory.createURI(stationIRI), "ontoEMS:hasObservationElevation", "?elevation");

        SelectBuilder sb = new SelectBuilder()
                .addWhere(wb);

        sb.addVar("?elevation");

        JSONArray queryResultArray = this.queryStore(route, sb.build().toString());

        if (!queryResultArray.isEmpty()) {
            return queryResultArray.getJSONObject(0).getString("elevation");
        }
        else{
            return "";
        }
    }

    /**
     * Queries for and returns the coordinate of a weather station
     * @param stationIRI IRI of weather station
     * @param route endpoint of the weather station query
     * @return coordinate of the weather station
     */
    private List<Double> getStationCoordinate(String stationIRI, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoEMS", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf));

        wb.addWhere(NodeFactory.createURI(stationIRI), "ontoEMS:hasObservationLocation", "?coordinate");

        SelectBuilder sb = new SelectBuilder()
                .addWhere(wb);

        sb.addVar("?coordinate");

        JSONArray queryResultArray = this.queryStore(route, sb.build().toString());

        if (!queryResultArray.isEmpty()) {
            String coordinate = queryResultArray.getJSONObject(0).getString("coordinate");
            String[] split = coordinate.split("#");
            List<Double> result = new ArrayList<>();
            result.add(Double.valueOf(split[0]));
            result.add(Double.valueOf(split[1]));
            return result;
        }
        else{
            return new ArrayList<>();
        }
    }

    /**
     * Returns UTC offset of the timestamps of the retrieved weather data
     * @param latitude latitude of the station of the retrieved weather data
     * @param longitude longitude of the station of the retrieved weather data
     * @param startDate start date of the retrieved historical weather data as an Instant object
     * @param endDate end date of the retrieved historical weather data as an Instant object
     * @return UTC offset of the timestamps of the retrieved historical weather data in seconds
     */
    public Double getStationOffset(Double latitude, Double longitude, Instant startDate, Instant endDate) {
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

            ZoneId localZone = ZoneId.systemDefault();
            LocalDate localStart = startDate.atZone(localZone).toLocalDate();
            LocalDate localEnd = endDate.atZone(localZone).toLocalDate();

            String start = formatter.format(localStart);
            String end = formatter.format(localEnd);

            String query = API_LAT + "=" + latitude + "&";
            query = query + API_LON + "=" + longitude + "&";
            query = query + API_START + "=" + start + "&";
            query = query + API_END + "=" + end + "&" + API_TIMEZONE + "=auto";


            URLConnection connection = new URL(API_URL + "?" + query).openConnection();

            InputStream is = connection.getInputStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(is));
            StringBuffer response = new StringBuffer();
            String line;
            while ((line = rd.readLine()) != null && !line.isEmpty()){
                response.append(line);
            }
            rd.close();
            JSONObject result = new JSONObject(response.toString());

            // return offset in seconds
            return result.getDouble(API_OFFSET);
        }
        catch (IOException e){
            // if failed to get the offset from API, return an approximation based on the longitude
            // approximation assumes Earth is rough divided into 24 time zones equally over the globe
            return longitude / 15;
        }
    }

    /**
     * Queries for and returns the weather data IRI, their weather data type, and the time series database URL of the corresponding weather time series
     * @param stationIRI IRI of weather station
     * @param route endpoint for weather query
     * @return map with weather data type as key, weather data IRI and time series database URL as a list for the map value
     */
    private Map<String, List<String>> getWeatherIRI(String stationIRI, String route) {
        Map<String, List<String>> result = new HashMap<>();
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoems", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("ontotimeseries", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontotimeseries));

        wb.addWhere("?station", "ontoems:reports", "?quantity")
                .addWhere("?quantity", "rdf:type", "?weatherParameter")
                .addWhere("?quantity", "om:hasValue", "?measure")
                .addWhere("?measure", "ontotimeseries:hasTimeSeries", "?timeseries")
                .addWhere("?timeseries", "ontotimeseries:hasRDB", "?rdb");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?weatherParameter")
                .addVar("?measure")
                .addVar("?rdb");

        sb.addWhere(wb);

        sb.setVar(Var.alloc( "station" ), NodeFactory.createURI(stationIRI));

        JSONArray queryResultArray = this.queryStore(route, sb.build().toString());

        if (!queryResultArray.isEmpty()) {
            for (int i = 0; i < queryResultArray.length(); i++) {
                result.put(queryResultArray.getJSONObject(i).getString("weatherParameter").split(ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoems))[1], Arrays.asList(queryResultArray.getJSONObject(i).getString("measure"), queryResultArray.getJSONObject(i).getString("rdb")));
            }
        }

        return result;
    }

    /**
     * Parses weather data into a list
     * @param weatherMap map with the weather parameter IRIs
     * @param result empty list to add the parsed weather data
     * @return true if the timestamps of weather data meet CEA requirements (result will contain the parsed data), false otherwise (result will be empty)
     */
    private boolean parseWeather(Map<String, List<String>> weatherMap, List<Object> result, Double latitude, Double longitude) {
        Map<String, List<Double>> weather = new HashMap<>();

        boolean getTimes = true;

        for (Map.Entry<String, List<String>> entry : weatherMap.entrySet()) {
            List<String> value = entry.getValue();
            String weatherIRI = value.get(0);
            Map<String, Object> weatherClients = getWeatherClients(value.get(1));
            TimeSeries<Instant> weatherTS = retrieveData(weatherIRI, (RemoteStoreClient) weatherClients.get(STORE_CLIENT), (RemoteRDBStoreClient) weatherClients.get(RDB_CLIENT), Instant.class);

            // want hourly data over a year
            if (!validateWeatherTimes(weatherTS.getTimes(), latitude, longitude)) {
                result.clear();
                return false;
            }

            if (getTimes) {
                List<Instant> times = weatherTS.getTimes().subList(0, 8760);
                Double offset = getStationOffset(latitude, longitude, times.get(0), times.get(times.size()-1));
                // parse times to OffsetDateTime with the correct offset
                List<OffsetDateTime> weatherTimes = parseWeatherTimes(times, offset.intValue());

                result.add(weatherTimes);
                getTimes = false;
            }

            weather.put(entry.getKey(),  weatherTS.getValuesAsDouble(weatherIRI).subList(0, 8760));
        }

        result.add(weather);
        return true;
    }

    /**
     * Parses timestamps of weather data into list of OffsetDateTimes
     * @param weatherTimes timestamps of weather data
     * @param offset UTC offset in seconds
     * @return  timestamps of weather data as a list of OffsetDateTimes
     */
    private List<OffsetDateTime> parseWeatherTimes(List<Instant> weatherTimes, Integer offset) {
        List<OffsetDateTime> result = new ArrayList<>();

        ZoneOffset zoneOffset = ZoneOffset.ofTotalSeconds(offset);

        for (int i = 0; i < weatherTimes.size(); i++) {
            result.add(weatherTimes.get(i).atOffset(zoneOffset));
        }

        return result;
    }

    /**
     * Validates whether the weather data meet the requirements for CEA
     * The requirements are, the number of data entries must be at least 8760, and the start date must be the first day of the year
     * @param weatherTimes list of timestamps
     * @param latitude latitude of the station of the retrieved weather data
     * @param longitude longitude of the station of the retrieved weather data
     * @return true if weatherTimes meet CEA requirements, false otherwise
     */
    private boolean validateWeatherTimes(List<Instant> weatherTimes, Double latitude, Double longitude) {
        if (weatherTimes.size() < 8760) {
            return false;
        }

        Double offset = getStationOffset(latitude, longitude, weatherTimes.get(0), weatherTimes.get(weatherTimes.size()-1));

        ZoneOffset zoneOffset = ZoneOffset.ofTotalSeconds(offset.intValue());

        OffsetDateTime startDate = weatherTimes.get(0).atOffset(zoneOffset);

        if (startDate.getMonthValue() != 1 || startDate.getDayOfMonth() != 1) {
            return false;
        }

        return true;
    }

    /**
     * Gets the RemoteStoreClient and RemoteRDBStoreClient objects for querying weather data
     * @param weatherDB database storing the weather data
     * @return a map containing a RemoteStoreClient object and RemoteRDBStoreClient object that will allow for the querying of weather data
     */
    public Map<String, Object> getWeatherClients(String weatherDB) {
        RemoteRDBStoreClient weatherRDBClient = new RemoteRDBStoreClient(weatherDB, dbUser, dbPassword);
        List<String> weatherEndpoints = RouteHelper.getRouteEndpoints(weatherRoute);
        RemoteStoreClient weatherStoreClient = new RemoteStoreClient(weatherEndpoints.get(0), weatherEndpoints.get(1));
        Map<String, Object> result = new HashMap<>();
        result.put(RDB_CLIENT, weatherRDBClient);
        result.put(STORE_CLIENT, weatherStoreClient);
        return result;
    }

    /**
     * Gets terrain data for city object
     * @param uriString city object id
     * @param route route to city object geometry data
     * @param crs coordinate reference system used by route
     * @param surroundingCoordinates  coordinates that formed the bounding box for surrounding query
     * @param database PostGIS database name
     * @param table PostGIS table name
     * @return terrain data as byte[]
     */
    private byte[] getTerrain(String uriString, String route, String crs, List<Coordinate> surroundingCoordinates, String database, String table) {
        String dbUrl = endpointConfig.getDbUrl(database);
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

        // query for the coordinate reference system used by the terrain data
        String sridQuery = String.format("SELECT ST_SRID(rast) as srid FROM public.%s LIMIT 1", table);

        Coordinate centerCoordinate;

        Double radius;

        if (!surroundingCoordinates.isEmpty()) {
            Envelope envelope = new Envelope();

            for (Coordinate coordinate : surroundingCoordinates) {
                envelope.expandToInclude(coordinate);
            }
            centerCoordinate = envelope.centre();

            Double w = envelope.getWidth();
            Double h = envelope.getHeight();

            radius = w > h ? w/2 : h/2;

            radius += 30;
        }
        else {
            String envelopeCoordinates = getValue(uriString, "envelope", route);

            Polygon envelopePolygon = (Polygon) GeometryHelper.toPolygon(envelopeCoordinates);

            Point center = envelopePolygon.getCentroid();

            centerCoordinate = center.getCoordinate();

            radius = 160.0;
        }

        crs = StringUtils.isNumeric(crs) ? "EPSG:" + crs : crs;

        List<byte[]> result = new ArrayList<>();

        try {
            JSONArray sridResult = postgisClient.executeQuery(sridQuery);

            if (sridResult.isEmpty()) {return null;}
            Integer postgisCRS = sridResult.getJSONObject(0).getInt("srid");

            Coordinate coordinate = GeometryHelper.transformCoordinate(centerCoordinate, crs, "EPSG:" + postgisCRS);

            // query for terrain data
            String terrainQuery = getTerrainQuery(coordinate.getX(), coordinate.getY(), radius, postgisCRS, table);

            try (Connection conn = postgisClient.getConnection()) {
                Statement stmt = conn.createStatement();
                ResultSet terrainResult = stmt.executeQuery(terrainQuery);
                while(terrainResult.next()) {
                    byte[] rasterBytes = terrainResult.getBytes("data");
                    result.add(rasterBytes);
                }
            }

            if (result.size() == 1) {
                return result.get(0);
            }
            else{
                return null;
            }
        }
        catch (Exception e) {
            return null;
        }
    }

    /**
     * Creates a SQL query string for raster data within a square bounding box of length 2*radius, center point at (x, y)
     * @param x first coordinate of center point
     * @param y second coordinate of center point
     * @param radius length of the square bounding box divided by 2
     * @param postgisCRS coordinate reference system of the raster data queried
     * @param table table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(Double x, Double y, Double radius, Integer postgisCRS, String table) {
        // SQL commands for creating a square bounding box
        String terrainBoundary = String.format("ST_Expand(ST_SetSRID(ST_MakePoint(%f, %f), %d), %f)", x, y, postgisCRS, radius);

        // query result to be converted to TIF format
        String query = String.format("SELECT ST_AsTIFF(ST_Union(ST_Clip(rast, %s))) as data ", terrainBoundary);
        query = query + String.format("FROM public.%s ", table);
        query = query + String.format("WHERE ST_Intersects(rast, %s);", terrainBoundary);

        return query;
    }

    /**
     * Add where for Building Consumption
     * @param builder update builder
     * @param type energy type in ontology
     */
    public void addBuildingConsumptionWhere(WhereBuilder builder, String type){
        builder.addWhere("?building", "ontoubemmp:consumesEnergy", "?grid")
                .addWhere("?grid", "rdf:type", type)
                .addWhere("?grid", "om:hasValue", "?measure")
                .addWhere("?measure", "om:hasUnit", "?unit");
    }

    /**
     * Add where for Device Supply
     * @param builder update builder
     * @param generatorType type of generator
     * @param energyType type of energy supply
     * @param facadeType  type of facade that the generator is theoretically installed on
     */
    public void addSupplyDeviceWhere(WhereBuilder builder, String generatorType, String energyType, String facadeType) {
        builder.addWhere("?building", "obs:hasFacade", "?facade")
                .addWhere("?facade", "rdf:type", facadeType)
                .addWhere("?facade", "ontoubemmp:hasTheoreticalEnergyProduction", "?SolarGenerators")
                .addWhere("?SolarGenerators", "rdf:type", generatorType)
                .addWhere("?SolarGenerators", "ontoubemmp:producesEnergy", "?supply")
                .addWhere("?supply", "rdf:type", energyType)
                .addWhere("?supply", "om:hasValue", "?measure")
                .addWhere("?measure", "om:hasUnit", "?unit");
    }

    /**
     * Add where for Device Area
     * @param builder update builder
     * @param building iri of building
     * @param facadeType type of facade
     */
    public void addSupplyDeviceAreaWhere(WhereBuilder builder, String building, String facadeType) {
        builder.addWhere(NodeFactory.createURI(building), "obs:hasFacade" , "?facade")
                .addWhere("?facade", "rdf:type", facadeType)
                .addWhere("?facade", "ontoubemmp:hasSolarSuitableArea", "?area")
                .addWhere("?area", "om:hasValue", "?measure")
                .addWhere("?measure", "om:hasNumericalValue", "?value")
                .addWhere("?measure", "om:hasUnit", "?unit");
    }

    /**
     * Retrieves iris from KG for the data type requested
     * @param building uri of building in energyprofile graph
     * @param value type of data from TIME_SERIES or SCALARS
     * @param route route to pass to access agent
     * @return list of iris
     */
    public ArrayList<String> getDataIRI(String building, String value, String route) {
        ArrayList<String> result = new ArrayList<>();

        SelectBuilder sb = new SelectBuilder();
        WhereBuilder wb = new WhereBuilder();

        if(building.equals("")) {
            return result;
        }

        wb.addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("ontoubemmp", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("obs", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltstructure));

        switch(value) {
            case KEY_ROOF_SOLAR_SUITABLE_AREA:
                addSupplyDeviceAreaWhere(wb, building, "obs:RoofFacade");
                break;
            case KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA:
                addSupplyDeviceAreaWhere(wb, building, "obs:SouthWallFacade");
                break;
            case KEY_NORTH_WALL_SOLAR_SUITABLE_AREA:
                addSupplyDeviceAreaWhere(wb, building, "obs:NorthWallFacade");
                break;
            case KEY_EAST_WALL_SOLAR_SUITABLE_AREA:
                addSupplyDeviceAreaWhere(wb, building, "obs:EastWallFacade");
                break;
            case KEY_WEST_WALL_SOLAR_SUITABLE_AREA:
                addSupplyDeviceAreaWhere(wb, building, "obs:WestWallFacade");
                break;
            case KEY_GRID_CONSUMPTION:
                addBuildingConsumptionWhere(wb,"ontoubemmp:GridConsumption");
                break;
            case KEY_ELECTRICITY_CONSUMPTION:
                addBuildingConsumptionWhere(wb,"ontoubemmp:ElectricityConsumption");
                break;
            case KEY_HEATING_CONSUMPTION:
                addBuildingConsumptionWhere(wb,"ontoubemmp:HeatingConsumption");
                break;
            case KEY_COOLING_CONSUMPTION:
                addBuildingConsumptionWhere(wb,"ontoubemmp:CoolingConsumption");
                break;
            case KEY_PV_ROOF_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVPanel", "ontoubemmp:ElectricitySupply", "obs:RoofFacade");
                break;
            case KEY_PV_WALL_SOUTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVPanel", "ontoubemmp:ElectricitySupply", "obs:SouthWallFacade");
                break;
            case KEY_PV_WALL_NORTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVPanel", "ontoubemmp:ElectricitySupply", "obs:NorthWallFacade");
                break;
            case KEY_PV_WALL_EAST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVPanel", "ontoubemmp:ElectricitySupply", "obs:EastWallFacade");
                break;
            case KEY_PV_WALL_WEST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVPanel", "ontoubemmp:ElectricitySupply", "obs:WestWallFacade");
                break;
            case KEY_PVT_PLATE_ROOF_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:ElectricitySupply", "obs:RoofFacade");
                break;
            case KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:ElectricitySupply", "obs:SouthWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:ElectricitySupply", "obs:NorthWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_EAST_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:ElectricitySupply", "obs:EastWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_WEST_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:ElectricitySupply", "obs:WestWallFacade");
                break;
            case KEY_PVT_PLATE_ROOF_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:HeatSupply", "obs:RoofFacade");
                break;
            case KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:HeatSupply", "obs:SouthWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:HeatSupply", "obs:NorthWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:HeatSupply", "obs:EastWallFacade");
                break;
            case KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTPlateCollector", "ontoubemmp:HeatSupply", "obs:WestWallFacade");
                break;
            case KEY_PVT_TUBE_ROOF_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:ElectricitySupply", "obs:RoofFacade");
                break;
            case KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:ElectricitySupply", "obs:SouthWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:ElectricitySupply", "obs:NorthWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_EAST_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:ElectricitySupply", "obs:EastWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_WEST_E_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:ElectricitySupply", "obs:WestWallFacade");
                break;
            case KEY_PVT_TUBE_ROOF_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:HeatSupply", "obs:RoofFacade");
                break;
            case KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:HeatSupply", "obs:SouthWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:HeatSupply", "obs:NorthWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:HeatSupply", "obs:EastWallFacade");
                break;
            case KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:PVTTubeCollector", "ontoubemmp:HeatSupply", "obs:WestWallFacade");
                break;
            case KEY_THERMAL_PLATE_ROOF_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalPlateCollector", "ontoubemmp:HeatSupply", "obs:RoofFacade");
                break;
            case KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalPlateCollector", "ontoubemmp:HeatSupply", "obs:SouthWallFacade");
                break;
            case KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalPlateCollector", "ontoubemmp:HeatSupply", "obs:NorthWallFacade");
                break;
            case KEY_THERMAL_PLATE_WALL_EAST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalPlateCollector", "ontoubemmp:HeatSupply", "obs:EastWallFacade");
                break;
            case KEY_THERMAL_PLATE_WALL_WEST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalPlateCollector", "ontoubemmp:HeatSupply", "obs:WestWallFacade");
                break;
            case KEY_THERMAL_TUBE_ROOF_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalTubeCollector", "ontoubemmp:HeatSupply", "obs:RoofFacade");
                break;
            case KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalTubeCollector", "ontoubemmp:HeatSupply", "obs:SouthWallFacade");
                break;
            case KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalTubeCollector", "ontoubemmp:HeatSupply", "obs:NorthWallFacade");
                break;
            case KEY_THERMAL_TUBE_WALL_EAST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalTubeCollector", "ontoubemmp:HeatSupply", "obs:EastWallFacade");
                break;
            case KEY_THERMAL_TUBE_WALL_WEST_SUPPLY:
                addSupplyDeviceWhere(wb,"ontoubemmp:ThermalTubeCollector", "ontoubemmp:HeatSupply", "obs:WestWallFacade");
                break;
            default:
                return result;
        }

        sb.addVar("?measure")
                .addVar("?unit");

        sb.addWhere(wb);

        sb.setVar( Var.alloc( "building" ), NodeFactory.createURI(building));

        JSONArray queryResultArray = new JSONArray(this.queryStore(route, sb.build().toString()));

        if(!queryResultArray.isEmpty()){
            result.add(queryResultArray.getJSONObject(0).get("measure").toString());
            result.add(queryResultArray.getJSONObject(0).get("unit").toString());
        }
        return result;
    }

    /**
     * Gets numerical value of specified measurement
     * @param measureUri Uri of the measurement with numerical value in KG
     * @param route route to pass to access agent
     * @param graph graph name
     * @return list of iris
     */
    public String getNumericalValue(String measureUri, String route, String graph){
        String result = "";

        WhereBuilder wb = new WhereBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
            .addWhere("?measure", "om:hasNumericalValue", "?value");

        SelectBuilder sb = new SelectBuilder().addVar("?value");

        if (!graph.isEmpty()){
            sb.addGraph(NodeFactory.createURI(graph), wb);
        }
        else{
            sb.addWhere(wb);
        }

        sb.setVar( Var.alloc( "measure" ), NodeFactory.createURI(measureUri));

        JSONArray queryResultArray = new JSONArray(this.queryStore(route, sb.build().toString()));

        if(!queryResultArray.isEmpty()){
            result = queryResultArray.getJSONObject(0).get("value").toString();
        }
        return result;
    }

    /**
     * Checks building linked to ontoCityGML is initialised in KG and is a bot:Building instance
     * @param uriString city object id
     * @param route route to pass to access agent
     * @return building
     */
    public String checkBuildingInitialised(String uriString, String route){
        WhereBuilder wb = new WhereBuilder();
        SelectBuilder sb = new SelectBuilder();

        wb.addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("ontoBuiltEnv", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                .addWhere("?building", "ontoBuiltEnv:hasOntoCityGMLRepresentation", "?s")
                .addWhere("?building", "rdf:type", "bot:Building");

        sb.addVar("?building").addWhere(wb);

        sb.setVar( Var.alloc( "s" ), NodeFactory.createURI(BuildingHelper.getBuildingUri(uriString)));

        JSONArray queryResultArray = new JSONArray(this.queryStore(route, sb.build().toString()));
        String building = "";
        if(!queryResultArray.isEmpty()){
            building = queryResultArray.getJSONObject(0).get("building").toString();
        }
        return building;
    }

    /**
     * Initialises building in KG with buildingUri as the bot:Building IRI, and link to ontoCityGMLRepresentation
     * @param uriString city object id
     * @param buildingUri building IRI from other endpoints if exist
     * @param route route to pass to access agent
     * @param graph graph name
     * @return building
     */
    public String initialiseBuilding(String uriString, String buildingUri, String route, String graph){

        UpdateBuilder ub = new UpdateBuilder();

        if (buildingUri.isEmpty()) {
            if (!graph.isEmpty()) {
                buildingUri = graph + "Building_" + UUID.randomUUID() + "/";
            }
            else{
                buildingUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv) + "Building_" + UUID.randomUUID() + "/";
            }
        }

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", ontologyUriHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                        .addPrefix("ontoBuiltEnv", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "bot:Building")
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "owl:NamedIndividual")
                        .addWhere(NodeFactory.createURI(buildingUri), "ontoBuiltEnv:hasOntoCityGMLRepresentation", NodeFactory.createURI(BuildingHelper.getBuildingUri(uriString)));

        if (!graph.isEmpty()){
            ub.addInsert(NodeFactory.createURI(graph), wb);
        }
        else{
            ub.addInsert(wb);
        }

        UpdateRequest ur = ub.buildRequest();

        //Use access agent
        this.updateStore(route, ur.toString());

        return buildingUri;
    }

    /**
     * Checks if energy profile data already exist in KG and get IRIs if they do
     * @param building building uri in energy profile graph
     * @param tsIris map of time series iris to data types
     * @param scalarIris map of iris in kg to data type
     * @param route route to pass to access agent
     * @param graph graph name
     * @return if time series are initialised
     */
    public Boolean checkDataInitialised(String building, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route, String graph) {
        ArrayList<String> result;
        List<String> allMeasures = new ArrayList<>();
        Stream.of(TIME_SERIES, SCALARS).forEach(allMeasures::addAll);
        for (String measurement: allMeasures) {
            result = getDataIRI(building, measurement, route);
            if (!result.isEmpty()) {
                if (TIME_SERIES.contains(measurement)) {
                    tsIris.put(measurement, result.get(0));
                } else {
                    scalarIris.put(measurement, result.get(0));
                }
            } else {
                return false;
            }
        }
        return true;
    }

    /**
     * Creates updates for building facades
     * @param builder update builder
     * @param building building iri
     * @param facade facade iri
     * @param facadeType type of facade
     */
    public void createFacadeUpdate(WhereBuilder builder, String building, String facade, String facadeType) {
        builder.addWhere(NodeFactory.createURI(building), "obs:hasFacade", NodeFactory.createURI(facade))
                .addWhere(NodeFactory.createURI(facade), "rdf:type", facadeType);
    }

    /**
     * Creates update for energy consumption
     * @param builder update builder
     * @param consumer iri of building/device
     * @param consumptionType type in ontology
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     */
    public void createConsumptionUpdate(WhereBuilder builder, String consumer, String consumptionType, String quantity, String measure){
        builder.addWhere(NodeFactory.createURI(quantity), "rdf:type", consumptionType)
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:energy-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour")
                .addWhere(NodeFactory.createURI(consumer), "ontoubemmp:consumesEnergy",NodeFactory.createURI(quantity));
    }

    /**
     * Creates update for solar energy generators supply
     * @param builder update builder
     * @param facade facade iri
     * @param solarGenerator solar energy generator iri
     * @param solarGeneratorType type of solar energy generator
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     */
    public void createSolarGeneratorSupplyUpdate(WhereBuilder builder, String facade, String solarGenerator, String solarGeneratorType, String quantity, String measure, String energySupply){
        builder.addWhere(NodeFactory.createURI(facade), "ontoubemmp:hasTheoreticalEnergyProduction", NodeFactory.createURI(solarGenerator))
                .addWhere(NodeFactory.createURI(solarGenerator), "rdf:type", solarGeneratorType)
                .addWhere(NodeFactory.createURI(solarGenerator), "ontoubemmp:producesEnergy", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", energySupply)
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:energy-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour");
    }

    /**
     * Creates update for solar suitable areas
     * @param builder update builder
     * @param facade obs:facade iri
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     * @param value numerical value
     */
    public void createSolarSuitableAreaUpdate(WhereBuilder builder, String facade, String quantity, String measure, String value) {
        builder.addWhere(NodeFactory.createURI(facade), "ontoubemmp:hasSolarSuitableArea", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "om:Area")
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:area-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "om:hasNumericalValue", value)
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:squareMetre");
    }

    /**
     * Initialises energy profile data in KG
     * @param uriCounter keep track of uris
     * @param scalars map of scalar measurements
     * @param buildingUri building uri
     * @param tsIris map of time series iris to data types
     * @param scalarIris map of iris in kg to data types
     * @param route route to pass to access agent
     * @param graph graph name
     */
    public void initialiseData(Integer uriCounter, LinkedHashMap<String, List<String>> scalars, String buildingUri, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route, String graph){

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("ontoubemmp", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                        .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", ontologyUriHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                        .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                        .addPrefix("obs", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltstructure));

        UpdateBuilder ub = new UpdateBuilder();

        //Device uris
        String pvRoofPanelUri = "PVRoofPanel_" + UUID.randomUUID() + "/";
        String pvWallSouthPanelUri = "PVWallSouthPanel_" + UUID.randomUUID() + "/";
        String pvWallNorthPanelUri = "PVWallNorthPanel_" + UUID.randomUUID() + "/";
        String pvWallEastPanelUri = "PVWallEastPanel_" + UUID.randomUUID() + "/";
        String pvWallWestPanelUri = "PVWallWestPanel_" + UUID.randomUUID() + "/";
        String pvtPlateRoofCollectorUri = "PVTPlateRoofCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallSouthCollectorUri = "PVTPlateWallSouthCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallNorthCollectorUri = "PVTPlateWallNorthCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallEastCollectorUri = "PVTPlateWallEastCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallWestCollectorUri = "PVTPlateWallWestCollector_" + UUID.randomUUID() + "/";
        String pvtTubeRoofCollectorUri = "PVTTubeRoofCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallSouthCollectorUri = "PVTTubeWallSouthCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallNorthCollectorUri = "PVTTubeWallNorthCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallEastCollectorUri = "PVTTubeWallEastCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallWestCollectorUri = "PVTTubeWallWestCollector_" + UUID.randomUUID() + "/";
        String thermalPlateRoofCollectorUri = "ThermalPlateRoofCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallSouthCollectorUri = "ThermalPlateWallSouthCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallNorthCollectorUri = "ThermalPlateWallNorthCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallEastCollectorUri = "ThermalPlateWallEastCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallWestCollectorUri = "ThermalPlateWallWestCollector_" + UUID.randomUUID() + "/";
        String thermalTubeRoofCollectorUri = "ThermalTubeRoofCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallSouthCollectorUri = "ThermalTubeWallSouthCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallNorthCollectorUri = "ThermalTubeWallNorthCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallEastCollectorUri = "ThermalTubeWallEastCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallWestCollectorUri = "ThermalTubeWallWestCollector_" + UUID.randomUUID() + "/";


        if (!graph.isEmpty()){
            pvRoofPanelUri = graph + pvRoofPanelUri;
            pvWallSouthPanelUri = graph + pvWallSouthPanelUri;
            pvWallNorthPanelUri = graph + pvWallNorthPanelUri;
            pvWallEastPanelUri = graph + pvWallEastPanelUri;
            pvWallWestPanelUri = graph + pvWallWestPanelUri;
            pvtPlateRoofCollectorUri = graph + pvtPlateRoofCollectorUri;
            pvtPlateWallSouthCollectorUri = graph + pvtPlateWallSouthCollectorUri;
            pvtPlateWallNorthCollectorUri = graph + pvtPlateWallNorthCollectorUri;
            pvtPlateWallEastCollectorUri = graph + pvtPlateWallEastCollectorUri;
            pvtPlateWallWestCollectorUri = graph + pvtPlateWallWestCollectorUri;
            pvtTubeRoofCollectorUri = graph + pvtTubeRoofCollectorUri;
            pvtTubeWallSouthCollectorUri = graph + pvtTubeWallSouthCollectorUri;
            pvtTubeWallNorthCollectorUri = graph + pvtTubeWallNorthCollectorUri;
            pvtTubeWallEastCollectorUri = graph + pvtTubeWallEastCollectorUri;
            pvtTubeWallWestCollectorUri = graph + pvtTubeWallWestCollectorUri;
            thermalPlateRoofCollectorUri = graph + thermalPlateRoofCollectorUri;
            thermalPlateWallSouthCollectorUri = graph + thermalPlateWallSouthCollectorUri;
            thermalPlateWallNorthCollectorUri = graph + thermalPlateWallNorthCollectorUri;
            thermalPlateWallEastCollectorUri = graph + thermalPlateWallEastCollectorUri;
            thermalPlateWallWestCollectorUri = graph + thermalPlateWallWestCollectorUri;
            thermalTubeRoofCollectorUri = graph + thermalTubeRoofCollectorUri;
            thermalTubeWallSouthCollectorUri = graph + thermalTubeWallSouthCollectorUri;
            thermalTubeWallNorthCollectorUri = graph + thermalTubeWallNorthCollectorUri;
            thermalTubeWallEastCollectorUri = graph + thermalTubeWallEastCollectorUri;
            thermalTubeWallWestCollectorUri = graph + thermalTubeWallWestCollectorUri;
        }
        else{
            pvRoofPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvRoofPanelUri;
            pvWallSouthPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallSouthPanelUri;
            pvWallNorthPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallNorthPanelUri;
            pvWallEastPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallEastPanelUri;
            pvWallWestPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallWestPanelUri;
            pvtPlateRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateRoofCollectorUri;
            pvtPlateWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallSouthCollectorUri;
            pvtPlateWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallNorthCollectorUri;
            pvtPlateWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallEastCollectorUri;
            pvtPlateWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallWestCollectorUri;
            pvtTubeRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeRoofCollectorUri;
            pvtTubeWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallSouthCollectorUri;
            pvtTubeWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallNorthCollectorUri;
            pvtTubeWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallEastCollectorUri;
            pvtTubeWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallWestCollectorUri;
            thermalPlateRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateRoofCollectorUri;
            thermalPlateWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallSouthCollectorUri;
            thermalPlateWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallNorthCollectorUri;
            thermalPlateWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallEastCollectorUri;
            thermalPlateWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallWestCollectorUri;
            thermalTubeRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeRoofCollectorUri;
            thermalTubeWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallSouthCollectorUri;
            thermalTubeWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallNorthCollectorUri;
            thermalTubeWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallEastCollectorUri;
            thermalTubeWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallWestCollectorUri;
        }

        Map<String, String> facades = new HashMap<>();

        // save om:Measure uris for scalars and create om:Quantity uris for scalars and time series
        // (time series om:Measure iris already created in createTimeSeries)
        for (String measurement: SCALARS) {
            String measure = measurement + UUID.randomUUID() + "/";
            String quantity = measurement + "Quantity_" + UUID.randomUUID() + "/";
            String facade = measurement.split("SolarSuitableArea")[0] + UUID.randomUUID() + "/";
            if (!graph.isEmpty()){
                measure = graph + measure;
                quantity = graph + quantity;
                facade = graph + facade;
            }
            else{
                measure = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + measure;
                quantity = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
                facade = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + facade;
            }
            scalarIris.put(measurement, measure);

            switch(measurement){
                case(KEY_ROOF_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:RoofFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(KEY_ROOF_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("Roof", facade);
                    break;
                case(KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:SouthWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("SouthWall", facade);
                    break;
                case(KEY_NORTH_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:NorthWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(KEY_NORTH_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("NorthWall", facade);
                    break;
                case(KEY_EAST_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:EastWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(KEY_EAST_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("EastWall", facade);
                    break;
                case(KEY_WEST_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:WestWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(KEY_WEST_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("WestWall", facade);
                    break;
            }
        }

        for (String measurement: TIME_SERIES) {
            String quantity = measurement+"Quantity_" + UUID.randomUUID() + "/";
            quantity = !graph.isEmpty() ? graph + quantity : ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
            if (measurement.equals(KEY_GRID_CONSUMPTION) || measurement.equals(KEY_ELECTRICITY_CONSUMPTION) || measurement.equals(KEY_COOLING_CONSUMPTION) || measurement.equals(KEY_HEATING_CONSUMPTION)) {
                createConsumptionUpdate(wb, buildingUri, "ontoubemmp:" + measurement, quantity, tsIris.get(measurement));
            }
            else if (measurement.equals(KEY_PV_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvRoofPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PV_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvWallSouthPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PV_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvWallNorthPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PV_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvWallEastPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PV_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvWallWestPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_ROOF_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtPlateRoofCollectorUri, quantity, "ontoubemmp:PVTPlateCollector", tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtPlateWallSouthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtPlateWallNorthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_EAST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtPlateWallEastCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_WEST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtPlateWallWestCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_ROOF_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtPlateRoofCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtPlateWallSouthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtPlateWallNorthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtPlateWallEastCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtPlateWallWestCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_ROOF_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtTubeRoofCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtTubeWallSouthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtTubeWallNorthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_EAST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtTubeWallEastCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_WEST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtTubeWallWestCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_ROOF_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtTubeRoofCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtTubeWallSouthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtTubeWallNorthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtTubeWallEastCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtTubeWallWestCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_PLATE_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), thermalPlateRoofCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), thermalPlateWallSouthCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), thermalPlateWallNorthCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_PLATE_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), thermalPlateWallEastCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_PLATE_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), thermalPlateWallWestCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_TUBE_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), thermalTubeRoofCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), thermalTubeWallSouthCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), thermalTubeWallNorthCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_TUBE_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), thermalTubeWallEastCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(KEY_THERMAL_TUBE_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), thermalTubeWallWestCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
        }

        if (graph.isEmpty()){
            ub.addInsert(wb);
        }
        else{
            ub.addInsert(NodeFactory.createURI(graph), wb);
        }

        UpdateRequest ur = ub.buildRequest();

        //Use access agent
        this.updateStore(route, ur.toString());
    }

    /**
     * Updates numerical value of scalars in KG
     * @param scalars map of scalar measurements
     * @param scalarIris map of iris in kg to data types
     * @param route route to pass to access agent
     * @param uriCounter keep track of uris
     * @param graph graph name
     */
    public void updateScalars(String route, LinkedHashMap<String,String> scalarIris, LinkedHashMap<String, List<String>> scalars, Integer uriCounter, String graph) {

        for (String measurement: SCALARS) {
            WhereBuilder wb1 = new WhereBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", "?s");
            UpdateBuilder ub1 = new UpdateBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(wb1);

            WhereBuilder wb2 = new WhereBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", scalars.get(measurement).get(uriCounter));
            UpdateBuilder ub2 = new UpdateBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology));

            if (!graph.isEmpty()){
                ub1.addDelete(NodeFactory.createURI(graph), wb1);
                ub2.addInsert(NodeFactory.createURI(graph), wb2);
            }
            else{
                ub1.addDelete(wb1);
                ub2.addInsert(wb2);
            }

            UpdateRequest ur1 = ub1.buildRequest();
            UpdateRequest ur2 = ub2.buildRequest();

            //Use access agent
            this.updateStore(route, ur1.toString());
            this.updateStore(route, ur2.toString());
        }

    }

    /**
     * Returns readable unit from ontology iri
     * @param ontologyUnit unit iri in ontology
     * @return unit as a String
     */
    public String getUnit(String ontologyUnit) {
        switch(ontologyUnit) {
            case("http://www.ontology-of-units-of-measure.org/resource/om-2/kilowattHour"):
                return "kWh";
            case("http://www.ontology-of-units-of-measure.org/resource/om-2/squareMetre"):
                return "m^2";
            default:
                return "";
        }
    }

    /**
     * Returns data using time series client for given data iri
     * @param dataIri iri in time series database
     * @return time series data
     */
    public <T> TimeSeries<T> retrieveData(String dataIri, RemoteStoreClient store, RemoteRDBStoreClient rdbStore, Class<T> timeClass) {
        TimeSeriesClient<T> client = new TimeSeriesClient<>(store, timeClass);

        List<String> iris = new ArrayList<>();
        iris.add(dataIri);
        try (Connection conn = rdbStore.getConnection()) {
            TimeSeries<T> data = client.getTimeSeries(iris, conn);
            return data;
        }
        catch (SQLException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Calculates annual value by summing all data in column in time series and rounding to 2dp
     * @param timeSeries time series data
     * @param dataIri iri in time series database
     * @return annualValue as a String
     */
    public String calculateAnnual(TimeSeries<OffsetDateTime> timeSeries, String dataIri){
        List<Double> values = timeSeries.getValuesAsDouble(dataIri);
        Double annualValue = 0.;
        for(Double value : values){
            annualValue += value;
        }
        annualValue = Math.round(annualValue*Math.pow(10,2))/Math.pow(10,2);
        return annualValue.toString();

    }
}
