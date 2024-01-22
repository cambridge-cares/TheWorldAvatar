package uk.ac.cam.cares.jps.agent.openmeteo;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.stack.clients.core.StackClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.Type;
import java.net.URL;
import java.net.URLConnection;
import java.sql.Connection;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@WebServlet(
        urlPatterns = {
                OpenMeteoAgent.URI_RUN,
                OpenMeteoAgent.URI_DELETE
        })
public class OpenMeteoAgent extends JPSAgent {
    public static final String KEY_REQ_METHOD = "method";
    public static final String KEY_REQ_URL = "requestUrl";

    public static final String URI_RUN = "/run";
    public static final String URI_DELETE = "/delete";

    public static final String KEY_ROUTE = "route";
    public static final String KEY_LAT = "latitude";
    public static final String KEY_LON = "longitude";
    public static final String KEY_START = "start_date";
    public static final String KEY_END = "end_date";

    public static final String KEY_STATION = "stationIRI";

    private static final String API_URL = "https://archive-api.open-meteo.com/v1/archive";
    private static final String API_HOURLY = "hourly";
    private static final String API_HOURLY_UNITS = "hourly_units";
    private static final String API_TIME = "time";
    private static final String API_TIMEZONE = "timezone";
    private static final String API_PARAMETER_TEMP = "temperature_2m";
    private static final String ONTOEMS_TEMP = "AirTemperature";
    private static final String API_PARAMETER_HUMIDITY = "relativehumidity_2m";
    private static final String ONTOEMS_HUMIDITY = "RelativeHumidity";
    private static final String API_PARAMETER_DEWPOINT = "dewpoint_2m";
    private static final String ONTOEMS_DEWPOINT = "DewPoint";
    private static final String API_PARAMETER_PRESSURE = "surface_pressure";
    private static final String ONTOEMS_PRESSURE = "AtmosphericPressure";
    private static final String API_PARAMETER_RAIN = "rain";
    private static final String ONTOEMS_RAIN = "Rainfall";
    private static final String API_PARAMETER_SNOW = "snowfall";
    private static final String ONTOEMS_SNOW = "Snowfall";
    private static final String API_PARAMETER_CLOUD = "cloudcover";
    private static final String ONTOEMS_CLOUD = "CloudCover";
    private static final String API_PARAMETER_DNI = "direct_normal_irradiance";
    private static final String ONTOEMS_DNI = "DirectNormalIrradiance";
    private static final String API_PARAMETER_DHI = "diffuse_radiation";
    private static final String ONTOEMS_DHI = "DiffuseHorizontalIrradiance";
    private static final String API_PARAMETER_WINDSPEED = "windspeed_10m";
    private static final String ONTOEMS_WINDSPEED = "WindSpeed";
    private static final String API_PARAMETER_WINDDIRECTION = "winddirection_10m";
    private static final String ONTOEMS_WINDDIRECTION = "WindDirection";
    private static final String API_WINDSPEED_UNIT = "windspeed_unit=ms";
    private static final String API_ELEVATION = "elevation";
    private static final String API_OFFSET = "utc_offset_seconds";

    private String ontoemsURI;
    private String ontotimeseriesURI;
    private String omURI;
    private String rdfURI;
    private String geospatialLiterals;
    private static final String STATION = "ReportingStation";
    private static final String OM_C = "om:degreeCelsius";
    private static final String OM_PA = "om:pascal";
    private static final String OM_PER = "om:percent";
    private static final String OM_MM = "om:millimetre";
    private static final String OM_CM = "om:centimetre";
    private static final String OM_MS = "om:metrePerSecond-Time";
    private static final String OM_DEGREE = "om:degree";
    private static final String OM_WM = "om:wattPerSquareMetre";
    private List<String> API_PARAMETERS = Arrays.asList(API_PARAMETER_TEMP, API_PARAMETER_HUMIDITY, API_PARAMETER_DEWPOINT, API_PARAMETER_PRESSURE, API_PARAMETER_RAIN, API_PARAMETER_SNOW, API_PARAMETER_CLOUD, API_PARAMETER_DNI, API_PARAMETER_DHI, API_PARAMETER_WINDSPEED, API_PARAMETER_WINDDIRECTION);
    private List<String> ontoems_conecpts = Arrays.asList(ONTOEMS_TEMP, ONTOEMS_HUMIDITY, ONTOEMS_DEWPOINT, ONTOEMS_PRESSURE, ONTOEMS_RAIN, ONTOEMS_SNOW, ONTOEMS_CLOUD, ONTOEMS_DNI, ONTOEMS_DHI, ONTOEMS_WINDSPEED, ONTOEMS_WINDDIRECTION);
    private Map<String, String> ontoems_api = IntStream.range(0, API_PARAMETERS.size()).boxed()
            .collect(Collectors.toMap(ontoems_conecpts::get, API_PARAMETERS::get));
    private Map<String, TimeSeriesClient.Type> api_timeseries = new HashMap<>();
    private Double latitude;
    private Double longitude;
    private Double elevation;

    public static final String STACK_NAME = "<STACK NAME>";
    private String stackName;
    private String stackAccessAgentBase;
    EndpointConfig endpointConfig = new EndpointConfig();
    private String dbName;
    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;
    private TimeSeriesClient<OffsetDateTime> tsClient;
    private String defaultLabel;

    public OpenMeteoAgent() {
        readConfig();
        stackName = StackClient.getStackName();
        stackAccessAgentBase = stackAccessAgentBase.replace(STACK_NAME, stackName);
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(dbName), endpointConfig.getDbUser(), endpointConfig.getDbPassword());
        setTimeSeriesTypes();
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            if (requestParams.getString(KEY_REQ_URL).contains(URI_RUN)) {
                String route = requestParams.has(KEY_ROUTE) ? requestParams.getString(KEY_ROUTE) : stackAccessAgentBase + defaultLabel;
                setStoreClient(route);
                latitude = Double.parseDouble(requestParams.getString(KEY_LAT));
                longitude = Double.parseDouble(requestParams.getString(KEY_LON));
                JSONObject response = getWeatherData(latitude, longitude, requestParams.getString(KEY_START), requestParams.getString(KEY_END));

                Integer offset = response.getInt(API_OFFSET);

                JSONObject weatherData = response.getJSONObject(API_HOURLY);
                JSONObject weatherUnit = response.getJSONObject(API_HOURLY_UNITS);

                elevation = response.getDouble(API_ELEVATION);

                List<OffsetDateTime> timesList = getTimesList(weatherData, API_TIME, offset);
                Map<String, List<Object>> parsedData = parseWeatherData(weatherData, weatherUnit);

                String stationIRI = getStation(latitude, longitude, route);

                List<TimeSeries<OffsetDateTime>> tsList = new ArrayList<>();

                tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);

                if (stationIRI.isEmpty()) {
                    stationIRI = createStation(latitude, longitude, elevation, route);
                    instantiateWeather(stationIRI, parsedData, timesList, tsList, route);
                }
                else {
                    JSONArray queryResults = getWeatherIRI(stationIRI, route);
                    try (Connection conn = rdbStoreClient.getConnection()) {
                        for (int i = 0; i < queryResults.length(); i++) {
                            // delete old time series history
                            OffsetDateTime min = tsClient.getMinTime(queryResults.getJSONObject(i).getString("measure"), conn);
                            OffsetDateTime max = tsClient.getMaxTime(queryResults.getJSONObject(i).getString("measure"), conn);
                            tsClient.deleteTimeSeriesHistory(queryResults.getJSONObject(i).getString("measure"), min, max, conn);

                            // new time series
                            List<String> dataIRI = Arrays.asList(queryResults.getJSONObject(i).getString("measure"));
                            String apiParam = ontoems_api.get(queryResults.getJSONObject(i).getString("weatherParameter").split(ontoemsURI)[1]);
                            tsList.add(new TimeSeries<>(timesList, dataIRI, List.of((List<Double>) parsedData.get(apiParam).get(1))));
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                        throw new JPSRuntimeException(e);
                    }
                }

                try (Connection conn = rdbStoreClient.getConnection()) {
                    tsClient.bulkaddTimeSeriesData(tsList, conn);
                } catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }

                requestParams.append(KEY_STATION, stationIRI);
            }
            else if (requestParams.getString(KEY_REQ_URL).contains(URI_DELETE)) {
                String route = requestParams.has(KEY_ROUTE) ? requestParams.getString(KEY_ROUTE) : stackAccessAgentBase + defaultLabel;
                setStoreClient(route);
                latitude = Double.parseDouble(requestParams.getString(KEY_LAT));
                longitude = Double.parseDouble(requestParams.getString(KEY_LON));

                String stationIRI = getStation(latitude, longitude, route);

                if (stationIRI.isEmpty()) {
                    throw new JPSRuntimeException("No reporting station found at the given coordinate.");
                }

                JSONArray queryResults = getWeatherIRI(stationIRI, route);

                tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);

                try (Connection conn = rdbStoreClient.getConnection()) {
                    for (int i = 0; i < queryResults.length(); i++){
                        tsClient.deleteTimeSeries(queryResults.getJSONObject(i).getString("timeseries"), conn);
                        deleteIRI(queryResults.getJSONObject(i).getString("measure"), route);
                        deleteIRI(queryResults.getJSONObject(i).getString("quantity"), route);
                    }
                }
                catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }

                deleteIRI(stationIRI, route);
            }
        }
        return requestParams;
    }

    /**
     * Gets variables from config.properties
     */
    private void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        ontoemsURI = config.getString("uri.ontology.ontoems");
        ontotimeseriesURI = config.getString("uri.ontology.ontotimeseries");
        omURI = config.getString("uri.ontology.om");
        rdfURI = config.getString("uri.ontology.rdf");
        geospatialLiterals = config.getString("uri.geospatial.literals");
        stackAccessAgentBase = config.getString("access.url");
        defaultLabel = config.getString("route.label");
        dbName = config.getString("db.name");
    }

    /**
     * Checks validity of incoming request
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        boolean validate = false;

        try {
            if (requestParams.get(KEY_REQ_METHOD).equals(HttpMethod.POST)) {
                // check latitude and longitude are provided, and are numbers
                validate = !requestParams.get(KEY_LAT).equals(null) && !requestParams.get(KEY_LON).equals(null)
                        && isNumber(requestParams.getString(KEY_LAT)) && isNumber(requestParams.getString(KEY_LON));

                if (requestParams.getString(KEY_REQ_URL).contains(URI_RUN)) {
                    validate = validate &&!requestParams.getString(KEY_START).isEmpty() && !requestParams.getString(KEY_END).isEmpty()
                            && validateDate(requestParams.getString(KEY_START))
                            && validateDate(requestParams.getString(KEY_END));

                    SimpleDateFormat ymd = new SimpleDateFormat("yyyy-MM-dd");
                    Date start = ymd.parse(requestParams.getString(KEY_START));
                    Date end = ymd.parse(requestParams.getString(KEY_END));
                    Date now = new Date();

                    // start date cannot be later than end date, and end date cannot be later than the date at the time of the incoming request
                    validate = validate && !end.before(start)
                            && (end.before(now) || end.equals(now));
                }
            }
        }
        catch (Exception e){
            throw new BadRequestException();
        }

        if (!validate){
            throw new BadRequestException();
        }

        return true;
    }

    /**
     * Validates whether the string is in the yyyy-mm-dd date format
     * @param date The date as a string
     * @return Validity of the format of date
     */
    public boolean validateDate(String date) {
        try {
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
            Date dateCheck = df.parse(date);
            return df.format(dateCheck).equals(date);
        } catch (ParseException e) {
            throw new BadRequestException();
        }
    }

    /**
     * Calls the Open-Meteo Historical Weather API to retrieve the hourly weather data from start_date to end_date with weather parameter in API_PARAMETERS for weather station located at (latitude, longitude)
     * @param latitude Geographical WGS84 coordinate of the weather station
     * @param longitude Geographical WGS84 coordinate of the weather station
     * @param startDate Start date of the time interval, in yyyy-mm-dd format
     * @param endDate End date of the time interval, in yyyy-mm-dd format
     * @return Weather data as a JSONObject
     */
    public JSONObject getWeatherData(Double latitude, Double longitude, String startDate, String endDate) {
        String query = KEY_LAT + "=" + latitude + "&";
        query = query + KEY_LON + "=" + longitude + "&";
        query = query + KEY_START + "=" + startDate + "&";
        query = query + KEY_END + "=" + endDate + "&" + API_TIMEZONE + "=auto&" + API_WINDSPEED_UNIT +"&" + API_HOURLY + "=";

        for (String parameter: API_PARAMETERS){
            query = query + parameter + ",";
        }

        query = query.substring(0, query.length() - 1);

        try {
            URLConnection connection = new URL(API_URL + "?" + query).openConnection();

            InputStream is = connection.getInputStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(is));
            StringBuffer response = new StringBuffer();
            String line;
            while ((line = rd.readLine()) != null && !line.isEmpty()){
                response.append(line);
            }
            rd.close();
            return new JSONObject(response.toString());
        }
        catch (IOException e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Parses weatherData into list
     * @param weatherData JSONObject containing weather data retrieved by the API
     * @param weatherUnit JSONObject containing the units of the weather data retrieved by the API
     * @return a Map object with the key being the API weather parameters, and the values being the corresponding units ontology and the corresponding data parsed to a list
     */
    private  Map<String, List<Object>> parseWeatherData(JSONObject weatherData, JSONObject weatherUnit) {
        Map<String, List<Object>> data = new HashMap<>();
        List<Object> temp;
        JSONArray tempJSONArray;
        Type tempType;
        for (String parameter: API_PARAMETERS){
            temp = new ArrayList<>();
            temp.add(getUnitOntology(weatherUnit.getString(parameter)));
            tempJSONArray = weatherData.getJSONArray(parameter);
            tempType = new TypeToken<List<Double>>() {}.getType();
            temp.add(new Gson().fromJson(tempJSONArray.toString(), tempType));
            data.put(parameter, temp);
        }

        return data;
    }

    /**
     * Converts unit text string to its representative ontology concept name
     * @param unit unit text string
     * @return unit ontology concept name
     */
    public String getUnitOntology(String unit) {
        if (unit.contains("°C")){return OM_C;}
        if (unit.contains("°")){return OM_DEGREE;}
        if (unit.contains("W/m")){return OM_WM;}

        switch (unit){
            case("%"):
                return OM_PER;
            case("mm"):
                return OM_MM;
            case("hPa"):
                return OM_PA;
            case("cm"):
                return OM_CM;
            case("m/s"):
                return OM_MS;
            default:
                return "";
        }
    }

    /**
     * Creates OntoEMS:ReportingStation instance with location being lat, lon
     * @param lat latitude of OntoEMS:ReportingStation instance
     * @param lon longitude of OntoEMS:ReportingStation instance
     * @param ele observation elevation of OntoEMS:ReportingStation instance
     * @param route access agent route
     * @return OntoEMS:ReportingStation instance IRI
     */
    private String createStation(Double lat, Double lon, Double ele, String route) {
        String stationIRI = ontoemsURI + STATION + "_" + UUID.randomUUID();

        String coordinate = lat + "placeHolder" + lon;

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoems", ontoemsURI)
                .addPrefix("rdf", rdfURI);

        wb.addWhere(NodeFactory.createURI(stationIRI), "rdf:type", "ontoems:ReportingStation")
                .addWhere(NodeFactory.createURI(stationIRI), "ontoems:hasObservationLocation", coordinate)
                .addWhere(NodeFactory.createURI(stationIRI), "ontoems:hasObservationElevation",  ele);

        UpdateBuilder ub = new UpdateBuilder()
                .addInsert(wb);

        String queryString = ub.buildRequest().toString().replace("placeHolder", "#");
        queryString = queryString.replace(lon + "\"", lon + "\"^^<" + geospatialLiterals + ">");

        this.updateStore(route, queryString);

        return stationIRI;
    }

    /**
     * Creates SPARQL update according to the ontoems ontology
     * @param builder where builder
     * @param station reporting station IRI
     * @param quantity quantity IRI
     * @param type type of quantity
     * @param measure measure IRI
     * @param unit unit of quantity
     */
    private void createUpdate(WhereBuilder builder, String station, String quantity, String type, String measure, String unit) {
        builder.addWhere(NodeFactory.createURI(station),  "ontoems:reports", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", type)
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", unit);
    }

    /**
     * Creates query for the quantity IRI, quantity type, measure IRI, and time series IRI related to the reporting station IRI
     * @param builder where builder
     * @param station reporting station IRI
     */
    private void addWeatherWhere(WhereBuilder builder, String station) {
        builder.addWhere(NodeFactory.createURI(station), "ontoems:reports", "?quantity")
                .addWhere("?quantity", "rdf:type", "?weatherParameter")
                .addWhere("?quantity", "om:hasValue", "?measure")
                .addWhere("?measure", "ontotimeseries:hasTimeSeries", "?timeseries");
    }

    /**
     * Queries for the OntoEMS:ReportingStation IRI with the given lat, lon coordinate
     * @param lat latitude of the reporting station
     * @param lon longitude of the reporting station
     * @param route access agent route
     * @return reporting station IRI at the given coordinate if it exists
     */
    private String getStation(Double lat, Double lon, String route) {
        String coordinate = lat + "placeHolder" + lon;

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoems", ontoemsURI)
                .addPrefix("rdf", rdfURI);


        wb.addWhere("?station", "rdf:type", "ontoems:ReportingStation")
                .addWhere("?station", "ontoems:hasObservationLocation", coordinate);

        SelectBuilder sb = new SelectBuilder()
                .addVar("?station")
                .addWhere(wb);
        
        String queryString = sb.build().toString().replace("placeHolder", "#");
        queryString = queryString.replace(lon + "\"", lon + "\"^^<" + geospatialLiterals + ">");

        JSONArray queryResult = this.queryStore(route, queryString);

        if (queryResult.isEmpty()){
            return "";
        }
        else{
            return queryResult.getJSONObject(0).getString("station");
        }
    }

    /**
     * Queries for and returns the quantity IRI, quantity type, measure IRI, and time series IRI related to stationIRI
     * @param stationIRI IRI of weather station
     * @param route access agent route
     * @return query result for quantity IRI, quantity type, measure IRI, and time series IRI related to stationIRI
     */
    private JSONArray getWeatherIRI(String stationIRI, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoems", ontoemsURI)
                .addPrefix("ontotimeseries", ontotimeseriesURI)
                .addPrefix("om", omURI)
                .addPrefix("rdf", rdfURI);

        addWeatherWhere(wb, stationIRI);

        SelectBuilder sb = new SelectBuilder()
                .addVar("timeseries")
                .addVar("quantity")
                .addVar("measure")
                .addVar("weatherParameter")
                .addWhere(wb);

        return this.queryStore(route, sb.build().toString());
    }

    /**
     * Deletes all triples related to the given IRI
     * @param iri IRI to delete
     * @param route access agent route
     */
    private void deleteIRI(String iri, String route){
        UpdateBuilder db = new UpdateBuilder()
                .addWhere(NodeFactory.createURI(iri), "?p", "?o");

        UpdateBuilder db1 = new UpdateBuilder()
                .addWhere("?s", "?p1", NodeFactory.createURI(iri));

        db.addDeleteQuads(db.buildDeleteWhere().getQuads());
        db1.addDeleteQuads(db1.buildDeleteWhere().getQuads());

        this.updateStore(route, db.buildRequest().toString());
        this.updateStore(route, db1.buildRequest().toString());
    }

    /**
     * Creates time series for dataIRI
     * @param dataIRI IRIs to create time series for
     * @param dataClass data class
     * @param timeUnit time unit
     * @param type type of time series
     * @param durations numeric duration of the averaging period for time series of TimeSeriesClient.Type.AVERAGE
     * @param units temporal unit of the averaging period for time series of TimeSeriesClient.Type.AVERAGE
     */
    private void createTimeSeries(List<List<String>> dataIRI, List<List<Class<?>>> dataClass, List<String> timeUnit, List<TimeSeriesClient.Type> type, List<Duration> durations, List<ChronoUnit> units) {
        try(Connection conn = rdbStoreClient.getConnection()){
            tsClient.bulkInitTimeSeries(dataIRI, dataClass, timeUnit, conn, type, durations, units);
        }
        catch (Exception e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Sets the time series types for each of the weather parameter
     */
    private void setTimeSeriesTypes() {
        api_timeseries.put(API_PARAMETER_TEMP, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_HUMIDITY, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_DEWPOINT, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_PRESSURE, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_RAIN, TimeSeriesClient.Type.STEPWISECUMULATIVE);
        api_timeseries.put(API_PARAMETER_SNOW, TimeSeriesClient.Type.STEPWISECUMULATIVE);
        api_timeseries.put(API_PARAMETER_CLOUD, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_DNI, TimeSeriesClient.Type.AVERAGE);
        api_timeseries.put(API_PARAMETER_DHI, TimeSeriesClient.Type.AVERAGE);
        api_timeseries.put(API_PARAMETER_WINDSPEED, TimeSeriesClient.Type.INSTANTANEOUS);
        api_timeseries.put(API_PARAMETER_WINDDIRECTION, TimeSeriesClient.Type.INSTANTANEOUS);
    }

    /**
     * Sets store client to the query and update endpoint of route
     * @param route access agent route
     */
    private void setStoreClient(String route) {
        JSONObject queryResult = this.getEndpoints(route);

        String queryEndpoint = queryResult.getString(JPSConstants.QUERY_ENDPOINT);
        String updateEndpoint = queryResult.getString(JPSConstants.UPDATE_ENDPOINT);

        if (!isDockerized()){
            queryEndpoint = queryEndpoint.replace("host.docker.internal", "localhost");
            updateEndpoint = updateEndpoint.replace("host.docker.internal", "localhost");
        }

        storeClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
    }

    /**
     * Check if the agent is running in Docker
     * @return true if running in Docker, false otherwise
     */
    private boolean isDockerized() {
        File f = new File("/.dockerenv");
        return f.exists();
    }

    /**
     * Parses the times in data into a list of OffsetDateTime
     * @param data JSONObject containing the weather data
     * @param key key to getting the times
     * @param offset timezone of the weather station
     * @return the times parsed into a list of OffsetDateTime
     */
    private List<OffsetDateTime> getTimesList(JSONObject data, String key, Integer offset) {
        JSONArray array = data.getJSONArray(key);

        ZoneOffset zoneOffset = ZoneOffset.ofTotalSeconds(offset);

        List<OffsetDateTime> timesList = new ArrayList<>();

        for (int i = 0; i < array.length(); i++){
            timesList.add(LocalDateTime.parse(array.getString(i)).atOffset(zoneOffset));
        }

        return  timesList;
    }

    /**
     * Instantiates weather data as time series
     * @param stationIRI IRI of weather station
     * @param parsedData weather data
     * @param timesList list of timestamps of parsedData
     * @param tsList list to store TimeSeries
     * @param route access agent route
     */
    private void instantiateWeather(String stationIRI,  Map<String, List<Object>> parsedData, List<OffsetDateTime> timesList,  List<TimeSeries<OffsetDateTime>> tsList, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoems", ontoemsURI)
                .addPrefix("om", omURI)
                .addPrefix("rdf", rdfURI);

        wb.addWhere(NodeFactory.createURI(stationIRI), "rdf:type", "ontoems:" + STATION);

        String apiParam;
        String ontoemsClass;
        String quantityIRI;
        String measureIRI;

        List<List<String>> dataIRIs = new ArrayList<>();
        List<String> dataIRI;
        List<TimeSeriesClient.Type> tsTypes = new ArrayList<>();
        List<Duration> durations = new ArrayList<>();
        List<ChronoUnit> units = new ArrayList<>();
        List<Double> value;

        for (var entry : ontoems_api.entrySet()) {
            apiParam = entry.getValue();

            if (parsedData.containsKey(apiParam)) {
                List<Object> data = parsedData.get(apiParam);
                ontoemsClass = entry.getKey();
                quantityIRI = ontoemsURI + ontoemsClass + "Quantity_" + UUID.randomUUID();
                measureIRI = ontoemsURI + ontoemsClass + "_" + UUID.randomUUID();

                // SPARQL update for weather triples
                createUpdate(wb, stationIRI, quantityIRI, "ontoems:" + ontoemsClass, measureIRI, (String) data.get(0));

                // store time series information
                dataIRI = Arrays.asList(measureIRI);
                dataIRIs.add(dataIRI);
                value = (List<Double>) data.get(1);
                tsList.add(new TimeSeries<>(timesList, dataIRI, List.of(value)));
                tsTypes.add(api_timeseries.get(apiParam));
                if (api_timeseries.get(apiParam) == TimeSeriesClient.Type.AVERAGE) {
                    durations.add(Duration.ofHours(1));
                    units.add(ChronoUnit.HOURS);
                } else {
                    durations.add(null);
                    units.add(null);
                }
            }
        }

        List<List<Class<?>>> dataClass = Collections.nCopies(dataIRIs.size(), Arrays.asList(Double.class));
        List<String> timeUnit = Collections.nCopies(dataIRIs.size(), OffsetDateTime.class.getSimpleName());

        UpdateBuilder ub = new UpdateBuilder()
                .addInsert(wb);

        // instantiate weather triples
        this.updateStore(route, ub.buildRequest().toString());

        // create time series for weather data
        createTimeSeries(dataIRIs, dataClass, timeUnit, tsTypes, durations, units);
    }

    /**
     * Checks if a string is able to be parsable as a number
     * @param number string to check
     * @return boolean value of check
     */
    public boolean isNumber(String number) {
        try{
            Double.parseDouble(number);
            return true;
        }
        catch (Exception e){
            return false;
        }
    }
}