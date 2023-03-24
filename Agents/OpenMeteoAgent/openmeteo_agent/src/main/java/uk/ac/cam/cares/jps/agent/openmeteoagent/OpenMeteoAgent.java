package uk.ac.cam.cares.jps.agent.openmeteoagent;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.Type;
import java.net.URL;
import java.net.URLConnection;
import java.sql.Connection;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@WebServlet(
        urlPatterns = {
                OpenMeteoAgent.URI_INSTANTIATE,
                OpenMeteoAgent.URI_DELETE
        })
public class OpenMeteoAgent extends JPSAgent {
    private static final String KEY_LAT = "latitude";
    private static final String KEY_LONG = "longitude";
    private static final String KEY_START = "start_date";
    private static final String KEY_END = "end_date";

    private static final String API_URL = "https://archive-api.open-meteo.com/v1/archive";
    private static final String API_HOURLY = "hourly";
    private static final String API_HOURLY_UNITS = "hourly_units";
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

    private String ontoemsURI;
    private String omURI;
    private String rdfURI;
    private static final String STATION = "ReportingStation";
    private static final String OM_C = "om:degreeCelsius";
    private static final String OM_PA = "om:pascal";
    private static final String OM_PER = "om:percent";
    private static final String OM_MM = "om:millimetre";
    private static final String OM_CM = "om:centimetre";
    private static final String OM_MS = "om:metrePerSecond-Time";
    private static final String OM_DEGREE = "om:degree";
    private List<String> API_PARAMETERS = Arrays.asList(API_PARAMETER_TEMP, API_PARAMETER_HUMIDITY, API_PARAMETER_DEWPOINT, API_PARAMETER_PRESSURE, API_PARAMETER_RAIN, API_PARAMETER_SNOW, API_PARAMETER_CLOUD, API_PARAMETER_DNI, API_PARAMETER_DHI, API_PARAMETER_WINDSPEED, API_PARAMETER_WINDDIRECTION);
    private List<String> ontoems_conecpts = Arrays.asList(ONTOEMS_TEMP, ONTOEMS_HUMIDITY, ONTOEMS_DEWPOINT, ONTOEMS_PRESSURE, ONTOEMS_RAIN, ONTOEMS_SNOW, ONTOEMS_CLOUD, ONTOEMS_DNI, ONTOEMS_DHI, ONTOEMS_WINDSPEED, ONTOEMS_WINDDIRECTION);
    private Map<String, String> api_ontoems = IntStream.range(0, API_PARAMETERS.size()).boxed()
            .collect(Collectors.toMap(API_PARAMETERS::get, ontoems_conecpts::get));
    private Map<String, TimeSeriesClient.Type> api_timeseries = new HashMap<>();
    private Double latitude;
    private Double longitude;
    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;
    private TimeSeriesClient tsClient;
    private String route;

    public static final String URI_INSTANTIATE = "/instantiate";
    public static final String URI_DELETE = "/delete";

    public OpenMeteoAgent() {
        readConfig();
        setTimeSeriesTypes();
        setStoreClient(route);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            latitude = requestParams.getDouble(KEY_LAT);
            longitude = requestParams.getDouble(KEY_LONG);
            JSONObject response = getWeatherData(latitude, longitude, requestParams.getString(KEY_START), requestParams.getString(KEY_END));

            JSONObject weatherData = response.getJSONObject(API_HOURLY);
            JSONObject weatherUnit = response.getJSONObject(API_HOURLY_UNITS);

            Map<String, List<Object>> parsedData = parseWeatherData(weatherData, weatherUnit);

            String stationIRI = createStation(latitude, longitude);

            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("ontoems", ontoemsURI)
                    .addPrefix("om", omURI)
                    .addPrefix("rdf", rdfURI);

            wb.addWhere(NodeFactory.createURI(stationIRI), "rdf:type", "ontoems:"+STATION);

            String apiParam;
            String ontoemsClass;
            String quantityIRI;
            String measureIRI;

            List<List<String>> dataIRIs = new ArrayList<>();
            List<String> dataIRI;
            List<TimeSeriesClient.Type> tsTypes = new ArrayList<>();
            List<Duration> durations = new ArrayList<>();
            List<ChronoUnit> units = new ArrayList<>();

            for (var entry: api_ontoems.entrySet()){
                apiParam = entry.getKey();

                if (parsedData.containsKey(apiParam)){
                    List<Object> data = parsedData.get(apiParam);
                    ontoemsClass = entry.getValue();
                    quantityIRI = ontoemsURI + ontoemsClass + "Quantity_" + UUID.randomUUID();
                    measureIRI = ontoemsURI + ontoemsClass + "_" + UUID.randomUUID();
                    createUpdate(wb, stationIRI, quantityIRI, "ontoems:"+ontoemsClass, measureIRI, (String) data.get(0));
                    dataIRI = Arrays.asList(measureIRI);
                    dataIRIs.add(dataIRI);
                    tsTypes.add(api_timeseries.get(apiParam));
                    if (api_timeseries.get(apiParam) == TimeSeriesClient.Type.AVERAGE){
                        durations.add(Duration.ofHours(1));
                        units.add(ChronoUnit.HOURS);
                    }
                    else{
                        durations.add(null);
                        units.add(null);
                    }
                }
            }

            List<List<Class<?>>> dataClass = Collections.nCopies(dataIRIs.size(), Arrays.asList(Double.class));
            List<String> timeUnit = Collections.nCopies(dataIRIs.size(), OffsetDateTime.class.getSimpleName());

            UpdateBuilder ub = new UpdateBuilder()
                    .addInsert(wb);

            this.updateStore(route, ub.buildRequest().toString());

            createTimeSeries(dataIRIs, dataClass, timeUnit, tsTypes, durations, units);


        }
        return requestParams;
    }

    /**
     * Gets variables from config.properties
     */
    private void readConfig(){
        ResourceBundle config = ResourceBundle.getBundle("config");
        ontoemsURI = config.getString("uri.ontology.ontoems");
        omURI = config.getString("uri.ontology.om");
        rdfURI = config.getString("uri.ontology.rdf");
        route = config.getString("route.uri");
        rdbStoreClient = new RemoteRDBStoreClient(config.getString("db.url"), config.getString("db.user"), config.getString("db.password"));
    }

    /**
     * Checks validity of incoming request
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        boolean validate;

        try{
            // check latitude and longitude are provided, and are numbers
            validate = !requestParams.get(KEY_LAT).equals(null) && ! requestParams.get(KEY_LONG).equals(null)
                    && !(requestParams.get(KEY_LAT) instanceof String) && !(requestParams.get(KEY_LONG) instanceof String);

            if (requestParams.getString("requestUrl").contains(URI_INSTANTIATE)){
                validate = !requestParams.getString(KEY_START).isEmpty() && !requestParams.getString(KEY_END).isEmpty()
                        && validateDate(requestParams.getString(KEY_START))
                        && validateDate(requestParams.getString(KEY_END));

                SimpleDateFormat ymd = new SimpleDateFormat("yyyy-MM-dd");
                Date start = ymd.parse(requestParams.getString(KEY_START));
                Date end = ymd.parse(requestParams.getString(KEY_END));
                Date now = new Date();

                // start date cannot be later than end date, and end date cannot be later than the date at the time of the incoming request
                validate = validate && start.before(end)
                        && (end.before(now) || end.equals(now));
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
    public boolean validateDate(String date){
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
        query = query + KEY_LONG + "=" + longitude + "&";
        query = query + KEY_START + "=" + startDate + "&";
        query = query + KEY_END + "=" + endDate + "&" + API_WINDSPEED_UNIT +"&" + API_HOURLY + "=";

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
    public  Map<String, List<Object>> parseWeatherData(JSONObject weatherData, JSONObject weatherUnit){
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
    public String getUnitOntology(String unit){
        if (unit.contains("°C")){return OM_C;}
        if (unit.contains("°")){return OM_DEGREE;}

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
     * @return OntoEMS:ReportingStation instance IRI
     */
    public String createStation(double lat, double lon){
        String stationIRI = ontoemsURI + STATION + "_" + UUID.randomUUID();

        return stationIRI;
    }

    /**
     * Creates SPARQL update according to the ontoems ontology
     * @param builder where builder
     * @param station reporting station iri
     * @param quantity quantity iri
     * @param type type of quantity
     * @param measure measure iri
     * @param unit unit of quantity
     */
    public void createUpdate(WhereBuilder builder, String station, String quantity, String type, String measure, String unit){
        builder.addWhere(NodeFactory.createURI(station),  "ontoems:reports", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", type)
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", unit);
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
    private void createTimeSeries(List<List<String>> dataIRI, List<List<Class<?>>> dataClass, List<String> timeUnit, List<TimeSeriesClient.Type> type, List<Duration> durations, List<ChronoUnit> units){
        tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);

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
    private void setTimeSeriesTypes(){
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
    private void setStoreClient(String route){
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
    private boolean isDockerized(){
        File f = new File("/.dockerenv");
        return f.exists();
    }
}