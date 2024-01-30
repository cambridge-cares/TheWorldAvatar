package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.RouteHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;

import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import kong.unirest.UnirestException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

import org.apache.commons.lang.StringUtils;
import org.apache.http.protocol.HTTP;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;
import org.json.JSONArray;
import org.json.JSONObject;

import org.locationtech.jts.geom.Coordinate;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class WeatherHelper extends JPSAgent {
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
    private static final String CRS_4326 = "EPSG:4326";

    private String openmeteoagentUrl;
    private String dbUser;
    private String dbPassword;
    private String weatherRoute;

    public WeatherHelper(String url, String user, String password, String route) {
        this.openmeteoagentUrl = url;
        this.dbUser = user;
        this.dbPassword = password;
        this.weatherRoute = route;

    }

    /**
     * Retrieves weather data
     * @param weatherRoute route to weather data
     * @param crs CRS of city object geometry
     * @param result list to add the retrieved weather data to
     * @return true if weather data retrieved, false otherwise
     */
    public boolean getWeather(CEAGeometryData building, List<CEAGeometryData> surroundings, String weatherRoute, String crs, List<Object> result) {
        Envelope envelope = new Envelope();
        Coordinate centerCoordinate;
        Double elevation = 0.0;
        long count = 0;
        if (surroundings != null) {
            for (CEAGeometryData ceaGeometryData : surroundings) {
                for (Geometry geometry : ceaGeometryData.getFootprint()) {
                    envelope.expandToInclude(geometry.getEnvelopeInternal());
                    elevation += Arrays.stream(geometry.getCoordinates())
                            .mapToDouble(coordinate -> Double.isNaN(coordinate.getZ()) ? 0.0 :coordinate.z)
                            .sum();
                    count += Arrays.stream(geometry.getCoordinates())
                            .filter(coordinate -> !Double.isNaN(coordinate.getZ()))
                            .count();
                }
            }

            elevation = elevation / count;
        }
        else {
            for (Geometry geometry : building.getFootprint()) {
                envelope.expandToInclude(geometry.getEnvelopeInternal());
                elevation += Arrays.stream(geometry.getCoordinates())
                        .mapToDouble(coordinate -> Double.isNaN(coordinate.getZ()) ? 0.0 :coordinate.z)
                        .sum();
                count += Arrays.stream(geometry.getCoordinates())
                        .filter(coordinate -> !Double.isNaN(coordinate.getZ()))
                        .count();
            }

            elevation = elevation / count;
        }
        if (count == 0) {
            elevation = 100.00;
        }

        centerCoordinate = envelope.centre();

        crs = StringUtils.isNumeric(crs) ? "EPSG:" + crs : crs;

        try {
            // coordinate in (longitude, latitude) format
            Coordinate coordinate = GeometryHandler.transformCoordinate(centerCoordinate, crs, CRS_4326);

            Double latitude;
            Double longitude;

            latitude = coordinate.getY();
            longitude = coordinate.getX();

            String stationIRI = getWeatherStation(coordinate, 2.0, weatherRoute);

            // if no nearby weather station, send request to OpenMeteoAgent to instantiate weather data
            if (stationIRI.isEmpty()) {
                stationIRI = runOpenMeteoAgent(String.valueOf(latitude), String.valueOf(longitude), weatherRoute);

                // if request fails
                if (stationIRI.isEmpty()) {return false;}
            }

            Map<String, List<String>> weatherMap = getWeatherIRI(stationIRI, weatherRoute);

            List<Double> lat_lon = getStationCoordinate(stationIRI, weatherRoute);

            if (!lat_lon.isEmpty()) {
                latitude = lat_lon.get(0);
                longitude = lat_lon.get(1);
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
            System.out.println("No weather data retrieved, agent will run CEA with CEA's default weather.");
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
                .addPrefix("geoservice", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geoservice))
                .addPrefix("geoliteral", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geoliteral))
                .addPrefix("ontoems", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems));

        wb.addWhere("?station", "geoservice:search", "inCircle")
                .addWhere("?station", "geoservice:searchDatatype", "geoliteral:lat-lon")
                .addWhere("?station", "geoservice:predicate", "ontoems:hasObservationLocation")
                // PLACEHOLDER because the coordinate will be treated as doubles instead of string otherwise
                .addWhere("?station", "geoservice:spatialCircleCenter", center.getX() + "PLACEHOLDER" + center.getY())
                .addWhere("?station", "geoservice:spatialCircleRadius", radius);

        SelectBuilder sb = new SelectBuilder()
                .addVar("?station");

        Query query = sb.build();

        // add geoservicespatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(OntologyURIHelper.getOntologyUri(OntologyURIHelper.geoservice) + "search", wb.build().getQueryPattern()));
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
                .addPrefix("ontoEMS", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf));

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
                .addPrefix("ontoEMS", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf));

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
            ZoneOffset utc0 = ZoneOffset.ofTotalSeconds(0);
            OffsetDateTime offsetStart = startDate.atOffset(utc0);

            // make use of the fact that the historical weather times instantiated by OpenMeteoAgent will always start at the 0 hour with an offset
            // if the start date when instantiated was the start of year, one could get back the correct offset with the above fact
            if (offsetStart.getMonthValue() == 1 && offsetStart.getDayOfMonth() == 1) {
                return -1 * Double.valueOf(offsetStart.getHour()) * 60 * 60;
            }
            else if (offsetStart.getMonthValue() == 12 && offsetStart.getDayOfMonth() == 31) {
                return Double.valueOf(24 - offsetStart.getHour()) * 60 * 60;
            }
            // if failed to get the offset from API or the date is not the start of the year,
            // return an approximation based on the longitude
            // approximation assumes Earth is rough divided into 24 time zones equally over the globe
            else {
                return longitude / 15 * 60 * 60;
            }
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
                .addPrefix("ontoems", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems))
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("ontotimeseries", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontotimeseries));

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
                result.put(queryResultArray.getJSONObject(i).getString("weatherParameter").split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems))[1], Arrays.asList(queryResultArray.getJSONObject(i).getString("measure"), queryResultArray.getJSONObject(i).getString("rdb")));
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
            TimeSeries<Instant> weatherTS = TimeSeriesHelper.retrieveData(weatherIRI, (RemoteStoreClient) weatherClients.get(STORE_CLIENT), (RemoteRDBStoreClient) weatherClients.get(RDB_CLIENT), Instant.class);

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
     * @param weatherDb database storing the weather data
     * @return a map containing a RemoteStoreClient object and RemoteRDBStoreClient object that will allow for the querying of weather data
     */
    public Map<String, Object> getWeatherClients(String weatherDb) {
        RemoteRDBStoreClient weatherRDBClient = new RemoteRDBStoreClient(weatherDb, dbUser, dbPassword);
        List<String> weatherEndpoints = RouteHelper.getRouteEndpoints(weatherRoute);
        RemoteStoreClient weatherStoreClient = new RemoteStoreClient(weatherEndpoints.get(0), weatherEndpoints.get(1));
        Map<String, Object> result = new HashMap<>();
        result.put(RDB_CLIENT, weatherRDBClient);
        result.put(STORE_CLIENT, weatherStoreClient);
        return result;
    }
}
