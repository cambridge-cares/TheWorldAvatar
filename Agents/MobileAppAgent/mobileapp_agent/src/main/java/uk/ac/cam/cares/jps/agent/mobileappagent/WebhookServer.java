package uk.ac.cam.cares.jps.agent.mobileappagent;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.sql.Connection;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.Point;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static uk.ac.cam.cares.jps.agent.mobileappagent.InstantiationClient.instantiationMethod;

public class WebhookServer {

    public static ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
    public static List<Double> accelList_x = new ArrayList<>();
    public static List<Double> accelList_y = new ArrayList<>();
    public static List<Double> accelList_z = new ArrayList<>();

    public static List<List<?>> accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);


    static ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
    static List<Double> magnetometerList_x = new ArrayList<>();
    static List<Double> magnetometerList_y = new ArrayList<>();
    static List<Double> magnetometerList_z = new ArrayList<>();

    public static List<List<?>> magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);

    static ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
    static List<Double> gravityList_x = new ArrayList<>();
    static List<Double> gravityList_y = new ArrayList<>();
    static List<Double> gravityList_z = new ArrayList<>();
    public static List<List<?>> gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);


    static ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
    static List<Double> bearingList = new ArrayList<>();
    static List<Double> speedList = new ArrayList<>();
    static List<Double> altitudeList = new ArrayList<>();
    static List<Point> geomLocationList = new ArrayList<>();
    public static List<List<?>> location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);



    static ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
    static List<Double> dBFSList = new ArrayList<>();
    public static List<List<?>> dBFS_lolValues= Arrays.asList(dBFSList);

    static ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
    static List<Double> lightValueList = new ArrayList<>();
    public static List<List<?>> lightValue_lolValues= Arrays.asList(lightValueList);



    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);
        server.createContext("/", new MyHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Server started on port 8000");

        //Start multithread programme here
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new ScheduledTask(), 5 * 1000, 5 * 1000);
//        testmethod();
        //Step 1: Check if timeseries exists


        //Step 1.1: Create timeseries IRI




        //Step 1.2: Inititailize timeseries
        //Step 2: Hashmap the dataIRI

        //Step 3: Add timeseries data
        //Step 3.1: Downsample data instead


    }

    private JSONArray dataArray;
    private String Query;

    private static final Logger LOGGER = LogManager.getLogger(MobileAppAgent.class);
    private static final String BASEURI = "https://www.theworldavatar.com/kg/measure_";
    private static final String dbURL = "jdbc:postgresql://localhost:5432/develop";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
    private static RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/test/sparql", "http://127.0.0.1:9999/blazegraph/namespace/test/sparql");
    private static TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);

    //Declaring

    public static final String accelerometer = "accelerometer";
    public static final String gravity = "gravity";
    public static final String light = "light";
    public static final String location = "location";
    public static final String magnetometer = "magnetometer";
    public static final String microphone = "microphone";

    //Declare table header as string.
    public static final String timestamp = "timestamp";
    public static final String accel_x = "accel_x";
    public static final String accel_y = "accel_y";
    public static final String accel_z = "accel_z";
    public static final String gravity_x = "gravity_x";
    public static final String gravity_y = "gravity_y";
    public static final String gravity_z = "gravity_z";
    public static final String light_value = "light_value";
    public static final String bearing = "bearing";
    public static final String speed = "speed";
    public static final String altitude = "altitude";
    public static final String geom_location = "geom_location";
    public static final String magnetometer_x = "magnetometer_x";
    public static final String magnetometer_y = "magnetometer_y";
    public static final String magnetometer_z = "magnetometer_z";
    public static final String dbfs = "dbfs";

    //Declare tableHeader as list of strings
    public static List<String> accelerometerHeader = Arrays.asList(timestamp, accel_x, accel_y, accel_z);
    public static List<String> gravityHeader = Arrays.asList(timestamp, gravity_x, gravity_y, gravity_z);
    public static List<String> lightHeader = Arrays.asList(timestamp, light_value);
    public static List<String> locationHeader = Arrays.asList(timestamp, bearing, speed, altitude, geom_location);
    public static List<String> magnetometerHeader = Arrays.asList(timestamp, magnetometer_x, magnetometer_y, magnetometer_z);
    public static List<String> microphoneHeader = Arrays.asList(timestamp, dbfs);
    public static List<List<String>> tableHeaderList= Arrays.asList(accelerometerHeader,gravityHeader,lightHeader,locationHeader,magnetometerHeader,microphoneHeader);
    public static List<String> tableList = Arrays.asList(accelerometer, gravity,light, location,magnetometer,microphone);
    public static void testmethod() {
        HashMap hashMap = new HashMap();
        Boolean unitsWereInstantiated = false;

        //Loop through each table
        for (int i = 0; i < tableList.size(); i++) {
//            resetList(i);

            if (checkIfTimeSeriesExists(i))//Check if dbTable exists
            {
                String IRIQuery;

                IRIQuery = getQueryDataIRIFromDBTable(i);
                JSONArray dataIRIArray = rdbStoreClient.executeQuery(IRIQuery);


                //Get the newest timeseries
                TimeSeries getTimeSeries = parseDataToLists(i, parseJSONArrayToList(dataIRIArray));
                updateData(getTimeSeries);
                unitsWereInstantiated=true;
            }
            else  //When time series does not exist create timeseries
            {
                initTimeseriesIfNotExist(i, hashMap);

            }
        }
//        if (unitsWereInstantiated==true)
//        { LOGGER.debug(String.format("Units were instantiated"));}
//        else{instantiationMethod(hashMap); LOGGER.debug(String.format("Units is now instantiated"));}
    }

    private static void resetList(int tableNumber) {
        //Declare as newlist to empty the list
        if (tableNumber==0){accel_tsList = new ArrayList<>(); accel_lolValues= new ArrayList<>();}
    else if (tableNumber==1){gravity_tsList = new ArrayList<>(); gravity_lolValues= new ArrayList<>();}
    else if (tableNumber==2){lightValue_tsList = new ArrayList<>(); lightValue_lolValues= new ArrayList<>();}
    else if (tableNumber==3){location_tsList = new ArrayList<>() ; location_lolValues= new ArrayList<>();}
    else if (tableNumber==4){magnetometer_tsList= new ArrayList<>(); magnetometer_lolValues= new ArrayList<>();}
    else if (tableNumber==5){dBFS_tsList= new ArrayList<>(); dBFS_lolValues= new ArrayList<>();}
    else {LOGGER.debug(String.format("Table number not specified correctly"));}

    }

    /** Boolean operation that checks if timeseries exists by querying the DBTable, if DBTable returns 0 or catch exception, it does not exist
     * @param i
     * @return
     */

    private static boolean checkIfTimeSeriesExists(int i){
        String IRIQuery;
        try {
            IRIQuery = getQueryDataIRIFromDBTable(i);
            JSONArray dataIRIArray = rdbStoreClient.executeQuery(IRIQuery);
            if (dataIRIArray.length() == 0){
                return false;
            }
        }
        // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
        catch (Exception e) {
            return false;
        }
        return true;
    }


    /** Initiliaze timeseries if it does not exists,
     * @param i table number
     */
    private static void initTimeseriesIfNotExist(int i, HashMap hashMap){
        //Create Timeseries
        List<String> dataIRIList = createTimeSeries(i, hashMap);

        //GetTimeSeries
        TimeSeries getTimeSeries = parseDataToLists(i, dataIRIList);

        //Add timeseries data with tsList
        try (Connection conn = rdbStoreClient.getConnection()) {

            tsClient.addTimeSeriesData(getTimeSeries, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /** Pass in tableNumber, dataArray and dataIRIList, parse dataArray into list of lists. Create timeseries <T> class.
     * @param tableNumber
     * @param dataIRIList
     * @return Timeseries
     */
    private static TimeSeries parseDataToLists(int tableNumber, List<String> dataIRIList) {

        ArrayList<OffsetDateTime> timesList = null;
        List<List<?>> lolvalues = null;
        if (tableNumber==0){timesList= accel_tsList; lolvalues= accel_lolValues;}
        else if (tableNumber==1){timesList= gravity_tsList; lolvalues= gravity_lolValues;}
        else if (tableNumber==2){timesList= lightValue_tsList; lolvalues= lightValue_lolValues;}
        else if (tableNumber==3){timesList= location_tsList; lolvalues= location_lolValues;}
        else if (tableNumber==4){timesList= magnetometer_tsList; lolvalues= magnetometer_lolValues;}
        else if (tableNumber==5){timesList= dBFS_tsList; lolvalues= dBFS_lolValues;}
        else {LOGGER.debug(String.format("Table number not specified correctly"));}
        //Pass time list, dataIRI List - just one, lolvalues, add timeseries to output
        return new TimeSeries(timesList, dataIRIList, lolvalues);
    }

    /** Pass in tableNumber, create dataIRI and assign random UUID.
     * @param tableNumber
     * @return dataIRIList
     */
    private static List<String> createTimeSeries(int tableNumber, HashMap hashMap) {
        List tableHeader= tableHeaderList.get(tableNumber);
        List<String> dataIRIList = new ArrayList<>();;

        //Create dataIRI for each variable
        for (int sensorVariable = 1; sensorVariable < tableHeader.size() ;sensorVariable++){
            String deviceId = null;
            String dataIRIName =BASEURI+deviceId+ tableHeader.get(sensorVariable)+ "_"+ UUID.randomUUID();

            String key = "measure_"+tableHeader.get(sensorVariable);
            hashMap.put(key,dataIRIName);
            dataIRIList.add(dataIRIName);
        }

        String timeUnit = OffsetDateTime.class.getSimpleName();
        List<Class> dataClass;

        //Add geom class when initilizating location
        if (tableHeader==locationHeader) {
            List <Class> dataClassInit = (Collections.nCopies(tableHeader.size()-2,Double.class));
            dataClass = new ArrayList<>(dataClassInit);
            dataClass.add(Point.class);
        }else //Dont need to initliaze
        {
            dataClass = (Collections.nCopies(tableHeader.size()-1,Double.class));
        }

        try (Connection conn = rdbStoreClient.getConnection()) {
            //Create POSTGIS Extension Automatically
            Statement stmt = conn.createStatement();
            String sql = "CREATE EXTENSION IF NOT EXISTS postgis";
            stmt.executeUpdate(sql);


            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.initTimeSeries(dataIRIList, dataClass, timeUnit, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        return dataIRIList;
    }

    /** Generate QueryString to query for dataArray subsequently
     * @param i
     * @return
     */
    private static String getQueryString(int i){
        String query;

        query = "SELECT ";
        for (int b = 0; b < tableHeaderList.get(i).size(); b++){
            if (b==0){ query =query+tableHeaderList.get(i).get(b);}
            else {query = query + ", " + tableHeaderList.get(i).get(b);}
        }
        query = query + " FROM public." + tableList.get(i);

        return query;
    }

    /** Generate query to retrieve dataIRI from DBTable
     * @param i
     * @return
     */
    private static String getQueryDataIRIFromDBTable(int i){
        String query;

        query = "SELECT \"dataIRI\" FROM public.\"dbTable\" WHERE \"dataIRI\" LIKE ";
        for (int b = 1; b < tableHeaderList.get(i).size(); b++){
            if (b==1){ query =query+"'%"+tableHeaderList.get(i).get(b)+"%'";}
            else {query = query + " OR \"dataIRI\" LIKE "+"'%"+tableHeaderList.get(i).get(b)+"%'";}
        }
        return query;
    }

    /** Parse JSONArray into a List, in this case dataIRIList
     * @param jArray
     * @return
     */
    private static ArrayList<String> parseJSONArrayToList (JSONArray jArray){
        ArrayList<String> listdata = new ArrayList<String>();
        if (jArray != null) {
            for (int i=0;i<jArray.length();i++){
                JSONObject row = jArray.getJSONObject(i);
                listdata.add(row.get("dataIRI").toString());
            }
        }
        return listdata;
    }

    /** Pass in timeseries to update data
     * @param ts
     * @throws IllegalArgumentException
     */
    public static void updateData(TimeSeries<OffsetDateTime> ts) throws IllegalArgumentException {
        // Update each time series
        // Retrieve current maximum time to avoid duplicate entries (can be null if no data is in the database yet)
        OffsetDateTime endDataTime;
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);

            try {
                endDataTime = (OffsetDateTime) tsClient.getMaxTime(ts.getDataIRIs().get(0),conn);
            } catch (Exception e) {
                throw new JPSRuntimeException("Could not get max time!");
            }

            OffsetDateTime startCurrentTime = ts.getTimes().get(0);

            // If there is already a maximum time
            if (endDataTime != null) {
                // If the new data overlaps with existing timestamps, prune the new ones
                if (startCurrentTime.isBefore(endDataTime)) {
                    ts = pruneTimeSeries(ts, endDataTime);
                }
            }
            // Only update if there actually is data
            if (!ts.getTimes().isEmpty()) {
                try {
                    tsClient.addTimeSeriesData(ts, conn);
                    LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
                } catch (Exception e) {
                    throw new JPSRuntimeException("Could not add timeseries!");
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    /**
     * Prunes a times series so that all timestamps and corresponding values start after the threshold.
     * @param timeSeries The times series tp prune
     * @param timeThreshold The threshold before which no data should occur
     * @return The resulting datetime object.
     */
    private static TimeSeries<OffsetDateTime> pruneTimeSeries(TimeSeries<OffsetDateTime> timeSeries, OffsetDateTime timeThreshold) {
        // Find the index from which to start
        List<OffsetDateTime> times = timeSeries.getTimes();
        int index = 0;
        while(index < times.size()) {
            if ((times.get(index)).isAfter(timeThreshold)) {
                break;

            }
            index++;
        }
        // Prune timestamps
        List<OffsetDateTime> newTimes = new ArrayList<>();
        // There are timestamps above the threshold
        if (index != times.size()) {
            // Prune the times
            newTimes = new ArrayList<>(times.subList(index, times.size()));
        }
        // Prune data
        List<List<?>> newValues = new ArrayList<>();
        // Prune the values
        for (String iri: timeSeries.getDataIRIs()) {
            // There are timestamps above the threshold
            if (index != times.size()) {
                newValues.add(timeSeries.getValues(iri).subList(index, times.size()));
            }
            else {
                newValues.add(new ArrayList<>());
            }
        }
        return new TimeSeries<>(newTimes, timeSeries.getDataIRIs(), newValues);
    }
















































    /**
     * Timertasks
     */
    public static class ScheduledTask extends TimerTask {
        public void run() {
            testmethod();
        }
    }


    /**
     * Server that handles HTTP Post requests and parse them into a list
     */

    public static class MyHandler implements HttpHandler {

        @Override
        public void handle(HttpExchange exchange) throws IOException {

            String requestMethod = exchange.getRequestMethod();
                if (requestMethod.equalsIgnoreCase("POST")) {
                    // handle POST request
                    StringBuilder data = new StringBuilder();
                    try (BufferedReader reader = new BufferedReader(new InputStreamReader(exchange.getRequestBody(), StandardCharsets.UTF_8))) {
                        String line;
                        while ((line = reader.readLine()) != null) {
                            data.append("[");
                            data.append(line);
                            data.append("]");
                        }
                    }

                    JSONArray dataHTTP = new JSONArray(data.toString());
                    ObjectMapper mapper = new ObjectMapper();
                    JsonNode root = mapper.readTree(String.valueOf(dataHTTP));

                    JsonNode payload = null;
                    for (JsonNode webhook : root) {
                        Integer messageId = webhook.get("messageId").intValue();
                        String sessionId = webhook.get("sessionId").textValue();
                        String deviceId = webhook.get("deviceId").textValue();
                        payload = webhook.get("payload");
                    }

                    for (JsonNode node : payload) {

                        JsonNode timeEPOCH = node.get("time");
                        JsonNode sensor = node.get("name");
                        JsonNode values = node.get("values");
                        Instant instant = Instant.ofEpochSecond(timeEPOCH.longValue() / 1000000000, timeEPOCH.longValue() % 1000000000);
                        OffsetDateTime timestamp = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC);

                        if (sensor.textValue().equals("accelerometer")) {
                            accel_tsList.add(timestamp);
                            accelList_x.add(values.get("x").doubleValue());
                            accelList_y.add(values.get("y").doubleValue());
                            accelList_z.add(values.get("z").doubleValue());
                        }

                        if (sensor.textValue().equals("magnetometer")) {
                            magnetometer_tsList.add(timestamp);
                            magnetometerList_x.add(values.get("x").doubleValue());
                            magnetometerList_y.add(values.get("y").doubleValue());
                            magnetometerList_z.add(values.get("z").doubleValue());

                        }

                        if (sensor.textValue().equals("gravity")) {
                            gravity_tsList.add(timestamp);
                            gravityList_x.add(values.get("x").doubleValue());
                            gravityList_y.add(values.get("y").doubleValue());
                            gravityList_z.add(values.get("z").doubleValue());

                        }

                        if (sensor.textValue().equals("location")) {
                            location_tsList.add(timestamp);
                            bearingList.add(values.get("bearing").doubleValue());
                            speedList.add(values.get("speed").doubleValue());
                            altitudeList.add(values.get("altitude").doubleValue());

                            //Parse latitude and longitude into geom_location
                            double latitude = values.get("latitude").doubleValue();
                            double longitude = values.get("longitude").doubleValue();
                            Point point = new Point(longitude, latitude);
                            point.setSrid(4326);

                            geomLocationList.add(point);
                        }


                        if (sensor.textValue().equals("microphone")) {
                            dBFS_tsList.add(timestamp);
                            dBFSList.add(values.get("dBFS").doubleValue());

//                            System.out.println("Microphone value has been added");
                        }

                        if (sensor.textValue().equals("light")) {
                            lightValue_tsList.add(timestamp);
                            lightValueList.add(values.get("lux").doubleValue());
//                            System.out.println("Light value has been added");
                        }
                    }


                    String response = "<html><body><pre>" + "The server is listening" + "</pre></body></html>";
                    exchange.sendResponseHeaders(200, response.length());
                    try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
                } else {
                    String response = "<html><body><pre>" + "This is not HTTP Post request" + "</pre></body></html>";
                    try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
            }
        }
    }


}
