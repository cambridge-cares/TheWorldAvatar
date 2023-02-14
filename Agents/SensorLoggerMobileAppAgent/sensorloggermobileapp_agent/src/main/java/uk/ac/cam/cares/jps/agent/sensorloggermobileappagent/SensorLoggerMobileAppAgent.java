package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.sql.Connection;
import java.sql.Statement;
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

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.InstantiationClient.instantiationMethod;
public class SensorLoggerMobileAppAgent {

    //Accelerometer list
    private static ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
    private static List<Double> accelList_x = new ArrayList<>();
    private static List<Double> accelList_y = new ArrayList<>();
    private static List<Double> accelList_z = new ArrayList<>();
    private static List<List<?>> accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);

    //Magnetometer list
    private static ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
    private static List<Double> magnetometerList_x = new ArrayList<>();
    private static List<Double> magnetometerList_y = new ArrayList<>();
    private static List<Double> magnetometerList_z = new ArrayList<>();
    private static List<List<?>> magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);

    //Gravity sensor list
    private static ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
    private static List<Double> gravityList_x = new ArrayList<>();
    private static List<Double> gravityList_y = new ArrayList<>();
    private static List<Double> gravityList_z = new ArrayList<>();
    private static List<List<?>> gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);

    //Location list
    private static ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
    private static List<Double> bearingList = new ArrayList<>();
    private static List<Double> speedList = new ArrayList<>();
    private static List<Double> altitudeList = new ArrayList<>();
    private static List<Point> geomLocationList = new ArrayList<>();
    private static List<List<?>> location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);

    //Microphone lists
    private static ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
    private static List<Double> dBFSList = new ArrayList<>();
    private static List<List<?>> dBFS_lolValues= Arrays.asList(dBFSList);

    //Light value lists
    private static ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
    private static List<Double> lightValueList = new ArrayList<>();
    private static List<List<?>> lightValue_lolValues= Arrays.asList(lightValueList);

    private static ArrayList<OffsetDateTime> brightness_tsList = new ArrayList<>();
    private static List<Double> brightnessList = new ArrayList<>();
    private static List<List<?>> brightness_lolValues= Arrays.asList(brightnessList);
    private static String DEVICEID;

    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);
        server.createContext("/", new MyHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Server started on port 8000");

        //Start scheduler
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new ScheduledTask(), 5 * 1000, 5 * 1000);
    }

    /**
     * Timer-tasks
     */
    public static class ScheduledTask extends TimerTask {
        public void run() {
            tsInstantiation();
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
                    JSONArray dataHTTP = new JSONArray(data.toString());
                    ObjectMapper mapper = new ObjectMapper();
                    JsonNode root = mapper.readTree(String.valueOf(dataHTTP));

                    JsonNode payload = null;
                    for (JsonNode webhook : root) {
                        Integer messageId = webhook.get("messageId").intValue();
                        String sessionId = webhook.get("sessionId").textValue();
                        String deviceId = webhook.get("deviceId").textValue();
                        DEVICEID=deviceId;
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
                        }

                        if (sensor.textValue().equals("light")) {
                            lightValue_tsList.add(timestamp);
                            lightValueList.add(values.get("lux").doubleValue());
                        }

                        if (sensor.textValue().equals("brightness")) {
                            brightness_tsList.add(timestamp);
                            brightnessList.add(values.get("brightness").doubleValue());
                        }


                    }
                    String response = "<html><body><pre>" + "The server is listening" + "</pre></body></html>";
                    exchange.sendResponseHeaders(200, response.length());
                    try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
                }
            } else {
                String response = "<html><body><pre>" + "This is not HTTP Post request" + "</pre></body></html>";
                try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
            }
        }
    }

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
    public static final String screen = "screen";
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
    public static final String brightness = "brightness";

    //Declare tableHeader as list of strings
    public static List<String> accelerometerHeader = Arrays.asList(timestamp, accel_x, accel_y, accel_z);
    public static List<String> gravityHeader = Arrays.asList(timestamp, gravity_x, gravity_y, gravity_z);
    public static List<String> lightHeader = Arrays.asList(timestamp, light_value);
    public static List<String> locationHeader = Arrays.asList(timestamp, bearing, speed, altitude, geom_location);
    public static List<String> magnetometerHeader = Arrays.asList(timestamp, magnetometer_x, magnetometer_y, magnetometer_z);
    public static List<String> microphoneHeader = Arrays.asList(timestamp, dbfs);
    public static List<String> screenHeader = Arrays.asList(timestamp, brightness);

    public static List<List<String>> tableHeaderList= Arrays.asList(accelerometerHeader,gravityHeader,lightHeader,locationHeader,magnetometerHeader,microphoneHeader, screenHeader);
    public static List<String> tableList = Arrays.asList(accelerometer, gravity,light, location,magnetometer,microphone,screen);
    public static void tsInstantiation() {
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
        if (unitsWereInstantiated==true)
        { LOGGER.debug(String.format("Units were instantiated"));}
        else{instantiationMethod(hashMap); LOGGER.debug(String.format("Units is now instantiated"));}
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


    /** Initialize timeseries if it does not exist,
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

    /** Pass in tableNumber, use lolvalues, clear it afterwards by initializing it to empty list, add in new lists, Create timeseries <T> class.
     * @param tableNumber
     * @param dataIRIList
     * @return Timeseries
     */
    private static TimeSeries parseDataToLists(int tableNumber, List<String> dataIRIList) {
        ArrayList<OffsetDateTime> timesList = null;
        List<List<?>> lolvalues = null;
        if (tableNumber==0){
            timesList= accel_tsList;
            lolvalues=accel_lolValues;
            accel_lolValues= new ArrayList<>();
            accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);
        }
        else if (tableNumber==1){
            timesList= gravity_tsList;
            lolvalues= gravity_lolValues;
            gravity_lolValues= new ArrayList<>();
            gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);
        }
        else if (tableNumber==2){
            timesList= lightValue_tsList;
            lolvalues= lightValue_lolValues;
            lightValue_lolValues=new ArrayList<>();
            lightValue_lolValues= Arrays.asList(lightValueList);
        }
        else if (tableNumber==3){
            timesList= location_tsList;
            lolvalues= location_lolValues;
            location_lolValues=new ArrayList<>();
            location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);
        }
        else if (tableNumber==4){
            timesList= magnetometer_tsList;
            lolvalues= magnetometer_lolValues;
            magnetometer_lolValues=new ArrayList<>();
            magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);
        }
        else if (tableNumber==5){
            timesList= dBFS_tsList;
            lolvalues= dBFS_lolValues;
            dBFS_lolValues = new ArrayList<>();
            dBFS_lolValues= Arrays.asList(dBFSList);
        }
        else if (tableNumber==6){
            timesList= brightness_tsList;
            lolvalues= brightness_lolValues;
            brightness_lolValues= new ArrayList<>();
            brightness_lolValues= Arrays.asList(brightnessList);
        }
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
            String dataIRIName =BASEURI+DEVICEID+"_"+tableHeader.get(sensorVariable)+ "_"+ UUID.randomUUID();

            String key = "measure_"+tableHeader.get(sensorVariable);
            hashMap.put(key,dataIRIName);
            dataIRIList.add(dataIRIName);
        }

        String timeUnit = OffsetDateTime.class.getSimpleName();
        List<Class> dataClass;

        //Add geom class when initializing location
        if (tableHeader==locationHeader) {
            List <Class> dataClassInit = (Collections.nCopies(tableHeader.size()-2,Double.class));
            dataClass = new ArrayList<>(dataClassInit);
            dataClass.add(Point.class);
        }else //Dont need to initialize
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
        OffsetDateTime endDataTime;
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.addTimeSeriesData(ts, conn);
            LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }



}
