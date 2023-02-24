package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.downsampling;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


import org.postgis.Point;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.Statement;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.InstantiationClient.instantiationMethod;


@WebServlet("/mb")

public class SensorLoggerMobileAppServlet extends JPSAgent {
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
    private static final long serialVersionUID = 1L;

    private Timer timer;
    private boolean isTimerStarted = false;
    private boolean hasData = false;

    public void init() {
        if (!isTimerStarted) {
            isTimerStarted = true;
            // Start the timer
            timer = new Timer();
            timer.schedule(new instantiationTask(), 2000,5000); // 5000 milliseconds = 5 seconds

        }

    }


    class instantiationTask extends TimerTask {
        public void run() {
            // This code will be executed when the timer expires
            try{
                if(hasData) {
                    tsInstantiation();
                    System.out.println("Timer expired, timeseries instantiated!");
                    hasData=false;
                }else
                {
                    System.out.println("Timer expired, but no timeseries data, so no timeseries were instantiated");
                }

            }catch (Exception e){
                System.out.println("Timeseries not instantiated, exception caught");
            }
//            isTimerStarted = false; // Reset the flag variable
        }
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        readConfig();
        // Parse the HTTP POST request
        // Do whatever processing you need to do with the request data

        // Send a response back to the client with a 200 status code
        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");


        StringBuilder data = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(request.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                data.append("[");
                data.append(line);
                data.append("]");

                JSONArray dataHTTP = new JSONArray(data.toString());
                ObjectMapper mapper = new ObjectMapper();
                JsonNode root = mapper.readTree(String.valueOf(dataHTTP));

                JsonNode payload = null;
                for (JsonNode webhook : root) {
                    Integer messageId = webhook.get("messageId").intValue();
                    String sessionId = webhook.get("sessionId").textValue();
                    String deviceId = webhook.get("deviceId").textValue();
                    DEVICEID = deviceId;
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
            }

        }

        // Log the request data
        System.out.println("Received POST request with data");
        hasData=true;


        // Build the response string
        String responseString = "Received POST request with data:\n" + data;

        // Set the content type of the response
        response.setContentType("text/plain");

        // Write the response string to the output stream
        out.print(responseString);
        out.flush();
    }

    public void destroy() {
        // Cancel the timer when the servlet is destroyed
        timer.cancel();
    }

//    /**
//     * Local server
//     */
//    private static final String BASEURI = "https://www.theworldavatar.com/kg/measure_";
//    private static final String dbURL = "jdbc:postgresql://localhost:5432/develop";
//    private static final String user = "postgres";
//    private static final String password = "postgres";
//    private static RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
//    private static RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/test/sparql", "http://127.0.0.1:9999/blazegraph/namespace/test/sparql");
//    private static TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
//
//    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppServlet.class);
//
//    private static Long downsamplingRate;
//    private static int downsamplingType;
//    private static int timerDelay;
//    private static int timerFrequency;
//
//    private static void readConfig() {
//        ResourceBundle config = ResourceBundle.getBundle("config");
//        downsamplingType = Integer.parseInt(config.getString("downsamplingType"));
//        downsamplingRate = Long.valueOf(config.getString("downsamplingRate"));
//        timerDelay = Integer.valueOf(config.getString("timerDelay"));
//        timerFrequency = Integer.valueOf(config.getString("timerFrequency"));
//    }

    /**
     * Stack server
     */

    private static Long downsamplingRate;
    private static int downsamplingType;
    private static int timerDelay;
    private static int timerFrequency;

    private static String dbURL;
    private static String user;
    private static String password;
    private static  String timeseriesDBUrl;
    private static String tsUser;
    private static String tsPassword;
    private static TimeSeriesClient tsClient;
    private static RemoteRDBStoreClient tsRDBStoreClient;
    private static RemoteRDBStoreClient rdbStoreClient;
    private static RemoteStoreClient storeClient;

    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppServlet.class);
    private static final String BASEURI = "https://www.theworldavatar.com/kg/measure_";

    private static void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        downsamplingType= Integer.parseInt(config.getString("downsamplingType"));
        downsamplingRate= Long.valueOf(config.getString("downsamplingRate"));
        timerDelay= Integer.valueOf(config.getString("timerDelay"));
        timerFrequency= Integer.valueOf(config.getString("timerFrequency"));


        dbURL = config.getString("db.url");
        user = config.getString("db.user");
        password = config.getString("db.password");
        timeseriesDBUrl = config.getString("timeseriesDB.url");
        tsUser = config.getString("ts.user");
        tsPassword = config.getString("ts.password");

        rdbStoreClient = new RemoteRDBStoreClient(dbURL, "postgres", "admin");
//        rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
        storeClient = new RemoteStoreClient(config.getString("timeseries.query.endpoint"), config.getString("timeseries.update.endpoint"));
        tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
        tsRDBStoreClient = new RemoteRDBStoreClient(timeseriesDBUrl, tsUser, tsPassword);
    }






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
    public void tsInstantiation() throws Exception {
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

                if (!getTimeSeries.getTimes().isEmpty()) {
                    //Skip location table
                    if (i != 3) {
                        getTimeSeries = downsampling.aggregation((getTimeSeries), downsamplingRate, downsamplingType);
                    }
                    updateData(getTimeSeries);
                    System.out.println("Timeseries has been updated");

                    unitsWereInstantiated = true;
                }
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
    private static void initTimeseriesIfNotExist(int i, HashMap hashMap) throws Exception {
        //Create Timeseries
        List<String> dataIRIList = createTimeSeries(i, hashMap);

        //GetTimeSeries
        TimeSeries getTimeSeries = parseDataToLists(i, dataIRIList);
        getTimeSeries= downsampling.aggregation((getTimeSeries), downsamplingRate, downsamplingType);

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
            accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);
            timesList= accel_tsList;
            lolvalues=accel_lolValues;

            //Reset List by declaring empty list
            accel_tsList =  new ArrayList<>();
            accelList_x =  new ArrayList<>();
            accelList_y=  new ArrayList<>();
            accelList_z=  new ArrayList<>();
            accel_lolValues= new ArrayList<>();
        }
        else if (tableNumber==1){
            gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);
            timesList= gravity_tsList;
            lolvalues= gravity_lolValues;


            //Reset List
            gravity_tsList =  new ArrayList<>();
            gravityList_x =  new ArrayList<>();
            gravityList_y=  new ArrayList<>();
            gravityList_z=  new ArrayList<>();
            gravity_lolValues= new ArrayList<>();
        }
        else if (tableNumber==2){
            lightValue_lolValues= Arrays.asList(lightValueList);
            timesList= lightValue_tsList;
            lolvalues= lightValue_lolValues;


            lightValue_tsList=new ArrayList<>();
            lightValueList =new ArrayList<>();
            lightValue_lolValues=new ArrayList<>();

        }
        else if (tableNumber==3){
            location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);
            timesList= location_tsList;
            lolvalues= location_lolValues;

            location_tsList=new ArrayList<>();
            bearingList=new ArrayList<>();
            speedList=new ArrayList<>();
            altitudeList=new ArrayList<>();
            geomLocationList=new ArrayList<>();
            location_lolValues=new ArrayList<>();

        }
        else if (tableNumber==4){
            magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);
            timesList= magnetometer_tsList;
            lolvalues= magnetometer_lolValues;

            magnetometer_tsList=new ArrayList<>();
            magnetometerList_x=new ArrayList<>();
            magnetometerList_y=new ArrayList<>();
            magnetometerList_z=new ArrayList<>();
            magnetometer_lolValues=new ArrayList<>();

        }
        else if (tableNumber==5){
            dBFS_lolValues= Arrays.asList(dBFSList);
            timesList= dBFS_tsList;
            lolvalues= dBFS_lolValues;

            dBFS_tsList = new ArrayList<>();
            dBFSList = new ArrayList<>();
            dBFS_lolValues = new ArrayList<>();
        }
        else if (tableNumber==6){
            brightness_lolValues= Arrays.asList(brightnessList);
            timesList= brightness_tsList;
            lolvalues= brightness_lolValues;


            brightness_tsList=new ArrayList<>();
            brightnessList = new ArrayList<>();
            brightness_lolValues= new ArrayList<>();

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
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }
}