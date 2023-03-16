package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.InstantiationClient.instantiationMethod;


@WebServlet(urlPatterns = "/update")

public class SensorLoggerMobileAppAgent extends JPSAgent {
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

    private static int maxSize = accel_lolValues.size()+magnetometer_lolValues.size()+gravity_lolValues.size()+location_lolValues.size()+dBFS_lolValues.size()+lightValue_lolValues.size()+brightness_lolValues.size();
    private static String DEVICEID;
    private static final long serialVersionUID = 1L;
    private static final String BASEURI = "https://www.theworldavatar.com/kg/sensorloggerapp/";

    private static String smartphoneString;
    private Timer timer;
    private boolean isTimerStarted = false;
    private boolean hasData = false;

    public void init() {
        if (!isTimerStarted) {
            isTimerStarted = true;
            // Start the timer
            timer = new Timer();
            timer.schedule(new instantiationTask(), 2000,5000); // 5000 milliseconds = 5 seconds

            //Create POSTGIS Database in the stack
            PostGISClient postGISClient = PostGISClient.getInstance();
            postGISClient.createDatabase(EnvConfig.DATABASE);
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
        //Readconfig
        readConfig();

        // Send a response back to the client with a 200 status code
        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");

        //Parse incoming data and add them to respective list
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
    private static QueryClientSPARQL queryClientSPARQL;

    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppAgent.class);

    private static String accel_xIRI;
    private static String accel_yIRI;
    private static String accel_zIRI;
    private static String gravity_xIRI;
    private static String gravity_yIRI;
    private static String gravity_zIRI;
    private static String magnetometer_xIRI;
    private static String magnetometer_yIRI;
    private static String magnetometer_zIRI;
    private static String bearingIRI;
    private static String speedIRI;
    private static String altitudeIRI;
    private static String pointIRI;
    private static String dbfsIRI;
    private static String relativeBrightnessIRI;
    private static String light_valueIRI;
    private static ArrayList<OffsetDateTime> timesList;
    private static List<List<?>> lolvalues;
    private static final String timeUnit = OffsetDateTime.class.getSimpleName();
    private HashMap iriHashmap = new HashMap();

    private static void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        downsamplingType= Integer.parseInt(config.getString("downsamplingType"));
        downsamplingRate= Long.valueOf(config.getString("downsamplingRate"));
        timerDelay= Integer.valueOf(config.getString("timerDelay"));
        timerFrequency= Integer.valueOf(config.getString("timerFrequency"));

        EndpointConfig endpointConfig = new EndpointConfig();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClientSPARQL = new QueryClientSPARQL(storeClient);
    }


    private static boolean staticInstantiated = false;
    public void tsInstantiation(){
        smartphoneString=BASEURI+"Smartphone_"+DEVICEID;
        Node smartphoneIRI= NodeFactory.createURI(smartphoneString);
        List tsList = new ArrayList();
        //Accelerometer
        if(queryClientSPARQL.getAccel_xIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getAccel_yIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getAccel_zIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            accel_xIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_x_"+ UUID.randomUUID();
            accel_yIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_y_"+ UUID.randomUUID();
            accel_zIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_z_"+ UUID.randomUUID();

            dataIRIList.add(accel_xIRI);
            dataIRIList.add(accel_yIRI);
            dataIRIList.add(accel_zIRI);
            iriHashmap.put("accel_x", accel_xIRI);
            iriHashmap.put("accel_y", accel_yIRI);
            iriHashmap.put("accel_z", accel_zIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else {
            List<String> dataIRIList = new ArrayList<>();
            accel_xIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getAccel_xIRIArray(smartphoneIRI));
            accel_yIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getAccel_yIRIArray(smartphoneIRI));
            accel_zIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getAccel_zIRIArray(smartphoneIRI));
            dataIRIList.add(accel_xIRI);
            dataIRIList.add(accel_yIRI);
            dataIRIList.add(accel_zIRI);


            accel_lolValues= Arrays.asList(accelList_x,accelList_y,accelList_z);
            timesList= accel_tsList;
            lolvalues=accel_lolValues;

            //Reset List by declaring empty list
            accel_tsList =  new ArrayList<>();
            accelList_x =  new ArrayList<>();
            accelList_y=  new ArrayList<>();
            accelList_z=  new ArrayList<>();
            accel_lolValues= new ArrayList<>();

            TimeSeries accelTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(accelTS);
        }

        //GravitySensor
        if(queryClientSPARQL.getGravity_xIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getGravity_yIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getGravity_zIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            gravity_xIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_x_"+ UUID.randomUUID();
            gravity_yIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_y_"+ UUID.randomUUID();
            gravity_zIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_z_"+ UUID.randomUUID();

            dataIRIList.add(gravity_xIRI);
            dataIRIList.add(gravity_yIRI);
            dataIRIList.add(gravity_zIRI);
            iriHashmap.put("gravity_x", gravity_xIRI);
            iriHashmap.put("gravity_y", gravity_yIRI);
            iriHashmap.put("gravity_z", gravity_zIRI);


            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else {
            List<String> dataIRIList = new ArrayList<>();
            gravity_xIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getGravity_xIRIArray(smartphoneIRI));
            gravity_yIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getGravity_yIRIArray(smartphoneIRI));
            gravity_zIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getGravity_zIRIArray(smartphoneIRI));
            dataIRIList.add(gravity_xIRI);
            dataIRIList.add(gravity_yIRI);
            dataIRIList.add(gravity_zIRI);

            gravity_lolValues= Arrays.asList(gravityList_x,gravityList_y,gravityList_z);
            timesList= gravity_tsList;
            lolvalues= gravity_lolValues;

            //Reset List
            gravity_tsList =  new ArrayList<>();
            gravityList_x =  new ArrayList<>();
            gravityList_y=  new ArrayList<>();
            gravityList_z=  new ArrayList<>();
            gravity_lolValues= new ArrayList<>();

            TimeSeries gravityTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(gravityTS);
        }

        //Magnetometer
        if(queryClientSPARQL.getMagnetometer_xIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getMagnetometer_yIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getMagnetometer_zIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            magnetometer_xIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_x_"+ UUID.randomUUID();
            magnetometer_yIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_y_"+ UUID.randomUUID();
            magnetometer_zIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_z_"+ UUID.randomUUID();

            dataIRIList.add(magnetometer_xIRI);
            dataIRIList.add(magnetometer_yIRI);
            dataIRIList.add(magnetometer_zIRI);
            iriHashmap.put("magnetometer_x", magnetometer_xIRI);
            iriHashmap.put("magnetometer_y", magnetometer_yIRI);
            iriHashmap.put("magnetometer_z", magnetometer_zIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else {
            List<String> dataIRIList = new ArrayList<>();
            magnetometer_xIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getMagnetometer_xIRIArray(smartphoneIRI));
            magnetometer_yIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getMagnetometer_yIRIArray(smartphoneIRI));
            magnetometer_zIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getMagnetometer_zIRIArray(smartphoneIRI));
            dataIRIList.add(magnetometer_xIRI);
            dataIRIList.add(magnetometer_yIRI);
            dataIRIList.add(magnetometer_zIRI);

            magnetometer_lolValues= Arrays.asList(magnetometerList_x,magnetometerList_y,magnetometerList_z);
            timesList= magnetometer_tsList;
            lolvalues= magnetometer_lolValues;

            //Reset List
            magnetometer_tsList =  new ArrayList<>();
            magnetometerList_x =  new ArrayList<>();
            magnetometerList_y=  new ArrayList<>();
            magnetometerList_z=  new ArrayList<>();
            magnetometer_lolValues= new ArrayList<>();

            TimeSeries magnetometerTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(magnetometerTS);
        }

        //GPSDevice
        if(queryClientSPARQL.getBearingIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getSpeedIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getAltitudeIRIArray(smartphoneIRI).isEmpty() && queryClientSPARQL.getPointIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            bearingIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_"+ UUID.randomUUID();
            speedIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_"+ UUID.randomUUID();
            altitudeIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_"+ UUID.randomUUID();
            pointIRI="https://www.theworldavatar.com/kg/sensorloggerapp/point_"+ UUID.randomUUID();
            dataIRIList.add(bearingIRI);
            dataIRIList.add(speedIRI);
            dataIRIList.add(altitudeIRI);
            dataIRIList.add(pointIRI);
            iriHashmap.put("bearing", bearingIRI);
            iriHashmap.put("speed", speedIRI);
            iriHashmap.put("altitude", altitudeIRI);
            iriHashmap.put("point",pointIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size()-1,Double.class);
            List <Class> dataClassWithPoint= new ArrayList<>(dataClass);
            dataClassWithPoint.add(Point.class);
            initTimeSeries(dataIRIList,dataClassWithPoint,timeUnit);

        }else {
            List<String> dataIRIList = new ArrayList<>();
            bearingIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getBearingIRIArray(smartphoneIRI));
            speedIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getSpeedIRIArray(smartphoneIRI));
            altitudeIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getAltitudeIRIArray(smartphoneIRI));
            pointIRI=queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getPointIRIArray(smartphoneIRI));
            dataIRIList.add(bearingIRI);
            dataIRIList.add(speedIRI);
            dataIRIList.add(altitudeIRI);
            dataIRIList.add(pointIRI);

            location_lolValues= Arrays.asList(bearingList,speedList,altitudeList,geomLocationList);
            timesList= location_tsList;
            lolvalues= location_lolValues;

            //Reset List
            location_tsList =  new ArrayList<>();
            bearingList =  new ArrayList<>();
            speedList=  new ArrayList<>();
            altitudeList=  new ArrayList<>();
            geomLocationList=  new ArrayList<>();
            location_lolValues= new ArrayList<>();

            TimeSeries locationTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(locationTS);
        }

        //Microphone
        if(queryClientSPARQL.getSoundPressureLevelIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            dbfsIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/measure_dbfs_"+ UUID.randomUUID();
            dataIRIList.add(dbfsIRI);
            iriHashmap.put("dbfs", dbfsIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else {
            List<String> dataIRIList = new ArrayList<>();
            dbfsIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getSoundPressureLevelIRIArray(smartphoneIRI));
            dataIRIList.add(dbfsIRI);

            dBFS_lolValues= Arrays.asList(dBFSList);
            timesList= dBFS_tsList;
            lolvalues= dBFS_lolValues;

            //Reset List
            dBFS_tsList = new ArrayList<>();
            dBFSList = new ArrayList<>();
            dBFS_lolValues = new ArrayList<>();

            TimeSeries locationTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(locationTS);
        }

        //RelativeBrightness
        if(queryClientSPARQL.getRelativeBrightnessIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            relativeBrightnessIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/relativeBrightness_"+ UUID.randomUUID();
            dataIRIList.add(relativeBrightnessIRI);
            iriHashmap.put("relativeBrightness", relativeBrightnessIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else {
            List<String> dataIRIList = new ArrayList<>();
            relativeBrightnessIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getRelativeBrightnessIRIArray(smartphoneIRI));
            dataIRIList.add(relativeBrightnessIRI);

            brightness_lolValues= Arrays.asList(brightnessList);
            timesList= brightness_tsList;
            lolvalues= brightness_lolValues;

            //Reset List
            brightness_tsList=new ArrayList<>();
            brightnessList = new ArrayList<>();
            brightness_lolValues= new ArrayList<>();

            TimeSeries relativeBrightnessTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(relativeBrightnessTS);
        }

        //Camera
        if(queryClientSPARQL.getIlluminanceIRIArray(smartphoneIRI).isEmpty())
        {
            List<String> dataIRIList = new ArrayList<>();
            light_valueIRI ="https://www.theworldavatar.com/kg/sensorloggerapp/light_value_"+ UUID.randomUUID();
            dataIRIList.add(light_valueIRI);
            iriHashmap.put("light_value", light_valueIRI);

            List<Class> dataClass = Collections.nCopies(dataIRIList.size(),Double.class);
            initTimeSeries(dataIRIList,dataClass,timeUnit);
        }else{
            List<String> dataIRIList = new ArrayList<>();
            light_valueIRI =queryClientSPARQL.getIRIfromJSONarray(queryClientSPARQL.getIlluminanceIRIArray(smartphoneIRI));
            dataIRIList.add(light_valueIRI);

            lightValue_lolValues= Arrays.asList(lightValueList);
            timesList= lightValue_tsList;
            lolvalues= lightValue_lolValues;

            //Reset list
            lightValue_tsList=new ArrayList<>();
            lightValueList =new ArrayList<>();
            lightValue_lolValues=new ArrayList<>();

            TimeSeries lightValueTS = new TimeSeries(timesList, dataIRIList, lolvalues);
            tsList.add(lightValueTS);
        }

        

        if (iriHashmap.size()==maxSize && !staticInstantiated){
            iriHashmap.put("deviceID", smartphoneString);
            instantiationMethod(iriHashmap);
            LOGGER.info(String.format("Units is now instantiated"));
            staticInstantiated = true;
        }else if (iriHashmap.size()==maxSize && staticInstantiated)
        {LOGGER.info(String.format("Units has been instantiated, InstantiationClient did not run"));}
        else if (iriHashmap.size()!=maxSize)
        { LOGGER.debug(String.format("Not all measuresIRIs are collected, InstantiationClient did not run"));}
        else
        {LOGGER.debug(String.format("Static relations was not instantiated"));}

        //
        if (tsList.size() != 7){
            System.out.println("It did not get all 7 timeseries");
            bulkAddTimeSeriesData(tsList);
        }else {
            System.out.println("All 7 timeseries has been collected");
            bulkAddTimeSeriesData(tsList);
        }
    }

    private void bulkAddTimeSeriesData(List<TimeSeries> tsList){
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.bulkaddTimeSeriesData(tsList, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    private void initTimeSeries(List<String> dataIRIList, List dataClass, String timeUnit){

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.initTimeSeries(dataIRIList, dataClass, timeUnit, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }
}