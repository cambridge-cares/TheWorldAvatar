package uk.ac.cam.cares.jps.agent.mobileappagent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.exception.DataAccessException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.Point;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.util.*;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatterBuilder;

//To run this agent, run curl -s http://localhost:8080/MobileAppAgent-1.0-SNAPSHOT/performTS/ in command prompt
@Controller
@WebServlet(urlPatterns = {"/performTS"})
public class MobileAppAgent extends JPSAgent {

    //Declare tables
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
    public static final String longitude = "longitude";
    public static final String latitude = "latitude";
    public static final String magnetometer_x = "magnetometer_x";
    public static final String magnetometer_y = "magnetometer_y";
    public static final String magnetometer_z = "magnetometer_z";
    public static final String dbfs = "dbfs";

    //Declare tableHeader as list of strings
    public static List<String> accelerometerHeader = Arrays.asList(timestamp, accel_x, accel_y, accel_z);
    public static List<String> gravityHeader = Arrays.asList(timestamp, gravity_x, gravity_y, gravity_z);
    public static List<String> lightHeader = Arrays.asList(timestamp, light_value);
    public static List<String> locationHeader = Arrays.asList(timestamp, bearing, speed, altitude, longitude, latitude);
    public static List<String> magnetometerHeader = Arrays.asList(timestamp, magnetometer_x, magnetometer_y, magnetometer_z);
    public static List<String> microphoneHeader = Arrays.asList(timestamp, dbfs);
//    public static List<List<String>> tableHeaderList= Arrays.asList(accelerometerHeader,gravityHeader,lightHeader,locationHeader,magnetometerHeader,microphoneHeader);
    public static List<List<String>> tableHeaderList= Arrays.asList(locationHeader);
//    public static List<String> tableList = Arrays.asList(accelerometer, gravity,light, location,magnetometer,microphone);
    public static List<String> tableList = Arrays.asList( location);

    private static final String dbURL = "jdbc:postgresql://localhost:5432/developing";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
    private static RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/developing/sparql", "http://127.0.0.1:9999/blazegraph/namespace/developing/sparql");
    private static TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
    private JSONArray dataArray;
    private String Query;
    private static final String BASEURI = "https://www.theworldavatar.com/kg/measure_";

    private static final Logger LOGGER = LogManager.getLogger(MobileAppAgent.class);
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    public static Point point = new Point();


    /**
     * Processes HTTP requests with originating details.
     * @param requestParams Request parameters in a JSONObject.
     * @param request HTTP Servlet Request.
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    /**
     *
     * Processes HTTP requests.
     * @param requestParams Request parameters as a JSONObject.
     * @return response in JSON format.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            //Loop through each table
            for (int i = 0; i < tableList.size(); i++) {

                Query = getQueryString(i);
                dataArray = rdbStoreClient.executeQuery(Query);


                if (checkIfTimeSeriesExists(i))//Check if dbTable exists
                {
                    String IRIQuery;

                    IRIQuery = getDataIRIFromDBTable(i);
                    JSONArray dataIRIArray = rdbStoreClient.executeQuery(IRIQuery);

                    //Get the newest timeseries
                    TimeSeries getTimeSeries = parseDataToLists(i, dataArray, parseJSONArrayToList(dataIRIArray));
                    updateData(getTimeSeries);
                }
                else  //When time series does not exist create timeseries
                {
                    initTimeseriesIfNotExist(i,dataArray);
                }
            }
        }
        return requestParams;
    }

    /**
     * Checks the incoming JSON request for validity.
     * @param requestParams JSON request parameters.
     * @return request validity
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean valid = true;
        return valid;
    }


    public void Test() {
        //Loop through each table
        for (int i = 0; i < tableList.size(); i++) {
            
            Query = getQueryString(i);
            dataArray = rdbStoreClient.executeQuery(Query);


            if (checkIfTimeSeriesExists(i))//Check if dbTable exists
            {
                String IRIQuery;

                IRIQuery = getDataIRIFromDBTable(i);
                JSONArray dataIRIArray = rdbStoreClient.executeQuery(IRIQuery);

                //Get the newest timeseries
                TimeSeries getTimeSeries = parseDataToLists(i, dataArray, parseJSONArrayToList(dataIRIArray));
                updateData(getTimeSeries);
            }
            else  //When time series does not exist create timeseries
            {
                initTimeseriesIfNotExist(i,dataArray);
            }
        }
    }

    /**
     *
     * @param i
     * @return
     */



    private boolean checkIfTimeSeriesExists(int i){
        String IRIQuery;
        try {
            IRIQuery = getDataIRIFromDBTable(i);
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



    /**
     *
     * @param i
     * @param dataArray
     */
    private void initTimeseriesIfNotExist(int i , JSONArray dataArray){
        //Create Timeseries
        List<String> dataIRIList = createTimeSeries(i);

        //GetTimeSeries
        TimeSeries getTimeSeries = parseDataToLists(i, dataArray, dataIRIList);

        //Add timeseries data with tsList
        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.addTimeSeriesData(getTimeSeries, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }


    /**
     * @param tableNumber
     * @param dataArray
     * @param dataIRIList
     * @return
     */
    private TimeSeries parseDataToLists(int tableNumber, JSONArray dataArray, List<String> dataIRIList) {
        List<TimeSeries<Double>> tsList = new ArrayList<>();
        List<String> timesList = new ArrayList<>();
        List<Double> valueList = new ArrayList<>();
        List tableHeader= tableHeaderList.get(tableNumber);
        List<List<Object>> lolvalues = new ArrayList<>();
        List<Point> pointList=new ArrayList<>();

        //Initialize arraylist
        for (int i = 1; i < tableHeader.size(); i++)  {
            lolvalues.add(new ArrayList<>());
        }
        if (tableHeader==locationHeader) {
            lolvalues.add(new ArrayList<>());
        }


        Double value;
        String timestamp;

        //iterate through row, i is a row
        for (int row = 0; row < dataArray.length(); row++) {

            timestamp = dataArray.getJSONObject(row).get("timestamp").toString();
            timesList.add(timestamp);

            //Another for loop here to get values, return lolvalues
            for (int column = 1; column < tableHeader.size();column++){
                value = dataArray.getJSONObject(row).getDouble((tableHeader.get(column)).toString());
                valueList.add(value);
                lolvalues.get(column-1).addAll(valueList);
                valueList.removeAll(valueList);
            }

            //Convert Lat and Long into Class Point
            if (tableHeader==locationHeader) {
                Double Long = dataArray.getJSONObject(row).getDouble((tableHeader.get(4)).toString());
                Double Lat = dataArray.getJSONObject(row).getDouble((tableHeader.get(5)).toString());

                point = new Point(Long,Lat);
                point.setSrid(4326);
                pointList.add(point);
                lolvalues.get(locationHeader.size()-1).addAll(pointList);
                pointList.removeAll(pointList);
            }
        }
        //Pass time list, dataIRI List - just one, lolvalues, add timeseries to output
        return new TimeSeries(timesList, dataIRIList, lolvalues);
    }

    /**
     * @param tableNumber
     * @return
     */
    private List<String> createTimeSeries(int tableNumber) {
        List tableHeader= tableHeaderList.get(tableNumber);
        List<String> dataIRIList = new ArrayList<>();;

        //Create dataIRI for each variable
        for (int sensorVariable = 1; sensorVariable < tableHeader.size() ;sensorVariable++){
            String dataIRIName =BASEURI+ tableHeader.get(sensorVariable)+ "_"+ UUID.randomUUID();

            dataIRIList.add(dataIRIName);
        }



        List <Class> dataClasInit = (Collections.nCopies(tableHeader.size()-1,Double.class));
        List<Class> dataClass = new ArrayList<>(dataClasInit);
        String timeUnit = OffsetDateTime.class.getSimpleName();

        //Add Geom dataIRI when intiliazing timeseries
        if (tableHeader==locationHeader) {
            String dataIRIName =BASEURI+ "geom"+ "_"+ UUID.randomUUID();
            dataIRIList.add(dataIRIName);
            dataClass.add(Point.class);

        }

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.initTimeSeries(dataIRIList, dataClass, timeUnit, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return dataIRIList;
    }

    /**
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


    /**
     * @param i
     * @return
     */
    private static String getDataIRIFromDBTable(int i){
        String query;

        query = "SELECT \"dataIRI\" FROM public.\"dbTable\" WHERE \"dataIRI\" LIKE ";
        for (int b = 1; b < tableHeaderList.get(i).size(); b++){
            if (b==1){ query =query+"'%"+tableHeaderList.get(i).get(b)+"%'";}
            else {query = query + " OR \"dataIRI\" LIKE "+"'%"+tableHeaderList.get(i).get(b)+"%'";}
        }

        return query;
    }
    /**
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

    /**
     * @param ts
     * @throws IllegalArgumentException
     */
    public void updateData(TimeSeries <OffsetDateTime> ts) throws IllegalArgumentException {
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


                OffsetDateTime startCurrentTime = convertStringToOffsetDateTime(String.valueOf(ts.getTimes().get(0)));

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
    private TimeSeries<OffsetDateTime> pruneTimeSeries(TimeSeries<OffsetDateTime> timeSeries, OffsetDateTime timeThreshold) {
        // Find the index from which to start
        List<OffsetDateTime> times = timeSeries.getTimes();
        int index = 0;
        while(index < times.size()) {
            if (convertStringToOffsetDateTime(String.valueOf(times.get(index))).isAfter(timeThreshold)) {
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
     * @param timestamp
     * @return
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp)  {

        timestamp=timestamp.replace("Z","");

        DateTimeFormatter formatter = new DateTimeFormatterBuilder()
                // date / time
                .appendPattern("yyyy-MM-dd HH:mm:ss")
                // nanoseconds, with minimum 1 and maximum 9 digits and a decimal point
                .appendFraction(ChronoField.NANO_OF_SECOND, 1, 9, true)
                // create formatter
                .toFormatter();
        LocalDateTime localTime = LocalDateTime.parse(timestamp, formatter);


        // Then add the zone id
        return OffsetDateTime.of(localTime, MobileAppAgent.ZONE_OFFSET);
    }

}
