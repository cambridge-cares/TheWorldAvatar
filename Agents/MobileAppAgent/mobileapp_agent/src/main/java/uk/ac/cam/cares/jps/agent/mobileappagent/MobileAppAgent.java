package uk.ac.cam.cares.jps.agent.mobileappagent;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.*;


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
    public static List<List<String>> tableHeaderList= Arrays.asList(accelerometerHeader,gravityHeader,lightHeader,locationHeader,magnetometerHeader,microphoneHeader);
    public static List<String> tableList = Arrays.asList(accelerometer, gravity,light, location,magnetometer,microphone);

    String dbURL = "jdbc:postgresql://localhost:5432/sensor";
    String user = "postgres";
    String password = "postgres";
    RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
    RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql", "http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql");
    TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
    JSONArray dataArray;
    String Query;

    public void main() {
        //Loop through each table
        for (int i=0; i<tableList.size(); i++){
            Query=getQueryString(i);

            dataArray = rdbStoreClient.executeQuery(Query);

            //Create Timeseries
            List<String> dataIRIList = createTimeSeries(i);

            //GetTimeSeries
            TimeSeries getTimeSeries =parseDataToLists(i, dataArray, dataIRIList);

            //Add timeseries data with tsList
            try (Connection conn = rdbStoreClient.getConnection()){
                tsClient.addTimeSeriesData(getTimeSeries, conn);
            }
            catch (Exception e){
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }

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
        List<List<Double>> lolvalues = new ArrayList<>();


        for (int i = 1; i < tableHeader.size(); i++)  {
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
            String dataIRIName ="https://www.theworldavatar.com/kg/ontotimeseries/MobileAppAgent_"+ tableList.get(tableNumber)+"_"+ tableHeader.get(sensorVariable)+ "_"+ UUID.randomUUID();

            dataIRIList.add(dataIRIName);
        }

        List<Class> dataClass = (Collections.nCopies(tableHeader.size()-1,Double.class));
        String timeUnit = OffsetDateTime.class.getSimpleName();

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
}
