package org.example;

import org.json.JSONArray;
import org.json.JSONObject;
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

    String dbURL = "jdbc:postgresql://localhost:5432/sensor";
    String user = "postgres";
    String password = "postgres";
    RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
    RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql", "http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql");
    List<String> dataIRI = Arrays.asList("https://www.theworldavatar.com/kg/ontotimeseries/MobileAppAgent_Light_"+UUID.randomUUID());
    List<Class> dataClass = Arrays.asList(Double.class);
    String timeUnit = OffsetDateTime.class.getSimpleName();

    TimeSeriesClient tsClient;




    public void main()
    {

        try (Connection conn = rdbStoreClient.getConnection()){
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.initTimeSeries(dataIRI, dataClass, timeUnit, conn);
        }
        catch (Exception e)
        {
            System.out.println("Something went wrong.");
        }
    }

    String tableName = "Light";
    ArrayList<List> dataArrayList;
    JSONArray dataArray;
    String Query="SELECT timestamp,light_value FROM public.light";




    public void addData(){

        dataArray = rdbStoreClient.executeQuery(Query);
        dataArrayList = parseDataToLists(dataArray);
        try (Connection conn = rdbStoreClient.getConnection()){
   //         tsClient.bulkaddTimeSeriesData(dataArrayList.get(1), conn);
            tsClient.bulkaddTimeSeriesData(dataArrayList.get(1), conn);
        }
        catch (Exception e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }

    public ArrayList<List> parseDataToLists(JSONArray dataArray) {
        ArrayList<List> output = new ArrayList<>();

        JSONObject temp;
        List<TimeSeries<Double>> tsList = new ArrayList<>();
        List<List<String>> dataIRI = new ArrayList<>();
        List<?> times = Collections.nCopies(1, null);

        for (int i = 0; i < dataArray.length(); i++){
            temp = dataArray.getJSONObject(i);
            dataIRI.add(Arrays.asList());
            tsList.add(new TimeSeries(times, dataIRI.get(i), Arrays.asList()));
        }

        output.add(dataIRI);
        output.add(tsList);

        return output;
    }





//    public String getQueryString(String tableName, Integer start, Integer end) {
//        String query;
//
//        query = "SELECT " + ;
//
//        for (int i = 0; i < TIME_SERIES.size(); i++){
//            query = query + ", " + TIME_SERIES.get(i);
//        }
//
//        query = query + " FROM \"" + tableName + "\" ";
//
//        return query + "WHERE " + KEY_OID + " BETWEEN " + start + " AND " + end;
//    }















//    /**
//     * Construct query as a string for Solarkataster time series parameters
//     * @param tableName name of table for which to query Solarkataster data from
//     * @param start starting oid
//     * @param end ending oid
//     * @return SQL query as a string
//     */
//    private String getQueryString(String tableName, Integer start, Integer end) {
//        String query;
//
//        query = "SELECT " + KEY_MOD;
//
//        for (int i = 0; i < TIME_SERIES.size(); i++){
//            query = query + ", " + TIME_SERIES.get(i);
//        }
//
//        query = query + " FROM \"" + tableName + "\" ";
//
//        return query + "WHERE " + KEY_OID + " BETWEEN " + start + " AND " + end;
//    }
//
//    /**
//     * Constructs and returns a list of doubles of the TIME_SERIES parameters value in data
//     * @param data SQL query response for one building
//     * @return List of Double
//     */
//    private List<Double> getDoubleList (JSONObject data) {
//        List<Double> dataList = new ArrayList<>();
//
//        for (int i = 0; i < TIME_SERIES.size(); i++){
//            if(data.has(TIME_SERIES.get(i))){
//                dataList.add(data.getDouble(TIME_SERIES.get(i)));
//            }
//            else{
//                dataList.add(Double.NaN);
//            }
//        }
//
//        return dataList;
//    }
//
//    /**
//     * Initialise time series that will be be associated with dataIRI
//     * @param dataIRI IRIs to which the timeseriesIRI will be associated to
//     */
//    private void createTimeSeries(List<List<String>> dataIRI) {
//        int n = dataIRI.size();
//
//        List<List<Class<?>>> dataClass = Collections.nCopies(n, Arrays.asList(Double.class));
//        List<String> timeUnit = Collections.nCopies(n, null);
//        List<TimeSeriesClient.Type> type = Collections.nCopies(n, TimeSeriesClient.Type.AVERAGE);
//        List<Duration> durations = Collections.nCopies(n, Duration.ofDays(31));
//        List<ChronoUnit> units = Collections.nCopies(n, ChronoUnit.MONTHS);
//
//        try (Connection conn = tsRDBStoreClient.getConnection()){
//            tsClient.bulkInitTimeSeries(dataIRI, dataClass, timeUnit, conn, type, durations, units);
//        }
//        catch (Exception e){
//            e.printStackTrace();
//            throw new JPSRuntimeException(e);
//        }
//    }
//
//    /**
//     * Parse dataArray into dataIRIs and corresponding TimeSeries
//     * @param dataArray JSONArray of SQL query results
//     * @return ArrayList of dataIRIs and TimeSeries
//     */
//    private ArrayList<List> parseDataToLists(JSONArray dataArray) {
//        ArrayList<List> output = new ArrayList<>();
//
//        JSONObject temp;
//        List<TimeSeries<Double>> tsList = new ArrayList<>();
//        List<List<String>> dataIRI = new ArrayList<>();
//        List<?> times = Collections.nCopies(TIME_SERIES.size(), null);
//
//        for (int i = 0; i < dataArray.length(); i++){
//            temp = dataArray.getJSONObject(i);
//            dataIRI.add(Arrays.asList(ubemSolar + "_" + temp.getString(KEY_MOD)));
//            tsList.add(new TimeSeries(times, dataIRI.get(i), Arrays.asList(getDoubleList(temp))));
//        }
//
//        output.add(dataIRI);
//        output.add(tsList);
//
//        return output;
//    }
}