//package org.example;
//
//import org.json.JSONArray;
//import org.json.JSONObject;
//import org.springframework.test.context.TestExecutionListeners;
//import uk.ac.cam.cares.jps.base.agent.JPSAgent;
//import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
//import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
//import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
//import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
//import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
//
//import java.sql.Connection;
//import java.sql.Time;
//import java.time.OffsetDateTime;
//import java.util.*;
//
//
//public class MobileAppAgent extends JPSAgent {
//
//    String dbURL = "jdbc:postgresql://localhost:5432/sensor";
//    String user = "postgres";
//    String password = "postgres";
//    RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
//    RemoteStoreClient storeClient = new RemoteStoreClient("http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql", "http://127.0.0.1:9999/blazegraph/namespace/sensor/sparql");
//    List<String> dataIRI = Arrays.asList("https://www.theworldavatar.com/kg/ontotimeseries/MobileAppAgent_Light_"+UUID.randomUUID());
//    List<Class> dataClass = Arrays.asList(Double.class);
//    String timeUnit = OffsetDateTime.class.getSimpleName();
//
//    TimeSeriesClient tsClient;
//
//    String tableName = "Light";
//    ArrayList<List> dataArrayList;
//
//    String Query="SELECT timestamp,light_value FROM public.light";
//
//
//
//    public void main()
//    {
//
//        try (Connection conn = rdbStoreClient.getConnection()){
//            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
//            tsClient.initTimeSeries(dataIRI, dataClass, timeUnit, conn);
//        }
//        catch (Exception e)
//        {
//            System.out.println("Something went wrong.");
//        }
//    }
//
//
//    public void addData(String Query,List<String> dataIRI){
//
//        JSONArray dataArray;
//        dataArray = rdbStoreClient.executeQuery(Query);
//        TimeSeries ts = parseDataToTimeSeries(dataArray, dataIRI);
//
//        try (Connection conn = rdbStoreClient.getConnection()){
//            tsClient.addTimeSeriesData(ts, conn);
//        }
//        catch (Exception e){
//            e.printStackTrace();
//            throw new JPSRuntimeException(e);
//        }
//
//    }
//
//    public TimeSeries parseDataToTimeSeries (JSONArray dataArray, List<String> dataIRI){
//        JSONObject temp;
//        List <String> timesList = Arrays.asList() ;
//        List <Double> lightValueList = Arrays.asList();
//        Double lightValue;
//        String timestamp;
//
//        for (int i = 0; i < dataArray.length(); i++){
//            timestamp = dataArray.getJSONObject(i).getString("timestamp");
//            lightValue = dataArray.getJSONObject(i).getDouble("light_value");
//            timesList.add(timestamp);
//            lightValueList.add(lightValue);
//        }
//        return new TimeSeries(timesList, dataIRI, lightValueList);
//    }
//}