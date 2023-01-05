package org.example;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
import java.sql.Time;
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

    TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);

    String tableName = "Light";
    ArrayList<List> dataArrayList;
    JSONArray dataArray;

    String Query="SELECT timestamp,light_value FROM public.light";



    @Test
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

        dataArray = rdbStoreClient.executeQuery(Query);

        JSONObject temp;
        List<String> timesList = new ArrayList<>();
        List<Double> lightValueList = new ArrayList<>();
        List<List<Double>> values =new ArrayList<>();
        Double lightValue;
        String timestamp;

        for (int i = 0; i < dataArray.length(); i++){
            lightValue = dataArray.getJSONObject(i).getDouble("light_value");
            timestamp = dataArray.getJSONObject(i).get("timestamp").toString();

            lightValueList.add(lightValue);
            timesList.add(timestamp);
        }
        values.add(lightValueList);

        TimeSeries ts = new TimeSeries(timesList, dataIRI, values);

        try (Connection conn = rdbStoreClient.getConnection()){
            tsClient.addTimeSeriesData(ts, conn);
        }
        catch (Exception e){
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

    }