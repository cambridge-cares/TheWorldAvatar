//package uk.ac.cam.cares.jps.sensor;
//
//import android.content.Context;
//
//import static org.hamcrest.CoreMatchers.any;
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertNotNull;
//import static org.junit.Assert.assertTrue;
//
//import android.content.Context;
//import android.content.Intent;
//import android.util.Log;
//
//import androidx.room.Room;
//import androidx.test.core.app.ApplicationProvider;
//import androidx.test.ext.junit.runners.AndroidJUnit4;
//
//import com.android.volley.RequestQueue;
//import com.android.volley.toolbox.StringRequest;
//import com.android.volley.toolbox.Volley;
//
//import org.json.JSONArray;
//import org.json.JSONException;
//import org.json.JSONObject;
//import org.junit.After;
//import org.junit.Before;
//import org.junit.Test;
//import org.junit.runner.RunWith;
//
//import java.io.IOException;
//import java.util.HashMap;
//import java.util.List;
//import java.util.Map;
//import java.util.concurrent.ExecutorService;
//import java.util.concurrent.Executors;
//import java.util.concurrent.Future;
//
//import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
//import uk.ac.cam.cares.jps.sensor.source.database.model.TestDatabase;
//import uk.ac.cam.cares.jps.sensor.source.database.model.dao.UnsentDataDao;
//import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;
//import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
//import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
//
//@RunWith(AndroidJUnit4.class)
//public class UnsentDataTest {
//
//    private TestDatabase db;
//    private UnsentDataDao unsentDataDao;
//    private SensorNetworkSource sensorNetworkSource;
//    private SensorLocalSource sensorLocalSource;
//    private RequestQueue requestQueue;
//
//    @Before
//    public void createDb() {
//        Context context = ApplicationProvider.getApplicationContext();
//        db = Room.inMemoryDatabaseBuilder(context, TestDatabase.class).build();
//        unsentDataDao = db.unsentDataDao();
//
//        requestQueue = Volley.newRequestQueue(context);
//        sensorNetworkSource = new SensorNetworkSource(context, requestQueue);
//        sensorLocalSource = new SensorLocalSource(context);
//    }
//
//    @After
//    public void closeDb() throws IOException {
//        db.close();
//    }
//
//    @Test
//    public void testUnsentDataStoredOnNetworkFailure() throws Exception {
//        // Simulate sensor data
//        JSONArray sensorData = new JSONArray();
//        JSONObject sensorDataObject = new JSONObject();
//        sensorDataObject.put("type", "location");
//        sensorDataObject.put("time", 2392392839L);
//        JSONObject valuesObject = new JSONObject();
//        valuesObject.put("latitude", 52.205);
//        valuesObject.put("longitude", 0.119);
//        sensorDataObject.put("values", valuesObject);
//        sensorData.put(sensorDataObject);
//
//
//        sensorNetworkSource.sendPostRequest("device123", sensorData);
//
//        // Verify that the data is stored in the unsent_data table
//        List<UnsentData> unsentDataList = unsentDataDao.getAllUnsentData();
//        assertEquals(1, unsentDataList.size());
//        assertEquals("device123", unsentDataList.get(0).deviceId);
//    }
//
//    @Test
//    public void testResendUnsentDataWhenNetworkIsAvailable() throws Exception {
//        // Simulate unsent data by manually inserting it into the database
//        UnsentData unsentData = new UnsentData();
//        unsentData.deviceId = "device123";
//        unsentData.data = "{\"location\":[{\"time\":2392392839,\"values\":{\"latitude\":52.205,\"longitude\":0.119}}]}";
//        unsentData.timestamp = System.currentTimeMillis();
//        unsentDataDao.insert(unsentData);
//
//
//        NetworkChangeReceiver networkChangeReceiver = new NetworkChangeReceiver(sensorLocalSource, sensorNetworkSource);
//        Intent intent = new Intent();
//        intent.putExtra("deviceId", "device123");
//
//
//        networkChangeReceiver.onReceive(ApplicationProvider.getApplicationContext(), intent);
//
//
//        Thread.sleep(2000);
//
//        // Verify that the unsent data was re-uploaded and then deleted from the database
//        List<UnsentData> remainingUnsentData = unsentDataDao.getAllUnsentData();
//        assertTrue(remainingUnsentData.isEmpty());
//    }
//
//    @Test
//    public void testUnsentDataStoredOnFailure() throws Exception {
//        Thread testThread = new Thread(() -> {
//            try {
//
//                JSONArray sensorData = new JSONArray();
//                JSONObject sensorDataObject = new JSONObject();
//                sensorDataObject.put("type", "location");
//                sensorDataObject.put("time", 2392392839L);
//                JSONObject valuesObject = new JSONObject();
//                valuesObject.put("latitude", 52.205);
//                valuesObject.put("longitude", 0.119);
//                sensorDataObject.put("values", valuesObject);
//                sensorData.put(sensorDataObject);
//
//
//                sensorNetworkSource.sendPostRequest("device123", sensorData);
//
//
//                List<UnsentData> unsentDataList = sensorLocalSource.retrieveUnsentData();
//
//
//                assertEquals(1, unsentDataList.size());
//                assertEquals("device123", unsentDataList.get(0).deviceId);
//
//            } catch (Exception e) {
//                e.printStackTrace();
//                System.out.println("Test failed due to exception: " + e.getMessage());
//            }
//        });
//
//        testThread.start();
//        testThread.join();
//    }
//
//    @Test
//    public void testUnsentDataStoredOnNetworkFailure2() throws Exception {
//        // Simulate sensor data
//        JSONArray sensorData = new JSONArray();
//        JSONObject sensorDataObject = new JSONObject();
//        sensorDataObject.put("type", "location");
//        sensorDataObject.put("time", 2392392839L);
//        JSONObject valuesObject = new JSONObject();
//        valuesObject.put("latitude", 52.205);
//        valuesObject.put("longitude", 0.119);
//        sensorDataObject.put("values", valuesObject);
//        sensorData.put(sensorDataObject);
//
//        // Use an ExecutorService to run the database operation on a background thread
//        ExecutorService executor = Executors.newSingleThreadExecutor();
//
//        // Execute the network request and database operation
//        Future<?> future = executor.submit(() -> {
//            sensorNetworkSource.sendPostRequest("device123", sensorData);
//        });
//
//        // Wait for the operation to complete
//        future.get();
//
//        // Check the database for the unsent data
//        Future<List<UnsentData>> resultFuture = executor.submit(() -> {
//            return sensorLocalSource.retrieveUnsentData();
//        });
//
//        List<UnsentData> unsentDataList = resultFuture.get();
//
//        assertEquals(1, unsentDataList.size());
//        assertEquals("device123", unsentDataList.get(0).deviceId);
//
//        // Shutdown the executor service
//        executor.shutdown();
//    }
//
//    @Test
//    public void testSerializeMap_WithValidInput() throws JSONException {
//
//        // Prepare input data
//        Map<String, JSONArray> map = new HashMap<>();
//        JSONArray locationArray = new JSONArray();
//        JSONObject locationObject = new JSONObject();
//        locationObject.put("latitude", 52.205);
//        locationObject.put("longitude", 0.119);
//        locationArray.put(locationObject);
//        map.put("location", locationArray);
//
//        JSONArray lightArray = new JSONArray();
//        JSONObject lightObject = new JSONObject();
//        lightObject.put("lux", 60.01);
//        lightArray.put(lightObject);
//        map.put("light", lightArray);
//
//        // Call the method under test
//        String serializedString = sensorNetworkSource.serializeMap(map);
//
//        Log.d("TestLong", serializedString);
//        // Verify the result
//        assertNotNull(serializedString);
//        JSONObject jsonObject = new JSONObject(serializedString);
//
//        assertEquals(serializedString, "{\"light\":[{\"lux\":60.01}],\"location\":[{\"latitude\":52.205,\"longitude\":0.119}]}");
//        assertEquals(2, jsonObject.length()); // Check that there are two keys
//        assertEquals(locationArray.toString(), jsonObject.getJSONArray("location").toString());
//        assertEquals(lightArray.toString(), jsonObject.getJSONArray("light").toString());
//    }
//
//}
