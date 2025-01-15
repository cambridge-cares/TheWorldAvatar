package uk.ac.cam.cares.jps.sensor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import android.content.Context;

import androidx.room.Room;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.ext.junit.runners.AndroidJUnit4;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.TestDatabase;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GravityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GyroDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LightDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.MagnetFieldStrengthDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.PressureDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.RelativeHumidityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.SoundLevelDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Gravity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.GyroData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LightData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.MagnetFieldStrength;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Pressure;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SoundLevel;


@RunWith(AndroidJUnit4.class)
public class LocalSourceTest {

    private TestDatabase db;
    private LocationDao mockLocationDao;
    private AccelerationDao mockAccelerationDao;
    private GravityDao mockGravityDao;
    private GyroDao mockGyroDao;
    private LightDao mockLightDao;
    private MagnetFieldStrengthDao mockMagnetFieldStrengthDao;
    private PressureDao mockPressureDao;
    private SoundLevelDao mockSoundLevelDao;
    private RelativeHumidityDao mockHumidityDao;

    private SensorLocalSource sensorLocalSource;


    @Before
    public void createDb() throws JSONException {
        Context context = ApplicationProvider.getApplicationContext();
        db = Room.inMemoryDatabaseBuilder(context, TestDatabase.class).build();

        // Initialize the DAOs from the TestDatabase
        mockLocationDao = db.locationDao();
        mockAccelerationDao = db.accelerationDao();
        mockGravityDao = db.gravityDao();
        mockGyroDao = db.gyroDao();
        mockLightDao = db.lightDao();
        mockMagnetFieldStrengthDao = db.magnetFieldStrengthDao();
        mockPressureDao = db.pressureDao();
        mockSoundLevelDao = db.soundLevelDao();
        mockHumidityDao = db.relativeHumidityDao();

        // Initialize SensorLocalSource using the TestDatabase
        sensorLocalSource = new SensorLocalSource(context) {
            {
                locationDao = mockLocationDao;
                accelerationDao = mockAccelerationDao;
                gravityDao = mockGravityDao;
                gyroDao = mockGyroDao;
                lightDao = mockLightDao;
                magnetFieldStrengthDao = mockMagnetFieldStrengthDao;
                pressureDao = mockPressureDao;
                soundLevelDao = mockSoundLevelDao;
                relativeHumidityDao = mockHumidityDao;
            }
        };

    }

    @After
    public void closeDb() throws IOException {
        if (db != null) {
            db.close();
        }
    }


    @Test
    public void testWriteLightAndLocData() throws Exception {
        // create the JSONArray and add the JSONObject to it
        JSONArray mockLocationData = new JSONArray();
        JSONObject mockLocationObject = new JSONObject();
        mockLocationObject.put("time", 2392392839L);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("latitude", 52.205);
        valuesObject.put("longitude", 0.119);
        valuesObject.put("altitude", 30.0);
        valuesObject.put("speed", 10.5f);
        valuesObject.put("bearing", 250.0f);
        valuesObject.put("horizontalAccuracy", 5.0f);
        valuesObject.put("bearingAccuracy", 1.0f);
        valuesObject.put("speedAccuracy", 0.5f);
        valuesObject.put("verticalAccuracy", 3.0f);
        mockLocationObject.put("values", valuesObject);

        mockLocationData.put(mockLocationObject);
        JSONArray mockLightData = new JSONArray();
        JSONObject mockLightObject = new JSONObject();
        JSONObject valuesObject2 = new JSONObject();
        mockLightObject.put("time", 2392398839L);
        valuesObject2.put("lux", 60.01);
        mockLightObject.put("values", valuesObject2);
        mockLightData.put(mockLightObject);

        Map<String, JSONArray> allSensorData = new HashMap<>();
        allSensorData.put("location", mockLocationData);
        allSensorData.put("light", mockLightData);

        sensorLocalSource.writeToDatabase(allSensorData);

        LocationData[] locationDataArray = mockLocationDao.getAll();

        assertEquals(1, locationDataArray.length);

        LightData[] lightDataArray = mockLightDao.getAll();


        assertNotNull(lightDataArray);
        assertEquals(1, lightDataArray.length);
    }

    @Test
    public void testWriteToDatabase() throws Exception {

        // create the JSONArray and add the JSONObject to it
        JSONArray mockLocationData = new JSONArray();
        JSONObject mockLocationObject = new JSONObject();
        mockLocationObject.put("time", 2392392839L);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("latitude", 52.205);
        valuesObject.put("longitude", 0.119);
        valuesObject.put("altitude", 30.0);
        valuesObject.put("speed", 10.5f);
        valuesObject.put("bearing", 250.0f);
        valuesObject.put("horizontalAccuracy", 5.0f);
        valuesObject.put("bearingAccuracy", 1.0f);
        valuesObject.put("speedAccuracy", 0.5f);
        valuesObject.put("verticalAccuracy", 3.0f);
        mockLocationObject.put("values", valuesObject);
        mockLocationData.put(mockLocationObject);

        Map<String, JSONArray> allSensorData = new HashMap<>();


        // add the JSONArray to the allSensorData map
        allSensorData.put("location", mockLocationData);

        // call the method to test
        sensorLocalSource.writeToDatabase(allSensorData);

        LocationData[] locationDataArray = mockLocationDao.getAll();

        assertEquals(1, locationDataArray.length);
        assertEquals(2392392839L, locationDataArray[0].time);
        assertEquals(52.2048, locationDataArray[0].latitude, 0.003);
        assertEquals(0.1218, locationDataArray[0].longitude, 0.003);
}

    @Test
    public void testWriteToDatabaseAndVerifyLightData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockLightData = new JSONArray();
        JSONObject mockLightObject = new JSONObject();
        JSONObject valuesObject = new JSONObject();
        mockLightObject.put("time", 2392392839L);
        valuesObject.put("lux", 60.01);
        mockLightObject.put("values", valuesObject);
        mockLightData.put(mockLightObject);
        allSensorData.put("light", mockLightData);


        sensorLocalSource.writeToDatabase(allSensorData);

        LightData[] lightDataArray = mockLightDao.getAll();


        assertNotNull(lightDataArray);
        assertEquals(1, lightDataArray.length);
        assertEquals(2392392839L, lightDataArray[0].time);
        assertEquals(60.01, lightDataArray[0].lux, 0.003);
    }

    @Test
    public void testWriteToDatabaseAndVerifyPressureData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockPresData = new JSONArray();
        JSONObject mockPresObject = new JSONObject();
        JSONObject valuesObject = new JSONObject();
        mockPresObject.put("time", 2392392839L);
        valuesObject.put("pressure", 60.01);
        mockPresObject.put("values", valuesObject);
        mockPresData.put(mockPresObject);
        allSensorData.put("pressure", mockPresData);


        sensorLocalSource.writeToDatabase(allSensorData);

        Pressure[] presDataArray = mockPressureDao.getAll();


        assertNotNull(presDataArray);
        assertEquals(1, presDataArray.length);
        assertEquals(2392392839L, presDataArray[0].time);
        assertEquals(60.01, presDataArray[0].pressure, 0.003);
    }

    @Test
    public void testWriteToDatabaseAndVerifyHumidityData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockHumidityData = new JSONArray();
        JSONObject mockHumidityObject = new JSONObject();
        JSONObject valuesObject = new JSONObject();
        mockHumidityObject.put("time", 2392392839L);
        valuesObject.put("humidity", 60.01);
        mockHumidityObject.put("values", valuesObject);
        mockHumidityData.put(mockHumidityObject);
        allSensorData.put("humidity", mockHumidityData);


        sensorLocalSource.writeToDatabase(allSensorData);

        RelativeHumidity[] humidityDataArray = mockHumidityDao.getAll();


        assertNotNull(humidityDataArray);
        System.out.println(humidityDataArray);
        assertEquals(1, humidityDataArray.length);
        assertEquals(2392392839L, humidityDataArray[0].time);
        assertEquals(60.01, humidityDataArray[0].humidity, 0.003);
    }

    @Test
    public void testWriteToDatabaseAndVerifySoundData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockSoundData = new JSONArray();
        JSONObject mockSoundObject = new JSONObject();
        JSONObject valuesObject = new JSONObject();
        mockSoundObject.put("time", 2392392839L);
        valuesObject.put("dBFS", 60.01);
        mockSoundObject.put("values", valuesObject);
        mockSoundData.put(mockSoundObject);
        allSensorData.put("sound", mockSoundData);


        sensorLocalSource.writeToDatabase(allSensorData);

        SoundLevel[] soundDataArray = mockSoundLevelDao.getAll();


        assertNotNull(soundDataArray);
        assertEquals(1, soundDataArray.length);
        assertEquals(2392392839L, soundDataArray[0].time);
        assertEquals(60.01, soundDataArray[0].dBFS, 0.003);
    }


    @Test
    public void testWriteToDatabaseAndVerifyMagnetData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockMagnetData = new JSONArray();
        JSONObject mockMagnetObject = new JSONObject();
        mockMagnetObject.put("time", 2392392839L);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("x", 78.01);
        valuesObject.put("y", 44.40);
        valuesObject.put("z", 222.0);
        mockMagnetObject.put("values", valuesObject);
        mockMagnetData.put(mockMagnetObject);
        allSensorData.put("magnet", mockMagnetData);


        sensorLocalSource.writeToDatabase(allSensorData);

        MagnetFieldStrength[] magnetDataArray = mockMagnetFieldStrengthDao.getAll();


        assertNotNull(magnetDataArray);
        assertEquals(1, magnetDataArray.length);
        assertEquals(2392392839L, magnetDataArray[0].time);
        assertEquals(78.01, magnetDataArray[0].x, 0.001);
        assertEquals(44.40, magnetDataArray[0].y, 0.001);
        assertEquals(222.0, magnetDataArray[0].z, 0.001);
    }

    @Test
    public void testWriteToDatabaseAndVerifyGyroData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockGyroData = new JSONArray();
        JSONObject mockGyroObject = new JSONObject();
        JSONObject valuesObject = new JSONObject();
        mockGyroObject.put("time", 2392392839L);
        valuesObject.put("x", 78.01);
        valuesObject.put("y", 44.40);
        valuesObject.put("z", 222.0);
        mockGyroObject.put("values", valuesObject);
        mockGyroData.put(mockGyroObject);
        allSensorData.put("gyro", mockGyroData);


        sensorLocalSource.writeToDatabase(allSensorData);

        GyroData[] gyroDataArray = mockGyroDao.getAll();


        assertNotNull(gyroDataArray);
        assertEquals(1, gyroDataArray.length);
        assertEquals(2392392839L, gyroDataArray[0].time);
        assertEquals(78.01, gyroDataArray[0].x, 0.001);
        assertEquals(44.40, gyroDataArray[0].y, 0.001);
        assertEquals(222.0, gyroDataArray[0].z, 0.001);
    }

    @Test
    public void testWriteToDatabaseAndVerifyGravityData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockGravityData = new JSONArray();
        JSONObject mockGravityObject = new JSONObject();
        mockGravityObject.put("time", 2392392839L);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("x", 78.01);
        valuesObject.put("y", 44.40);
        valuesObject.put("z", 222.0);
        mockGravityObject.put("values", valuesObject);
        mockGravityData.put(mockGravityObject);
        allSensorData.put("gravity", mockGravityData);


        sensorLocalSource.writeToDatabase(allSensorData);

        Gravity[] gravityDataArray = mockGravityDao.getAll();


        assertNotNull(gravityDataArray);
        assertEquals(1, gravityDataArray.length);
        assertEquals(2392392839L, gravityDataArray[0].time);
        assertEquals(78.01, gravityDataArray[0].x, 0.001);
        assertEquals(44.40, gravityDataArray[0].y, 0.001);
        assertEquals(222.0, gravityDataArray[0].z, 0.001);
    }

    @Test
    public void testWriteToDatabaseAndVerifyAccData() throws Exception {

        Map<String, JSONArray> allSensorData = new HashMap<>();
        JSONArray mockAccData = new JSONArray();
        JSONObject mockAccObject = new JSONObject();
        mockAccObject.put("time", 2392392839L);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("x", 78.01);
        valuesObject.put("y", 44.40);
        valuesObject.put("z", 222.0);
        mockAccObject.put("values", valuesObject);
        mockAccData.put(mockAccObject);
        allSensorData.put("acceleration", mockAccData);


        sensorLocalSource.writeToDatabase(allSensorData);

        Acceleration[] accelerationDataArray = mockAccelerationDao.getAll();


        assertNotNull(accelerationDataArray);
        //assertEquals(1, accelerationDataArray.length);
        assertEquals(2392392839L, accelerationDataArray[0].time);
        assertEquals(78.01, accelerationDataArray[0].x, 0.001);
        assertEquals(44.40, accelerationDataArray[0].y, 0.001);
        assertEquals(222.0, accelerationDataArray[0].z, 0.001);
    }

    @Test
    public void testDeleteHistoricalData() throws JSONException {
        // Insert test data
        long currentTime = System.currentTimeMillis();
        long oldTime = currentTime - 40L * 24 * 60 * 60 * 1000; // 40 days old
        long newTime = currentTime - 10L * 24 * 60 * 60 * 1000; // 10 days old

        Map<String, JSONArray> allSensorData = new HashMap<>();
        // create the JSONArray and add the JSONObject to it
        JSONArray mockLocationData = new JSONArray();
        JSONObject mockLocationObject = new JSONObject();
        mockLocationObject.put("time", oldTime);
        JSONObject valuesObject = new JSONObject();
        valuesObject.put("latitude", 52.205);
        valuesObject.put("longitude", 0.119);
        valuesObject.put("altitude", 30.0);
        valuesObject.put("speed", 10.5f);
        valuesObject.put("bearing", 250.0f);
        valuesObject.put("horizontalAccuracy", 5.0f);
        valuesObject.put("bearingAccuracy", 1.0f);
        valuesObject.put("speedAccuracy", 0.5f);
        valuesObject.put("verticalAccuracy", 3.0f);
        mockLocationObject.put("values", valuesObject);

        mockLocationData.put(mockLocationObject);


        JSONArray mockLocationData2 = new JSONArray();
        JSONObject mockLocationObject2 = new JSONObject();
        mockLocationObject.put("time", newTime);
        JSONObject valuesObject2 = new JSONObject();
        valuesObject2.put("latitude", 52.205);
        valuesObject2.put("longitude", 0.119);
        valuesObject2.put("altitude", 30.0);
        valuesObject2.put("speed", 10.5f);
        valuesObject2.put("bearing", 250.0f);
        valuesObject2.put("horizontalAccuracy", 5.0f);
        valuesObject2.put("bearingAccuracy", 1.0f);
        valuesObject2.put("speedAccuracy", 0.5f);
        valuesObject2.put("verticalAccuracy", 3.0f);
        mockLocationObject2.put("values", valuesObject);

        mockLocationData2.put(mockLocationObject2);
        // add the JSONArray to the allSensorData map
        allSensorData.put("location", mockLocationData);
        allSensorData.put("location2", mockLocationData2);


        // call the method to test
        sensorLocalSource.writeToDatabase(allSensorData);

        LocationData[] locationDataArray = mockLocationDao.getAll();
        // Perform deletion
        long cutoffTime = currentTime - 30L * 24 * 60 * 60 * 1000; // 30 days ago
        sensorLocalSource.deleteHistoricalData(cutoffTime);

        // Verify that only new data remains
        LocationData[] remainingData = mockLocationDao.getAll();
        assertEquals(1, remainingData.length);
        assertEquals(newTime, remainingData[0].time);
    }



}
