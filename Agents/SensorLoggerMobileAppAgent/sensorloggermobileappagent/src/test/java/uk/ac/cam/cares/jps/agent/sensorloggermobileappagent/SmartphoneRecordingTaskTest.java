package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.json.JSONArray;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class SmartphoneRecordingTaskTest {
    SmartphoneRecordingTask task;

    @Before
    public void setup() {
        RemoteStoreClient storeClientMock = mock(RemoteStoreClient.class);
        when(storeClientMock.executeQuery(ArgumentMatchers.anyString())).thenReturn(new JSONArray());

        RemoteRDBStoreClient rdbStoreClientMock = mock(RemoteRDBStoreClient.class);
        when(rdbStoreClientMock.getRdbURL()).thenReturn("rdb_url");
        when(rdbStoreClientMock.getUser()).thenReturn("rdb_user");
        when(rdbStoreClientMock.getPassword()).thenReturn("rdb_password");

        AgentConfig agentConfigMock = mock(AgentConfig.class);
        when(agentConfigMock.getTimerFrequency()).thenReturn(2);
        when(agentConfigMock.getTaskInactiveTime()).thenReturn(2);

        String deviceId = "123";
        task = new SmartphoneRecordingTask(storeClientMock, rdbStoreClientMock, agentConfigMock, deviceId,
                storeClientMock);
    }

    private HashMap<String, List<?>> getDataHashMap() {
        OffsetDateTime now = OffsetDateTime.now();
        OffsetDateTime fiveMinutesAgo = now.minusMinutes(5);
        OffsetDateTime tenMinutesAgo = now.minusMinutes(10);

        // Accelerometer list
        ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>(Arrays.asList(now, fiveMinutesAgo, tenMinutesAgo));
        List<Double> accelList_x = Arrays.asList(0.1, 0.2, 0.3);
        List<Double> accelList_y = Arrays.asList(0.4, 0.5, 0.6);
        List<Double> accelList_z = Arrays.asList(0.7, 0.8, 0.9);

        // Magnetometer list
        ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>(Arrays.asList(fiveMinutesAgo));
        List<Double> magnetometerList_x = Arrays.asList(30.1);
        List<Double> magnetometerList_y = Arrays.asList(30.4);
        List<Double> magnetometerList_z = Arrays.asList(30.7);

        // Gravity sensor list
        ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>(Arrays.asList(now, fiveMinutesAgo));
        List<Double> gravityList_x = Arrays.asList(9.8, 9.7);
        List<Double> gravityList_y = Arrays.asList(0.1, 0.0);
        List<Double> gravityList_z = Arrays.asList(0.0, 0.1);

        // Location list
        ArrayList<OffsetDateTime> location_tsList = new ArrayList<>(Arrays.asList(now, fiveMinutesAgo));
        List<Double> bearingList = Arrays.asList(45.0, 50.0);
        List<Double> speedList = Arrays.asList(10.0, 15.0);
        List<Double> altitudeList = Arrays.asList(100.0, 105.0);
        List<Point> geomLocationList = Arrays.asList(new Point(1, 2), new Point(3, 4));

        // Microphone lists
        ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>(Arrays.asList(now, fiveMinutesAgo, tenMinutesAgo));
        List<Double> dBFSList = Arrays.asList(-10.0, -20.0, -30.0);

        // Light value lists
        ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>(
                Arrays.asList(now, fiveMinutesAgo, tenMinutesAgo));
        List<Double> lightValueList = Arrays.asList(300.0, 500.0, 700.0);

        ArrayList<OffsetDateTime> brightness_tsList = new ArrayList<>(
                Arrays.asList(now, fiveMinutesAgo, tenMinutesAgo));
        List<Double> brightnessList = Arrays.asList(200.0, 250.0, 300.0);

        HashMap<String, List<?>> dataHashmap = new HashMap<>();
        dataHashmap.put("accel_tsList", accel_tsList);
        dataHashmap.put("accelList_x", accelList_x);
        dataHashmap.put("accelList_y", accelList_y);
        dataHashmap.put("accelList_z", accelList_z);
        dataHashmap.put("magnetometer_tsList", magnetometer_tsList);
        dataHashmap.put("magnetometerList_x", magnetometerList_x);
        dataHashmap.put("magnetometerList_y", magnetometerList_y);
        dataHashmap.put("magnetometerList_z", magnetometerList_z);
        dataHashmap.put("gravity_tsList", gravity_tsList);
        dataHashmap.put("gravityList_x", gravityList_x);
        dataHashmap.put("gravityList_y", gravityList_y);
        dataHashmap.put("gravityList_z", gravityList_z);
        dataHashmap.put("location_tsList", location_tsList);
        dataHashmap.put("bearingList", bearingList);
        dataHashmap.put("speedList", speedList);
        dataHashmap.put("altitudeList", altitudeList);
        dataHashmap.put("geomLocationList", geomLocationList);
        dataHashmap.put("dBFS_tsList", dBFS_tsList);
        dataHashmap.put("dBFSList", dBFSList);
        dataHashmap.put("lightValue_tsList", lightValue_tsList);
        dataHashmap.put("lightValueList", lightValueList);
        dataHashmap.put("brightness_tsList", brightness_tsList);
        dataHashmap.put("brightnessList", brightnessList);

        return dataHashmap;
    }

    @Test
    public void testAddData() {
        task.addData(getDataHashMap());

        assertEquals(3, task.accelerometerProcessor.getTimeSeriesLength());
        assertEquals(1, task.magnetometerDataProcessor.getTimeSeriesLength());
        assertEquals(2, task.gravityDataProcessor.getTimeSeriesLength());
        assertEquals(2, task.locationDataProcessor.getTimeSeriesLength());
        assertEquals(3, task.dbfsDataProcessor.getTimeSeriesLength());
        assertEquals(3, task.relativeBrightnessProcessor.getTimeSeriesLength());
        assertEquals(3, task.illuminationProcessor.getTimeSeriesLength());
    }

    @Test
    public void shouldProcessData() {
        try {
            Thread.sleep(3000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assertTrue(task.shouldProcessData());
    }

    @Test
    public void shouldNotProcessData() {
        assertFalse(task.shouldProcessData());
    }

    @Test
    public void shouldTerminateTask() {
        try {
            Thread.sleep(3000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assertTrue(task.shouldTerminateTask());
    }

    @Test
    public void shouldNotTerminateTask() {
        assertFalse(task.shouldTerminateTask());
    }
}
