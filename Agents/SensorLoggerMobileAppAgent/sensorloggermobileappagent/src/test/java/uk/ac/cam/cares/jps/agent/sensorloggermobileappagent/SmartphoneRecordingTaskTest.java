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
