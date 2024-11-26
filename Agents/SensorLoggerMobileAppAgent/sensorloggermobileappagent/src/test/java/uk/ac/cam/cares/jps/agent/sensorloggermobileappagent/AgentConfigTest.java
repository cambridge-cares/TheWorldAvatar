package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.junit.Before;
import org.junit.Test;
import uk.ac.cam.cares.downsampling.Downsampling;

import static org.junit.Assert.assertEquals;

public class AgentConfigTest {
    AgentConfig config;

    @Before
    public void setup() {
        config = new AgentConfig();
    }

    @Test public void agentConfigTest() {
        assertEquals(Downsampling.Type.INSTANTANEOUS, config.getAccelDSType());
        assertEquals(Long.valueOf(5), config.getAccelDSResolution());

        assertEquals(Downsampling.Type.INSTANTANEOUS, config.getGravityDSType());
        assertEquals(Long.valueOf(5), config.getAccelDSResolution());

        assertEquals(Downsampling.Type.AVERAGE, config.getRbDSType());
        assertEquals(Long.valueOf(5), config.getAccelDSResolution());

        assertEquals(Downsampling.Type.AVERAGE, config.getLightValueDSType());
        assertEquals(Long.valueOf(5), config.getAccelDSResolution());

        assertEquals(5, config.getTimerDelay());
        assertEquals(5, config.getTimerFrequency());
        assertEquals(600, config.getTaskInactiveTime());
    }
}
