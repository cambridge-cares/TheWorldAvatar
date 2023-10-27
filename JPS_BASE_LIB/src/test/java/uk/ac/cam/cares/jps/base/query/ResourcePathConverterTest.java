package uk.ac.cam.cares.jps.base.query;

import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

import static org.mockito.ArgumentMatchers.anyString;

public class ResourcePathConverterTest {

    @Test
    public void testGetInstance() {
        Assert.assertNotNull(ResourcePathConverter.getInstance());
    }

    @Test
    public void testConvert() {
        String testPath1 = "http://www.theworldavatar.com/ontology/test";
        String testPath2 = "http://www.theworldavatar.com/scenariourl";
        String testPath3 = "http://www.jparksimulator.com/test";
        String address = KeyValueManager.getServerAddress();

        try (MockedStatic<AgentLocator> mockA = Mockito.mockStatic(AgentLocator.class)) {
            mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(false);
            Assert.assertEquals(testPath1, ResourcePathConverter.convert(testPath1));
        }

        try (MockedStatic<AgentLocator> mockA = Mockito.mockStatic(AgentLocator.class);
             MockedStatic<JPSContext> mockJPS = Mockito.mockStatic(JPSContext.class)) {
            mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(true);
            Assert.assertEquals(testPath1, ResourcePathConverter.convert(testPath1));
            Assert.assertEquals(address+"/scenariourl", ResourcePathConverter.convert(testPath2));
            Assert.assertEquals(address+"/test", ResourcePathConverter.convert(testPath3));

            mockJPS.when(JPSContext::getScenarioUrl).thenReturn("http://localhost:8080/scenariourl");
            Assert.assertEquals(testPath2, ResourcePathConverter.convert(testPath2));
        }
    }

    @Test
    public void testConvertToLocalPath() {
        String testPath1 = "http://www.theworldavatar.com/Ontology";

        try (MockedStatic<KeyValueManager> mockKey = Mockito.mockStatic(KeyValueManager.class)) {
            mockKey.when(() -> KeyValueManager.get(anyString())).thenReturn("http://localhost:8080/test");
            Assert.assertEquals("http://localhost:8080/test/Ontology", ResourcePathConverter.convertToLocalPath(testPath1));
        }
    }
}