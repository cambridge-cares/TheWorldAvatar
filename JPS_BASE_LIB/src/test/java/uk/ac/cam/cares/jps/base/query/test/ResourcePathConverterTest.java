package uk.ac.cam.cares.jps.base.query.test;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

import java.io.IOException;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.anyString;

public class ResourcePathConverterTest {

    @Rule
    public TemporaryFolder folder= new TemporaryFolder();

    @Test
    public void testGetInstance() {
        Assert.assertNotNull(ResourcePathConverter.getInstance());
    }

    @Test
    public void testConvert() {
        String testPath1 = "http://www.test.com/test";
        String testPath2 = "http://www.theworldavatar.com/scenariourl";
        String testPath3 = "http://www.jparksimulator.com/test";
        String address = KeyValueManager.getServerAddress();
        MockedStatic<AgentLocator> mockA = Mockito.mockStatic(AgentLocator.class);

        mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(false);
        Assert.assertEquals("http://www.test.com/test", ResourcePathConverter.convert(testPath1));

        mockA.when(AgentLocator::isJPSRunningForTest).thenReturn(true);
        Assert.assertEquals("http://www.test.com/test", ResourcePathConverter.convert(testPath1));
        Assert.assertEquals(address+"/scenariourl", ResourcePathConverter.convert(testPath2));
        Assert.assertEquals(address+"/test", ResourcePathConverter.convert(testPath3));

        MockedStatic<JPSContext> mockJPS = Mockito.mockStatic(JPSContext.class);
        mockJPS.when(() -> JPSContext.getScenarioUrl()).thenReturn("http://localhost:8080/scenariourl");
        Assert.assertEquals("http://www.theworldavatar.com/scenariourl", ResourcePathConverter.convert(testPath2));

        mockA.close();
    }

    @Test
    public void testConvertToLocalPath() {
        String testPath1 = "http://www.theworldavatar.com/Ontology";

        MockedStatic<KeyValueManager> mockKey = Mockito.mockStatic(KeyValueManager.class);
        mockKey.when(() -> KeyValueManager.get(anyString())).thenReturn("http://localhost:8080/test");

        Assert.assertEquals("http://localhost:8080/test/Ontology", ResourcePathConverter.convertToLocalPath(testPath1));
        mockKey.close();
    }
}