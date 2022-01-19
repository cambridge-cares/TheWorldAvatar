package uk.ac.cam.cares.jps.agent.matlab.test;

import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.*;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

public class JPSMatlabAgentTest {

    // Folder for temporary files required by different tests that gets deleted after the tests
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testNewMatlabAgent() {
        JPSMatlabAgent agent = null;
        try {
            agent = new JPSMatlabAgent();
        } finally {
            Assert.assertNotNull(agent);
        }
    }

    @Test
    public void testValidateInput() {
        JPSMatlabAgent agent = new JPSMatlabAgent();
        Assert.assertFalse(agent.validateInput("test"));
        gPROMSAgent.TEMP_DIRECTORY = "test";
        Assert.assertTrue(agent.validateInput("test"));
    }

    @Test
    public void testQueryRDF4J() throws IOException {
        // Create a test file which path is returned by the mockup query result
        File file = folder.newFile("test.txt");
        // Absolute path to the file which is JSON safe
        String filePath = file.getAbsolutePath().replace(System.getProperty("file.separator"), "/");
        JPSMatlabAgent agent = new JPSMatlabAgent();
        String agentIRI = "http://test.com/agent";
        List<String> lst = new ArrayList<>();
        // Mock query result returned instead of actually querying for meta data (needs to be in SPARQL JSON return format)
        JSONObject mockedResult = new JSONObject();
        mockedResult.put("head", new JSONObject("{'vars': [ 'file']}"));
        mockedResult.put("results", new JSONObject("{'bindings': [ {'file': {'type': 'literal', 'value': '"+filePath+"'}} ]}"));
        try (MockedStatic<MetaDataQuery> theMock = Mockito.mockStatic(MetaDataQuery.class)) {
            theMock.when(() -> MetaDataQuery.queryResources(null, null, null, agentIRI, null, null, null, lst))
                    .thenReturn(mockedResult.toString());
            Assert.assertEquals(filePath, agent.queryRDF4J(agentIRI, lst));
        }
    }

    @Test
    public void testGettingFilecreationtime() throws IOException {
        // Create a test file
        File file = folder.newFile("test.txt");
        FileTime creationTime = (FileTime) Files.getAttribute(file.toPath(), "creationTime");
        Assert.assertEquals(creationTime.toString(), JPSMatlabAgent.gettingFilecreationtime(file));
    }

}
