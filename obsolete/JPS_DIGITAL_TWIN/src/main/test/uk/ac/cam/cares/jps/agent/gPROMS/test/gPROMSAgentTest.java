package uk.ac.cam.cares.jps.agent.gPROMS.test;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.FileTime;

public class gPROMSAgentTest {

    // Folder for temporary files required by different tests that gets deleted after the tests
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testNewMatlabAgent() {
        gPROMSAgent agent = null;
        try {
            agent = new  gPROMSAgent();
        } finally {
            Assert.assertNotNull(agent);
        }
    }

    @Test
    public void testGettingFilecreationtime() throws IOException {
        // Create a test file
        File file = folder.newFile("test.txt");
        FileTime creationTime = (FileTime) Files.getAttribute(file.toPath(), "creationTime");
        Assert.assertEquals(creationTime.toString(), JPSMatlabAgent.gettingFilecreationtime(file));
    }

    @Test
    public void testGetNewJobFolderName() {
        String folderName = "test_1234567";
        gPROMSAgent agent = new gPROMSAgent();
        Assert.assertEquals(folderName, agent.getNewJobFolderName("test", 1234567));
    }

    @Test
    public void testGetJobId() {
        gPROMSAgent agent = new gPROMSAgent();
        Assert.assertNull(agent.getJobId(new JSONObject()));
        JSONObject json = new JSONObject("{'jobId': 'test'}");
        Assert.assertEquals("test", agent.getJobId(json));
    }

}
