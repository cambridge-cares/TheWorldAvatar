package uk.ac.cam.cares.jps.agent.configuration.test;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentProperty;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = gPROMSAgentProperty.class)
@TestPropertySource(locations = "classpath:gPROMSAgentPropertyTest.properties")
public class gPROMSAgentPropertyTest extends TestCase {

    @Autowired
    gPROMSAgentProperty property;

    @Test
    public void testGetProperties() {
        assertEquals("test_user", property.getHpcServerLoginUserName());
        assertEquals("password123", property.getHpcServerLoginUserPassword());
        assertEquals("gPROMSAgent", property.getAgentClass());
        assertEquals("gPROMSAgent", property.getAgentWorkspacePrefix());
        assertEquals("CompletedJobs", property.getAgentCompletedJobsSpacePrefix());
        assertEquals("FailedJobs", property.getAgentFailedJobsSpacePrefix());
        assertEquals("127.0.0.1", property.getHpcAddress());
        assertEquals("input", property.getInputFileName());
        assertEquals(".zip", property.getInputFileExtension());
        assertEquals("input_json", property.getJsonInputFileName());
        assertEquals(".json", property.getJsonFileExtension());
        assertEquals("Slurm.sh", property.getSlurmScriptFileName());
        assertEquals("fornow", property.getOutputFileName());
        assertEquals(".gPLOT", property.getOutputFileExtension());
        assertEquals(10, property.getMaxNumberOfHPCJobs());
        assertEquals(10, property.getAgentInitialDelayToStartJobMonitoring());
        assertEquals(60, property.getAgentPeriodicActionInterval());
        assertEquals("output.json", property.getReferenceOutputJsonFile());
    }

    @Test
    public void testGetAndSetExecutableFile() {
        assertEquals("executable.sh", property.getExecutableFile());
        property.setExecutableFile("new_executable.sh");
        assertEquals("new_executable.sh", property.getExecutableFile());
    }

    @Test
    public void testGetAgentScriptsLocation() {
        String agentScriptLocation = property.getAgentScriptsLocation();
        assertFalse(agentScriptLocation.contains("\\"));
        assertFalse(agentScriptLocation.contains("//"));
        String[] pathComponents = agentScriptLocation.split("/");
        int numberOfFolders = pathComponents.length;
        assertEquals("gPROMS-agent", pathComponents[numberOfFolders-2]);
        assertEquals("agent-scripts", pathComponents[numberOfFolders-1]);
    }
}
