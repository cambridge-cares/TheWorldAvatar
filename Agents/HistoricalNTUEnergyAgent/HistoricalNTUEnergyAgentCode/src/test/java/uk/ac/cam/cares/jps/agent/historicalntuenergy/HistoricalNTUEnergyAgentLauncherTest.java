package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;

public class HistoricalNTUEnergyAgentLauncherTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    // Name of the properties files
    private final String agentPropertiesFilename = "agent.properties";
    private final String connectorPropertiesFilename = "xlsxconnector.properties";
    // Argument array used with the main function containing all the paths to the property files as string
    private String[] args;

    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    @Before
    public void initializePropertyFile() throws IOException {
        File agentPropertyFile = folder.newFile(agentPropertiesFilename);
        File connectorPropertiesFile = folder.newFile(connectorPropertiesFilename);
        // Paths to the two different property files
        String agentPropertiesFile = agentPropertyFile.getCanonicalPath();
        String apiPropertiesFile = connectorPropertiesFile.getCanonicalPath();

        args = new String[]{agentPropertiesFile, apiPropertiesFile};
    }

    @Test
    public void testMainNoArgs() {
        String[] args = {};
        try {
            HistoricalNTUEnergyAgentLauncher.initializeAgent(args);
            Assert.fail();
        } catch (JPSRuntimeException e) {
            Assert.assertEquals("Need two properties files in the following order: 1) input agent 2) xlsx connector.",
                    e.getMessage());
        }
    }

    @Test
    public void testMainInvalidAgentPropertyFile() {
        // Empty agent properties file should result in an error
        try {
            HistoricalNTUEnergyAgentLauncher.initializeAgent(args);
            Assert.fail();
        } catch (JPSRuntimeException e) {
            Assert.assertEquals("The Historical NTUEnergy agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingTSClient() throws IOException {
        createProperAgentPropertiesFile();
        // Empty properties file for time series client should result in exception
        try {
            HistoricalNTUEnergyAgentLauncher.initializeAgent(args);
            Assert.fail();
        } catch (JPSRuntimeException e) {
            Assert.assertEquals("The Historical NTUEnergy agent could not be constructed!", e.getMessage());
        }
    }

    private void createProperAgentPropertiesFile() throws IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        // Create mappings folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create empty file in mappings folder
        File mappingFile = new File(Paths.get(mappingFolder.getCanonicalPath(), "energy.properties").toString());
        Assert.assertTrue(mappingFile.createNewFile());
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), agentPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("ntuenergy.mappingfolder=" + mappingFolder.getCanonicalPath().
                    replace("\\", "/") + "\n");
        }
    }

}
