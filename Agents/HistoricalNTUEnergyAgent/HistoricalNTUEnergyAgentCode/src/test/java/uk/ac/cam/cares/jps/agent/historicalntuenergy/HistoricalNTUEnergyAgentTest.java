package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.junit.Test;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.IOException;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import static org.junit.Assert.*;

public class HistoricalNTUEnergyAgentTest {

    // Use a temporary folder for testing
    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Before
    public void setUp() throws Exception {
        HistoricalNTUEnergyAgent agent = new HistoricalNTUEnergyAgent();
    }

    @Test
    public void testReadMappingsWithValidFolderAndOneFile() throws IOException {
        File mappingFile = tempFolder.newFile("test.json");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write("{\"key1\":\"http://example.com/iri1\",\"key2\":\"http://example.com/iri2\"}");
        writer.close();
        ArrayList<JSONKeyToIRIMapper> mappings = agent.readMappings(tempFolder.getRoot().getAbsolutePath());
        assertEquals(1, mappings.size());
        JSONKeyToIRIMapper mapper = mappings.get(0);
        assertNotNull(mapper);
        assertEquals("http://example.com/iri1", mapper.getIRI("key1"));
        assertEquals("http://example.com/iri2", mapper.getIRI("key2"));
    }

    @Test
    public void getNumberOfTimeSeries() {
    }

    @Test
    public void setTsClient() {
    }

    @Test
    public void initializeTimeSeriesIfNotExist() {
    }

    @Test
    public void updateData() {
    }
}