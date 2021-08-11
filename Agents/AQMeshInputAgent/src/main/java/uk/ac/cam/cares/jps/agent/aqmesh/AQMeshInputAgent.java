package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.utils.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Class to retrieve data from the AQMesh API and storing it with connection to The World Avatar (Knowledge Base).
 * @author Niklas Kasenburg
 */
public class AQMeshInputAgent {

    private TimeSeriesClient<LocalDateTime> tsClient;
    private AQMeshAPIConnector connector;
    private List<JSONKeyToIRIMapper> mappings;
    private static final String generatedIRIPrefix = TimeSeriesSparql.ns_kb + "aqmesh";

    /**
     * Standard constructor which reads in JSON key to IRI mappings from the config folder
     * defined in the provided properties file.
     * @param propertiesFile The properties file from which to read the path of the mapping folder.
     */
    public AQMeshInputAgent(String propertiesFile) throws IOException {
        // Set the mapping between JSON keys and IRIs
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            // Read the mappings folder from the properties file
            String mappingFolder = prop.getProperty("aqmesh.mappingfolder");
            // Read the JSON key to IRI mappings from
            readMappings(mappingFolder);
        }
    }

    public static void main(String[] args) throws IOException {

        if (args.length == 0) {
            throw new JPSRuntimeException("No properties file provided as command line argument.");
        }
        String propertiesFile = args[0];

        AQMeshInputAgent agent = new AQMeshInputAgent(propertiesFile);
        // Check whether time series already exists
        if (!agent.timeSeriesExists()) {
            agent.initializeTimeSeries();
        }

        AQMeshAPIConnector connector = new AQMeshAPIConnector(propertiesFile);
        connector.connect();
        agent.setAPIConnector(connector);
    }


    public void setTsClient(TimeSeriesClient tsClient) {
        this.tsClient = tsClient;
    }

    public void setAPIConnector(AQMeshAPIConnector connector) {
        this.connector = connector;
    }

    private void updateTimeSeries() {
        try {
            updateParticleReadings();
            updateGasReadings();
        }
        catch (Exception e) {
        }
    }

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     * @param mappingFolder The path to the folder in which the mapping files are located.
     */
    private void readMappings(String mappingFolder) throws IOException {
        mappings = new ArrayList<>();
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        // Make sure the folder exists and contains files
        if (mappingFiles == null) {
            throw new IOException("Folder does not exist: " + mappingFolder);
        }
        if (mappingFiles.length == 0) {
            throw new IOException("No files in the folder: " + mappingFolder);
        }
        // Create a mapper for each file
        else {
            for (File mappingFile: mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(AQMeshInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
            }
        }
    }


    private void updateGasReadings() {
        JSONArray gasReadings = connector.getGasReadings();
    }

    private void updateParticleReadings() {
        JSONArray particleReadings = connector.getParticleReadings();
    }

    private boolean timeSeriesExists() {
        return true;
    }

    private void initializeTimeSeries() {
    }

}
