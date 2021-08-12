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
import java.util.regex.Pattern;

/**
 * Class to retrieve data from the AQMesh API and storing it with connection to The World Avatar (Knowledge Base).
 * @author Niklas Kasenburg
 */
public class AQMeshInputAgent {

    // The time series client to interact with the knowledge graph and data storage
    private TimeSeriesClient<LocalDateTime> tsClient;
    // The connector to interact with the AQMesh API
    private AQMeshAPIConnector connector;
    // A list of mappings between JSON keys and the corresponding IRI, contains one mapping per time series
    private List<JSONKeyToIRIMapper> mappings;
    // The prefix to use when no IRI exists for a JSON key originally
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

    /**
     * Retrieves the number of time series the input agent is handling.
     * @return  The number of time series maintained by the agent.
     */
    public int getNumberOfTimeSeries() {
        return mappings.size();
    }

    /**
     * Setter for the time series client.
     * @param tsClient The time series client to use.
     */
    public void setTsClient(TimeSeriesClient tsClient) {
        this.tsClient = tsClient;
    }

    /**
     * Setter for the AQMesh API connector.
     * @param connector The AQMesh API connector to use.
     */
    public void setAPIConnector(AQMeshAPIConnector connector) {
        this.connector = connector;
    }

    public void initializeTimeSeriesIfNotExist() {
        // Iterate through all mappings (each represents one time series)
        for (JSONKeyToIRIMapper mapping: mappings) {
            // The IRIs used by the current mapping
            List<String> iris = mapping.getAllIRIs();
            // Check whether IRIs have a time series linked and if not initialize the corresponding time series
            if(timeSeriesExist(iris)) {
                initializeTimeSeries(iris);
            }
        }
    }

    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for(String iri: iris) {
            if (!tsClient.checkDataHasTimeSeries(iri)) {
                return false;
            }
        }
        return true;
    }

    private void initializeTimeSeries(List<String> iris) {
        // TODO: How to get the datatype for each key/iri? Config file or parsing from JSON?
        //tsClient.initTimeSeries(iris);
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

    /**
     * Returns the class (datatype) corresponding to a JSON key. Note: rules for the mapping are hardcoded in the method.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String jsonKey) {
        // JSON keys for reading and sending intervals end in _p1, _p2 or _p3
        Pattern intervalPattern = Pattern.compile(".*_p[123]$");
        // Intervals are integers representing the seconds
        if (intervalPattern.matcher(jsonKey).matches()) {
            return Integer.class;
        }
        // Battery voltage is a floating point
        else if (jsonKey.contains("_voltage")) {
            return Double.class;
        }
        // Environment conditions are floating point measures
        else if (jsonKey.contains("temperature") | jsonKey.contains("pressure") | jsonKey.contains("humidity")) {
            return Double.class;
        }
        // Noise information for gas readings are floating point numbers
        else if (jsonKey.contains("noise")) {
            return Double.class;
        }
        // Sensor readings and corresponding offset and slope are floating point numbers
        else if (jsonKey.contains("prescale") | jsonKey.contains("slope") | jsonKey.contains("offset")) {
            return Double.class;
        }
        // Battery low warning and particle modem overlap are boolean
        else if (jsonKey.equals("battery_low") | jsonKey.equals("particle_modem_overlap")) {
            return Boolean.class;
        }
        // The default datatype is string
        else {
            return String.class;
        }
    }

    private void updateGasReadings() {
        JSONArray gasReadings = connector.getGasReadings();
    }

    private void updateParticleReadings() {
        JSONArray particleReadings = connector.getParticleReadings();
    }

}
