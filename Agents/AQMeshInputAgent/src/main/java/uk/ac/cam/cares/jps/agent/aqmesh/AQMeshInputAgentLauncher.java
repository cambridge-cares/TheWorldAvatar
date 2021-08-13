package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.time.ZonedDateTime;

public class AQMeshInputAgentLauncher {

    // TODO: Proper logging for each phase of the main script
    public static void main(String[] args) {

        // Ensure that a properties file is provided
        if (args.length == 0) {
            throw new JPSRuntimeException("No properties file provided as command line argument.");
        }
        String propertiesFile = args[0];

        // Create the agent
        AQMeshInputAgent agent;
        try {
            agent = new AQMeshInputAgent(propertiesFile);
        } catch (IOException e) {
            throw new JPSRuntimeException("The AQMesh input agent could not be constructed!", e);
        }

        // Create and set the time series client
        try {
            TimeSeriesClient<ZonedDateTime> tsClient = new TimeSeriesClient<>(ZonedDateTime.class, propertiesFile);
            agent.setTsClient(tsClient);
        } catch (IOException e) {
            throw new JPSRuntimeException("Could not construct the time series client needed by the input agent!", e);
        }

        // Initialize time series'
        agent.initializeTimeSeriesIfNotExist();

        // Create the connector to interact with the AQMesh API
        AQMeshAPIConnector connector;
        try {
            connector = new AQMeshAPIConnector(propertiesFile);
        } catch (IOException e) {
            throw new JPSRuntimeException("Could not construct the AQMesh API connector needed to interact with the API!", e);
        }
        connector.connect();

        // Retrieve readings
        JSONArray particleReadings = connector.getParticleReadings();
        JSONArray gasReadings = connector.getGasReadings();

        // Update the data
        agent.updateDate(particleReadings, gasReadings);

    }

}
