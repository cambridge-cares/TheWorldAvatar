package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.time.OffsetDateTime;

public class AQMeshInputAgentLauncher {

    // TODO: Proper logging for each phase of the main script
    // TODO: Use proper argument parsing
    /**
     * Main method that runs through all steps to update the data received from the AQMesh API.
     * defined in the provided properties file.
     * @param args The command line arguments. Three properties files should be passed here in order: 1) input agent
     *             2) time series client 3) API connector.
     */
    public static void main(String[] args) {

        // Ensure that there are three properties files
        if (args.length != 3) {
            throw new JPSRuntimeException("Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.");
        }

        // Create the agent
        AQMeshInputAgent agent;
        try {
            agent = new AQMeshInputAgent(args[0]);
        } catch (IOException e) {
            throw new JPSRuntimeException("The AQMesh input agent could not be constructed!", e);
        }

        // Create and set the time series client
        try {
            TimeSeriesClient<OffsetDateTime> tsClient = new TimeSeriesClient<>(OffsetDateTime.class, args[1]);
            agent.setTsClient(tsClient);
        } catch (IOException | JPSRuntimeException e) {
            throw new JPSRuntimeException("Could not construct the time series client needed by the input agent!", e);
        }

        // Initialize time series'
        agent.initializeTimeSeriesIfNotExist();

        // Create the connector to interact with the AQMesh API
        AQMeshAPIConnector connector;
        try {
            connector = new AQMeshAPIConnector(args[2]);
        } catch (IOException e) {
            throw new JPSRuntimeException("Could not construct the AQMesh API connector needed to interact with the API!", e);
        }
        connector.connect();

        // Retrieve readings
        JSONArray particleReadings;
        JSONArray gasReadings;
        try {
            particleReadings = connector.getParticleReadings();
            gasReadings = connector.getGasReadings();
        }
        catch (Exception e) {
            throw new JPSRuntimeException("One or both readings could not be retrieved, this might have created a mismatch" +
                    " in the pointers if one readings was successful and needs to be fixed!", e);
        }

        // If both readings are not empty there is new data
        if(!particleReadings.isEmpty() && !gasReadings.isEmpty()) {
            // Update the data
            agent.updateData(particleReadings, gasReadings);
        }
        // If both are empty no new readings are available
        else if(particleReadings.isEmpty() && gasReadings.isEmpty()) {
            // TODO: should be logged as info
            System.out.println("No new readings available.");
        }
        // One reading is empty and the other is not. This is likely due to asynchronous access to the readings, which
        // sets the pointers for each reading separately (should not happen when only using the agent unless there is an API error).
        // Data should be added to keep the database consistent. The pointer should be reset and
        // probably manual clean up in the database is required.
        else {
            throw new JPSRuntimeException("One of the readings (gas or particle) is empty, that means there is " +
                    "a mismatch in the pointer for each readings. This should be fixed (and might require a clean up of the database)!");
        }
    }

}
