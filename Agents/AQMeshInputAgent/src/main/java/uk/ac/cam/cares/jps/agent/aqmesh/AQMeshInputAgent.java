package uk.ac.cam.cares.jps.agent.aqmesh;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.time.Instant;

public class AQMeshInputAgent {

    private static final String propertiesFile = "aqmesh_inputAgent.properties";
    private static TimeSeriesClient<Instant> tsClient;

    public static void main(String[] args) throws IOException {
        // Initialize time series client
        tsClient = new TimeSeriesClient<Instant>(Instant.class, propertiesFile);

        // Check whether time series already exists
        if (!timeSeriesExists()) {
            initializeTimeSeries();
        }

        String token = getAccessToken();
        updateTimeSeries(token);
        
    }

    private static void updateTimeSeries(String token) {
        try {
            updateParticleReadings(token);
            updateGasReadings(token);
        }
        catch (Exception e) {
        }
    }

    private static void updateGasReadings(String token) {
    }

    private static void updateParticleReadings(String token) {
    }

    private static String getAccessToken() {
        return "token";
    }

    private static boolean timeSeriesExists() {
        return false;
    }

    private static void initializeTimeSeries() {
    }

}
