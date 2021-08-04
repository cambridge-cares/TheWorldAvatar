package uk.ac.cam.cares.jps.agent.aqmesh;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.Properties;

public class AQMeshInputAgent {

    private TimeSeriesClient<LocalDateTime> tsClient;

    public AQMeshInputAgent(String propertiesFile) throws IOException {
        // Initialize time series client
        tsClient = new TimeSeriesClient<>(LocalDateTime.class, propertiesFile);
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
        agent.updateTimeSeries(connector);
    }

    private void updateTimeSeries(AQMeshAPIConnector connector) {
        try {
            updateParticleReadings(connector);
            updateGasReadings(connector);
        }
        catch (Exception e) {
        }
    }

    private void updateGasReadings(AQMeshAPIConnector connector) {
        TimeSeries<LocalDateTime> gasReadings = connector.getGasReadings();
    }

    private void updateParticleReadings(AQMeshAPIConnector connector) {
        TimeSeries<LocalDateTime> particleReadings = connector.getParticleReadings();
    }

    private boolean timeSeriesExists() {
        return true;
    }

    private void initializeTimeSeries() {
    }

}
