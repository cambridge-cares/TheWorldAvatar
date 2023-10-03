package com.cmclinnovations.featureinfo.core.time;

import java.sql.Connection;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.config.StackInteractor;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This utility class handles the creation of sample time series data
 * to be used in manual testing. It should be run on it's own from VS Code
 * and is not considered an automated test.
 * 
 * Points to note:
 *  - It is expected that the FIA sample stack is running before this class
 *    is manually executed.
 *  - Time series data is added, but not cleaned up, by this class.
 *  - If the configuration of the sample stack is updated, this class will
 *    likely need updating to match.
 *  - As manually launched class, solely for testing in the sample stack,
 *    this class doesn't have as much error checking & logging. Be warned.
 */
public final class TimeSeriesCreator {
    
    /**
     * Should match number of castles created in sample data.
     */
    private static final int CASTLE_NUMBER = 116;

    /**
     * Discovered stack endpoints.
     */
    private final List<StackEndpoint> stackEndpoints = new ArrayList<>();

    /**
     * KG client.
     */
    private RemoteStoreClient kgClient;

    /**
     * TS client.
     */
    private TimeSeriesClient<Instant> tsClient;

    /**
     * RDB client.
     */
    private RemoteRDBStoreClient rdbClient;

    /**
     * 
     */
    private TimeSeriesCreator() {
        // No.
    }

    /**
     * Discover available stack endpoints.
     */
    private void discoverStackDetails() {
        StackInteractor interactor = new StackInteractor(this.stackEndpoints);
        interactor.discoverEndpoints();
    }

    /**
     * Create required client instances.
     */
    private void createClients() {
        // Find the "sample-data" namespace in Blazegraph
        StackEndpoint kgEndpoint = this.stackEndpoints
            .stream()
            .filter(endpoint -> endpoint.url().contains("sample-data"))
            .findFirst()
            .orElse(null);
        
        // Find the postgres endpoint
        StackEndpoint rdbEndpoint = this.stackEndpoints
            .stream()
            .filter(endpoint -> endpoint.type().equals(StackEndpointType.POSTGRES))
            .findFirst()
            .orElse(null);

        // KG client
        this.kgClient = new RemoteStoreClient(
            kgEndpoint.url(),
            kgEndpoint.url(),
            kgEndpoint.username(),
            kgEndpoint.password()
        );

        // TS client
        this.tsClient = new TimeSeriesClient<>(kgClient, Instant.class);

        // RDB client
        String postgresURL = StackInteractor.generatePostgresURL("castles");
        this.rdbClient = new RemoteRDBStoreClient(
            postgresURL,
            rdbEndpoint.username(),
            rdbEndpoint.password()
        );
    }

    /**
     * Generate some sample data.
     */
    private void addSampleData() throws Exception {
        String populationBase = "https://theworldavatar.io/sample-data/PopulationMeasurement/"; 
        String worthBase = "https://theworldavatar.io/sample-data/WorthMeasurement/"; 
        String lordBase = "https://theworldavatar.io/sample-data/FeudalLordMeasurement/"; 

        // Cache the RDB connection
        Connection connection = this.rdbClient.getConnection();

        for(int i = 1; i <= CASTLE_NUMBER; i++) {
            String populationIRI = populationBase + Integer.toString(i);
            String worthIRI = worthBase + Integer.toString(i);
            String lordIRI = lordBase + Integer.toString(i);

            // Initialise single times series
            this.tsClient.initTimeSeries(
                new ArrayList<String>(Arrays.asList(populationIRI, worthIRI, lordIRI)),
                new ArrayList<Class<?>>(Arrays.asList(Instant.class, Instant.class, Instant.class)),
                "TBD",
                connection
            );

            // Re-query for timeseries object because TimeSeriesClient is shit.
            //TimeSeries<Instant> timeseries = this.tsClient.getTimeSeries(null, connection)
        }
    }

    /**
     * Entry point.
     * 
     * @param args CLI args.
     */
    public static void main(String[] args) {
        TimeSeriesCreator creator = new TimeSeriesCreator();
        creator.discoverStackDetails();
        creator.createClients();
    }

}
// End of class.