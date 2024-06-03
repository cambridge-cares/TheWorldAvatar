package com.cmclinnovations.featureinfo.utils;

import java.sql.Connection;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.config.StackInteractor;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This utility class handles the creation of sample time series data to be used in manual
 * testing. It is specific to the data format set out in the 'sample' directory of the
 * FeatureInfoAgent.
 * 
 * Ideally this should be a located within it's own Agent project, so that it can
 * easily be deployed and run within a TWA Stack instance. However, as time is short,
 * this class is currently deplyed with all instances of the FeatureInfoAgent and can
 * be triggered via an undocumented route. In future, it can be split out into a more
 * generic test-stack-populator type Agent.
 */
public final class TimeSeriesCreator {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeSeriesCreator.class);

    /**
     * Should match number of castles created in sample data.
     */
    private static final int CASTLE_NUMBER = 116;

    /**
     * Loaded configuration store.
     */
    private final ConfigStore configStore;
    
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
    public TimeSeriesCreator(ConfigStore configStore) {
        this.configStore = configStore;
    }

    /**
     * Create required client instances.
     * 
     * @param namespace KG namespace to add triples to.
     * @param database 
     */
    public void createClients(String namespace, String database) {
        // Find the "sample-data" namespace in Blazegraph
        StackEndpoint kgEnd = this.configStore.getStackEndpoints()
            .stream()
            .filter(endpoint -> endpoint.url().contains(namespace))
            .findFirst()
            .orElse(null);
        
        // Find the postgres endpoint
        StackEndpoint rdbEnd = this.configStore.getStackEndpoints()
            .stream()
            .filter(endpoint -> endpoint.type().equals(StackEndpointType.POSTGRES))
            .findFirst()
            .orElse(null);

        // KG client
        this.kgClient = new RemoteStoreClient(
            kgEnd.url(),
            kgEnd.url(),
            (kgEnd.username() == null || kgEnd.username().isEmpty()) ? null : kgEnd.username(),
            (kgEnd.password() == null || kgEnd.password().isEmpty()) ? null : kgEnd.password()
        );

        // TS client
        this.tsClient = new TimeSeriesClient<>(kgClient, Instant.class);

        // RDB client
        String postgresURL = StackInteractor.generatePostgresURL(database);
        this.rdbClient = new RemoteRDBStoreClient(
            postgresURL,
            (rdbEnd.username() == null || rdbEnd.username().isEmpty()) ? null : rdbEnd.username(),
            (rdbEnd.password() == null || rdbEnd.password().isEmpty()) ? null : rdbEnd.password()
        );
    }

    /**
     * Generate some sample data.
     */
    public void addSampleData() {
        String populationBase = "https://theworldavatar.io/sample-data/PopulationMeasurement/"; 
        String worthBase = "https://theworldavatar.io/sample-data/WorthMeasurement/"; 
        String lordBase = "https://theworldavatar.io/sample-data/FeudalLordMeasurement/"; 

        // Cache the RDB connection
        try (Connection connection = this.rdbClient.getConnection()) {

            for(int i = 1; i <= CASTLE_NUMBER; i++) {
                String populationIRI = populationBase + Integer.toString(i);
                String worthIRI = worthBase + Integer.toString(i);
                String lordIRI = lordBase + Integer.toString(i);

                // Initialise single times series
                this.tsClient.initTimeSeries(
                    new ArrayList<String>(Arrays.asList(populationIRI, worthIRI, lordIRI)),
                    new ArrayList<Class<?>>(Arrays.asList(Integer.class, Double.class, String.class)),
                    null,
                    connection
                );

                // Create timeseries object with times and values
                TimeSeries<Instant> timeseries = new TimeSeries<>(
                    generateTimes(),
                    new ArrayList<String>(Arrays.asList(populationIRI, worthIRI, lordIRI)),
                    generateValues()
                );

                // Push to db
                this.tsClient.addTimeSeriesData(timeseries, connection);
                LOGGER.debug("Generated time series for castle #{}", Integer.toString(i));
            }
        } catch(Exception exception) {
            LOGGER.error("Exception when attempting to generate sample time series!", exception);
        }
    }

    /**
     * Generate sample time data.
     * 
     * @return List of Instant objects.
     */
    private List<Instant> generateTimes() {
        List<Instant> times = new ArrayList<>();

        // Start from 15 days ago
        Instant start = Instant.now().minus(15, ChronoUnit.DAYS);
        times.add(start);

        // Add 29 other points, one per day
        for(int i = 1; i < 30; i++ ) {
            Instant newDay = start.plus(i, ChronoUnit.DAYS);
            times.add(newDay);
        }

        return times;
    }

    /**
     * Generate sample value data.
     * 
     * @return sample value data.
     */
    private List<List<?>> generateValues() {
        List<List<?>> values = new ArrayList<>();

        // Population values
        List<Integer> population = new ArrayList<>();
        for(int i = 0; i < 60; i += 2) {
            population.add((int) Math.pow(i, 3));
        }
        values.add(population);

        // Worth values
        List<Double> worth = new ArrayList<>();
        for(int i = 0; i < 30; i++) {
            worth.add(Math.random() * 1_000_000);
        }
        values.add(worth);

        // Lord values
        List<String> lords = new ArrayList<>();
        lords.add("Egbert");        lords.add("Aethelwulf");
        lords.add("Aethelbald");    lords.add("Aethelbert");
        lords.add("Aethelred");     lords.add("Alfred the Great");
        lords.add("Edmund");        lords.add("Athelstan");
        lords.add("Eadred");        lords.add("Eadwig");
        lords.add("Aethelred II");  lords.add("Edmund II");
        lords.add("Canute");        lords.add("Harold");
        lords.add("Harthacanute");  lords.add("Egbert");
        lords.add("Egbert");        lords.add("Edward the Confessor");
        lords.add("Harold II");     lords.add("William I");
        lords.add("William II");    lords.add("Henry I");
        lords.add("Stephen I");     lords.add("Henry II");
        lords.add("Richard I");     lords.add("John I");
        lords.add("Henry II");      lords.add("Edward I");
        lords.add("Edward II");     lords.add("Edward III");
        values.add(lords);

        return values;
    }

}
// End of class.