package com.cmclinnovations.featureinfo.core.time;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.config.StackInteractor;
import com.cmclinnovations.featureinfo.config.TimeReference;
import com.cmclinnovations.featureinfo.utils.Utils;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This class handles the Knowledge Graph to determine measurement IRIs, then
 * passing these to the RDB to get time series values.
 */
public class TimeHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeHandler.class);

    /**
     * IRI of the asset.
     */
    private final String iri;

    /**
     * Optional enforced Blazegraph URL.
     */
    private final Optional<String> enforcedEndpoint;
    
    /**
     * Configuration store.
     */
    private final ConfigStore configStore;

    /**
     * Communications with KG.
     */
    private RemoteStoreClient kgClient;

    /**
     * Communications with RDB. 
     */
    private RemoteRDBStoreClient dbClient;

    /**
     * Time series client, limited to Instant due to design.
     */
    private TimeSeriesClient<Instant> tsClient;

    /**
     * Initialise a new TimeHandler instance.
     * 
     * @param iri IRI of the asset.
     * @param enforcedEndpoint optional enforced Blazegraph URL.
     * @param configStore Store of class mappings and stack endpoints.
     */
    public TimeHandler(String iri, Optional<String> enforcedEndpoint, ConfigStore configStore) {
        this.iri = iri;
        this.enforcedEndpoint = enforcedEndpoint;
        this.configStore = configStore;
    }

    /**
     * Sets the remote store clients used to connect to the KG.
     * 
     * @param kgClient KG connection client.
     */
    public void setClients(
        RemoteStoreClient kgClient,
        TimeSeriesClient<Instant> tsClient,
        RemoteRDBStoreClient dbClient) {

        this.kgClient = kgClient;
        this.tsClient = tsClient;
        this.dbClient = dbClient;
    }

    /**
     * Queries the KG to determine measurement IRIs, the passes these onto the 
     * relational database to get time series values.
     * 
     * @param classMatches configuration entries that contain class matches.
     * @param response HTTP response to write to.
     * 
     * @return JSONObject of query result.
     */
    public JSONArray getData(List<ConfigEntry> classMatches, HttpServletResponse response) {
        // Pool for all measureables across all class matches
        List<Measurable> allMeasurables = new ArrayList<>();

        // Iterate through each matching query
        classMatches.forEach(classMatch -> {
            try {
                if(classMatch.getTimeQueryContent() == null || classMatch.getTimeQueryContent().isEmpty()) {
                    LOGGER.info("No time configuration set for class match, skipping: {}", classMatch.getClassIRI());
                } else {
                    // Get measurable entities
                    LOGGER.info("Processing time series queries for class match: {}", classMatch.getClassIRI());
                    allMeasurables.addAll(getMeasurables(classMatch));
                }
            } catch(Exception exception) {
                LOGGER.error("Execution for time series acquisition has failed!", exception);
            }
        });
        LOGGER.debug("Detected {} measurable instances.", allMeasurables.size());

        // Group measurables by the database their time series is defined within
        Map<String, List<Measurable>> groupByDatabase = allMeasurables
            .stream()
            .collect(
                Collectors.groupingBy((measurable) -> measurable.getConfigEntry().getTimeDatabase())
            );
        LOGGER.debug("Time series queries will take place across {} distinct databases.", groupByDatabase.size());

        // Get postgres endpoint (assume there's one)
        StackEndpoint rdbEndpoint = this.configStore.getStackEndpoints(StackEndpointType.POSTGRES).get(0);

        // Pool of all constructed and populated timeseries objects
        Map<TimeSeries<Instant>, List<Measurable>> allTimeSeries = new LinkedHashMap<>();

        // Iterate through unique database names
        for(Map.Entry<String, List<Measurable>> entryByDB : groupByDatabase.entrySet()) {
            LOGGER.debug("Running queries for time series in database: {}", entryByDB);

            // Connect to a new database
            try (Connection connection = connectToDatabase(rdbEndpoint, entryByDB.getKey())) {

                // Group measurables by their time series IRIs
                Map<String, List<Measurable>> groupByTimeSeries = entryByDB.getValue()
                    .stream()
                    .collect(
                        Collectors.groupingBy(Measurable::getTimeSeriesIRI)
                    );
                LOGGER.debug("There are {} distinct time series in database.", groupByTimeSeries.size());

                // Iterate through unique timeseries IRIs
                for(Map.Entry<String, List<Measurable>> entryByTime : groupByTimeSeries.entrySet()) {

                    // Group measurable again, by shared configuration entries
                    // This is because whilst they share a timeseries IRI, each configuration
                    // entry may define different value bounds.
                    Map<ConfigEntry, List<Measurable>> groupedByConfigs = entryByTime.getValue()
                        .stream()
                        .collect(
                            Collectors.groupingBy(Measurable::getConfigEntry)
                        );

                    // Iterate through unique config entries
                    for(Map.Entry<ConfigEntry, List<Measurable>> entryByConfig : groupedByConfigs.entrySet()) {
                        ConfigEntry thisConfig = entryByConfig.getKey();
                        List<Measurable> theseMeasurables = entryByConfig.getValue();

                        // Get populated time series object from client
                        LOGGER.debug("Getting data for time series IRI: {}", entryByTime.getKey());

                        TimeSeries<Instant> timeseries = getTimeSeries(thisConfig, connection, theseMeasurables);
                        LOGGER.debug("Got populated time series instance from the client.");

                        timeseries.getDataIRIs().forEach(dataIRI -> {
                            LOGGER.debug("There are {} values for measurable IRI: {}", timeseries.getValues(dataIRI).size(), dataIRI);
                        });

                        if(timeseries != null && timeseries.getTimes() != null && !timeseries.getTimes().isEmpty()) {
                            allTimeSeries.put(timeseries, theseMeasurables);
                        } else {
                            LOGGER.debug("Returned time series has no time entries, skipping: {}", entryByTime.getKey());
                        }
                    }
                }

            } catch(Exception exception) {
                LOGGER.error("Exception occurered when connecting to RDB!", exception);
            }
        }


        // Convert all timeseries to JSON objects.
        LOGGER.debug("Converting time series data to JSON...");
        return convertTimeSeries(allTimeSeries);
    }

    /**
     * Queries the KG and returns details on measurements for the class match.
     * 
     * @param classMatch configuration entry.
     * 
     * @returns List of discovered measurement objects.
     * 
     * @throws Exception if SPARQL queries fail.
     */
    private List<Measurable> getMeasurables(ConfigEntry classMatch) throws Exception {
        String templateQuery = classMatch.getTimeQueryContent();
        String query = Utils.queryInject(
            templateQuery,
            this.iri,
            configStore.getStackEndpoints(StackEndpointType.ONTOP),
            Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint)
        );

        // Run query
        List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);
        JSONArray result = null;

        if(endpoints.size() == 1) {
            LOGGER.debug("Running non-federated measurement IRI query.");
            kgClient.setQueryEndpoint(endpoints.get(0));
            result = this.kgClient.executeQuery(query);
            
        } else {
            LOGGER.debug("Running federated measurement IRI query.");
            result = this.kgClient.executeFederatedQuery(endpoints, query);
        }

        // Parse results
        List<Measurable> measurables = new ArrayList<>();
        for(int i = 0; i < result.length(); i++) {
            JSONObject obj = result.getJSONObject(i);
            Measurable measurable = MeasurableBuilder.build(classMatch, obj);

            if(measurable != null) {
                LOGGER.debug("Discovered measurable with data IRI: {}", measurable.getEntityIRI());
                if(measurable.getTimeSeriesIRI() != null) {
                    LOGGER.debug("Measurable has parent time series: {}", measurable.getTimeSeriesIRI());
                }
                measurables.add(measurable);
            }
        }

        // Fill in timeseries IRI if missing
        this.populateTimeSeriesIRIs(measurables);
        return measurables;
    }

    /**
     * For the input list of measurable objects, this method iterates through them an runs
     * new queries to determine the IRI of the linked time series if that was not provided
     * in the original KG query.
     * 
     * @param measurements configuration entry.
     * @param classMatch discovered measurable objects.
     */
    private void populateTimeSeriesIRIs(List<Measurable> measurables) {
        measurables.forEach(measurable -> {
            try {
                MeasurableBuilder.populateTimeSeriesIRIs(
                    measurable,
                    configStore,
                    enforcedEndpoint,
                    kgClient
                );
            } catch(Exception exception) {
                LOGGER.error("Could not determine timeseries IRI for measurable object!", exception);
            }
        });
    }

    /**
     * For the input classMatch and discovered measurements, build and populate the 
     * TimeSeries instances using the relational databases.
     * 
     * Note that this currently assumes all TimeSeries objects were created using the Instant
     * class; this is a current limitation in the time series design.
     * 
     * @param classMatch configuration entry.
     * @param connection cached DB connection.
     * @param measurements discovered measurement objects.
     * 
     * @return List of populated TimeSeries objects.
     */
    private TimeSeries<Instant> getTimeSeries(ConfigEntry classMatch, Connection connection, List<Measurable> measurables) {
        // Iterate through discovered measurements to get IRIs
        List<String> measurableIRIs = measurables
            .stream()
            .map(measurable -> measurable.getEntityIRI())
            .collect(Collectors.toList());
        
        // Calculate time bounds
        Pair<Instant, Instant> bounds = calculateBounds(classMatch, connection, measurables);

        // Call client to get TimeSeries object
        if(bounds == null) {
            LOGGER.debug("Calling TimeSeriesClient without time bounds.");

            return this.tsClient.getTimeSeries(
                measurableIRIs,
                connection
            );

        } else {
            Instant lowerBound = bounds.getLeft().isBefore(bounds.getRight()) ? bounds.getLeft() : bounds.getRight();
            Instant upperBound = bounds.getLeft().isAfter(bounds.getRight()) ? bounds.getLeft() : bounds.getRight();

            LOGGER.debug("Calculated lower time bound as: {}", lowerBound);
            LOGGER.debug("Calculated upper time bound as: {}", upperBound);

            return this.tsClient.getTimeSeriesWithinBounds(
                measurableIRIs,
                lowerBound,
                upperBound,
                connection
            );
        }
    }

    /**
     * Given the configuration entry for a class match, and the resulting measurable IRIs,
     * determine the time limit bounds to be given to the TimeSeriesClient when grabbing
     * data for all measurable instances.
     * 
     * @param classMatch class match object.
     * @param connection cached RDB connection.
     * @param measurementIRI measurement IRI.
     * 
     * @return Pair of time bounds (not in order)
     */
    private Pair<Instant, Instant> calculateBounds(ConfigEntry classMatch, Connection connection, List<Measurable> measurables) {
        TimeReference reference = classMatch.getTimeReference();
        int timeLimit = classMatch.getTimeLimitValue();
        TimeUnit timeUnit = classMatch.getTimeLimitUnit();
        
        // All these measurables should have the same bounds as they come from
        // a single time series, so just use IRI of first measurable
        String measureIRI = measurables.get(0).getEntityIRI();

        switch(reference) {
            case NOW: {
                Instant boundOne = LocalDateTime.now().toInstant(ZoneOffset.UTC);
                Instant boundTwo = offsetTime(timeLimit * -1, timeUnit, boundOne);
                return new ImmutablePair<>(boundOne, boundTwo);
            }

            case LATEST: {
                TimeSeries<Instant> latest = this.tsClient.getLatestData(measureIRI, connection);
                Instant boundOne = latest.getTimes().get(0);
                Instant boundTwo = offsetTime(timeLimit * -1, timeUnit, boundOne);
                return new ImmutablePair<>(boundOne, boundTwo);
            }

            case FIRST: {
                TimeSeries<Instant> first = this.tsClient.getOldestData(measureIRI, connection);
                Instant boundOne = first.getTimes().get(0);
                Instant boundTwo = offsetTime(timeLimit, timeUnit, boundOne);
                return new ImmutablePair<>(boundOne, boundTwo);
            }

            default:
                return null;
        }
    }

    /**
     * Returns a copy of the input Instant with the input offset.
     * 
     * @param offsetValue value to offset.
     * @param unit time unit.
     * @param toOffset Instant to copy.
     * 
     * @return copy of Instant with offset.
     */
    private Instant offsetTime(int offsetValue, TimeUnit unit, Instant toOffset) {
        switch(unit) {
            case NANOSECONDS:
                return toOffset.plusNanos(offsetValue);
            case MICROSECONDS:
                return toOffset.plusMillis(offsetValue / 1000);
            case MILLISECONDS:
                return toOffset.plusMillis(offsetValue);
            case SECONDS:
                return toOffset.plusSeconds(offsetValue);
            case MINUTES:
                return toOffset.plusSeconds(offsetValue * 60);
            case HOURS:
                return toOffset.plusSeconds(offsetValue * 3600);
            case DAYS: 
                return toOffset.plusSeconds(offsetValue * 86_400);
        }
        return null;
    }

    /**
     * Converts all timeseries instances into a single JSON representation.
     * 
     * @param allTimeSeries timeseries to convert.
     * 
     * @return JSON representation.
     */
    private JSONArray convertTimeSeries(Map<TimeSeries<Instant>, List<Measurable>> allTimeSeries) {
        JSONArray combinedArray = new JSONArray();

        // Iterate through entries
        for(Map.Entry<TimeSeries<Instant>, List<Measurable>> entry : allTimeSeries.entrySet()) {
            TimeSeries<Instant> timeseries = entry.getKey();
            List<Measurable> measurables = entry.getValue();

            // Extract units and names for measurables
            Map<String, String> unitsMap = new LinkedHashMap<>();
            Map<String, String> namesMap = new LinkedHashMap<>();

            measurables.forEach(measurable -> {
                unitsMap.put(measurable.getEntityIRI(), measurable.getUnit());
                namesMap.put(measurable.getEntityIRI(), measurable.getName());
            });

            // Convert to JSON
            JSONArray jsonTime = TimeParser.convertToJSON(
                timeseries,
                unitsMap,
                namesMap
            );

            if(jsonTime != null) {
                // Append objects to combined array
                for(int i = 0; i < jsonTime.length(); i++) {
                    combinedArray.put(jsonTime.getJSONObject(i));
                }
            }
        }
        return combinedArray;
    }

    /**
     * Re-initialises the RDB client with a connection to the input database.
     * 
     * @param rdbEndpoint endpoint for postgres.
     * @param database database name.
     * 
     * @throws SQLException if database cannot be connected to.
     */
    protected Connection connectToDatabase(StackEndpoint rdbEndpoint, String database) throws SQLException {
        if(rdbEndpoint == null || database == null) return null;

        LOGGER.info("Making new connection to database: {}", database);
        String postgresURL = StackInteractor.generatePostgresURL(database);
        
        // Create new connection
        this.dbClient = new RemoteRDBStoreClient(
            postgresURL,
            rdbEndpoint.username(),
            rdbEndpoint.password()
        );
        return this.dbClient.getConnection();
    }

}
// End of class.