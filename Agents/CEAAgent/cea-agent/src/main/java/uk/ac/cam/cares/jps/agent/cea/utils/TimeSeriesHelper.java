package uk.ac.cam.cares.jps.agent.cea.utils;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.jooq.exception.DataAccessException;

public class TimeSeriesHelper {
    public static final String timeUnit = Instant.class.getSimpleName();
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Instant> tsClient;

    public TimeSeriesHelper(RemoteStoreClient remoteStoreClient, RemoteRDBStoreClient remoteRDBStoreClient, String dbName) {
        this.storeClient = remoteStoreClient;
        this.rdbStoreClient = remoteRDBStoreClient;
        ensureDatabaseExist(dbName);
    }

    private void ensureDatabaseExist(String dbName) {
        try (Connection conn = rdbStoreClient.getConnection()) {
            return;
        } catch (SQLException e) {
            if ("3D000".equals(e.getSQLState())) {
                // Database does not exist
                PostGISClient.getInstance().createDatabase(dbName);
            } else {
                throw new RuntimeException("Unexpected SQLstate: " + e.getSQLState(), e);
            }
        }
    }

    /**
     * Creates and initialises a time series using the time series client
     * @param fixedIris map containing time series iris mapped to measurement type
     */
    public void createTimeSeries(LinkedHashMap<String,String> fixedIris) {
        tsClient = new TimeSeriesClient<>(storeClient, Instant.class);

        // Create a iri for each measurement
        List<String> iris = new ArrayList<>();
        for(String measurement: CEAConstants.TIME_SERIES){
            String iri = measurement+"_"+ UUID.randomUUID()+ "/";
            iri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + iri ;
            iris.add(iri);
            fixedIris.put(measurement, iri);
        }

        // Check whether IRIs have a time series linked and if not initialize the corresponding time series
        if(!timeSeriesExist(iris)) {
            // All values are doubles
            List<Class<?>> classes =  new ArrayList<>();
            for (int i = 0; i < iris.size(); i++) {
                classes.add(Double.class);
            }

            try (Connection conn = rdbStoreClient.getConnection()) {
                // Initialize the time series
                tsClient.initTimeSeries(iris, classes, timeUnit, conn, TimeSeriesClient.Type.STEPWISECUMULATIVE, null, null);
            }
            catch (SQLException e) {
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Adds new data to time series
     * @param values output CEA data
     * @param times times for output time series data
     * @param iriMap iri map containing time series iris
     */
    public void addDataToTimeSeries(List<List<?>> values, List<OffsetDateTime> times, LinkedHashMap<String,String> iriMap) {
        List<String> iris = new ArrayList<>();

        for (String iri : iriMap.values()) {
            iris.add(iri);
        }

        // If CreateTimeSeries has not been run, get time series client
        if (tsClient == null) {
            tsClient = new TimeSeriesClient<>(storeClient, Instant.class);
        }

        // Time series must be in Instant for feature info agent to work

        List<Instant> instants = times.stream().map(OffsetDateTime::toInstant).collect(Collectors.toList());


        TimeSeries<Instant> currentTimeSeries = new TimeSeries<>(instants, iris, values);

        try (Connection conn = rdbStoreClient.getConnection()) {
            
            // Add New data
            tsClient.addTimeSeriesData(currentTimeSeries, conn);
        }
        catch (SQLException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Checks whether a time series exists by checking whether any of the IRIs that should be attached to
     * the time series is not initialised in the central RDB lookup table using the time series client.
     * @param iris The IRIs that should be attached to the same time series provided as list of strings.
     * @return true if all IRIs have a time series attached, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for(String iri: iris) {
            try {
                try (Connection conn = rdbStoreClient.getConnection()) {
                    if (!tsClient.checkDataHasTimeSeries(iri, conn)) {
                        return false;
                    }
                }
                catch (SQLException e) {
                    throw new JPSRuntimeException(e);
                }
                // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return false;
                }
                else {
                    throw e;
                }
            }
        }
        return true;
    }

    /**
     * Returns data using time series client for given data iri
     * @param dataIri iri in time series database
     * @return time series data
     */
    public static <T> TimeSeries<T> retrieveData(String dataIri, RemoteStoreClient store, RemoteRDBStoreClient rdbStore, Class<T> timeClass) {
        TimeSeriesClient<T> client = new TimeSeriesClient<>(store, timeClass);

        List<String> iris = new ArrayList<>();
        iris.add(dataIri);
        try (Connection conn = rdbStore.getConnection()) {
            TimeSeries<T> data = client.getTimeSeries(iris, conn);
            return data;
        }
        catch (SQLException e) {
            throw new JPSRuntimeException(e);
        }
    }
}
