package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A decorator that interacts with the Time Series client in the JPS-base-lib dependency. It provides functionality specific to this agent.
 * Only Instant times are uploaded to the TS client.
 *
 * @author qhouyee
 */
class DateTSClientDecorator {
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalHouse45UtilitiesAgent.class);
    private static int[] dateArrays = null;
    private static String dateKey;
    private TimeSeriesClient<Instant> tsClient;
    private RemoteRDBStoreClient rdbClient;
    public final String timeUnit = Instant.class.getSimpleName(); // The time unit used by the Excel Workbook
    private final String instantTimeZone = "T00:00:00Z"; // UTC Time zone

    /**
     * Standard constructor
     */
    protected DateTSClientDecorator(String dateKey) {
        DateTSClientDecorator.dateKey = dateKey;
    }

    protected DateTSClientDecorator(String dateKey, int[] dateArrays) {
        DateTSClientDecorator.dateKey = dateKey;
        DateTSClientDecorator.dateArrays = dateArrays;
    }

    /**
     * Setter for the time series client.
     *
     * @param tsClient The time series client to use.
     */
    protected void setTsClient(TimeSeriesClient<Instant> tsClient) {
        this.tsClient = tsClient;
    }

    /**
     * Get the RDB client.
     */
    protected RemoteRDBStoreClient getRDBClient() {
        return this.rdbClient;
    }

    /**
     * Set a new RDB client.
     *
     * @param url      The RDB JDBC URL.
     * @param username The RDB's user name.
     * @param password The RDB's password.
     */
    protected void setRDBClient(String url, String username, String password) {
        this.rdbClient = new RemoteRDBStoreClient(url, username, password);
    }

    /**
     * Initializes all time series if they do not exist using the time series client.
     *
     * @param excelReadings Excel readings parsed from the Excel Workbook
     * @param iriMappings   Mappings between measures' names and their corresponding data IRI.
     */
    protected void initializeTimeSeries(Map<String, List<?>> excelReadings, Map<String, String> iriMappings) throws SQLException {
        List<String> excelHeaders = new ArrayList<>(iriMappings.keySet());
        List<String> iris = new ArrayList<>(iriMappings.values());
        // If IRIs do not have a time series linked, initialize the corresponding time series
        if (timeSeriesDoesNotExist(iris)) {
            // Get the classes (datatype) corresponding to each measure's name needed for initialization
            // excelReadings.get(header) returns array list, so add get method to get the value
            List<Class<?>> classes = excelHeaders.stream()
                    .map(header -> excelReadings.get(header).get(0).getClass())
                    .collect(Collectors.toList());
            try (Connection conn = rdbClient.getConnection()) {
                tsClient.initTimeSeries(iris, classes, this.timeUnit, conn, TimeSeriesClient.Type.STEPWISECUMULATIVE, null, null);
                LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
            }
        }
    }

    /**
     * Checks if a time series does not exist in the central RDB lookup table using the time series client.
     * This is achieved through ensuring that all the IRIs are linked and initialised to a time series.
     *
     * @param iris A list of string containing IRIs that should be attached to the same time series.
     * @return False if any IRI have a time series attached, true otherwise.
     */
    private boolean timeSeriesDoesNotExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for (String iri : iris) {
            try {
                if (!tsClient.checkDataHasTimeSeries(iri, rdbClient.getConnection())) {
                    return true;
                }
                // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return true;
                } else {
                    throw e;
                }
            } catch (SQLException e) {
                throw new JPSRuntimeException(e);
            }
        }
        return false;
    }

    /**
     * Updates the database with new readings.
     *
     * @param excelReadings Excel readings parsed from the Excel Workbook
     * @param iriMappings   Mappings between measures' names and their corresponding data IRI.
     */
    protected void updateData(Map<String, List<?>> excelReadings, Map<String, String> iriMappings) {
        TimeSeries<Instant> timeSeries = convertReadingsToTimeSeries(excelReadings, iriMappings);
        // Update each time series
        try {
            tsClient.addTimeSeriesData(timeSeries, rdbClient.getConnection());
        } catch (SQLException e) {
            throw new JPSRuntimeException(e);
        }
        LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", timeSeries.getDataIRIs())));
    }

    /**
     * Converts the readings in the form of hash maps to time series format for use with the time series client.
     * The time series must be in Instant class.
     *
     * @param readings    Readings stored in a hash map.
     * @param iriMappings Data IRI mappings to their measures' names.
     * @return A time series object.
     */
    private TimeSeries<Instant> convertReadingsToTimeSeries(Map<String, List<?>> readings, Map<String, String> iriMappings) {
        List<String> dataKeys = new ArrayList<>(iriMappings.keySet());
        List<String> iris = new ArrayList<>(iriMappings.values());
        List<Instant> instantItems = new ArrayList<>();
        // Handling dates values
        if (dateArrays == null) {
            List<LocalDateTime> dateValues = (List<LocalDateTime>) readings.get(dateKey);
            for (LocalDateTime dates : dateValues) {
                // Convert LocalDateTime to Instant class
                String instance = dates.toLocalDate().toString() +instantTimeZone;
                instantItems.add(Instant.parse(instance));
            }
        } else {
            List<LocalDate> dateItems = (List<LocalDate>) readings.get(dateKey);
            for (LocalDate date : dateItems){
                // Convert LocalDate to Instant class
                String instance = date.toString() +instantTimeZone;
                instantItems.add(Instant.parse(instance));
            }
        }

        List<List<?>> dataValues = new ArrayList<>();
        for (String key : dataKeys) {
            dataValues.add(readings.get(key));
        }
        // Create the time series object
        return new TimeSeries<>(instantItems, iris, dataValues);
    }
}
