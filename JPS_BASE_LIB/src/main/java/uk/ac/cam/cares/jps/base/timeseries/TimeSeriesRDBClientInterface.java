package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

import org.jooq.SQLDialect;

interface TimeSeriesRDBClientInterface<T> {
    static final SQLDialect DIALECT = SQLDialect.POSTGRES;

    void setSchema(String schema);

    String getSchema();

    Class<T> getTimeClass();

    /**
     * Initialise a time series
     * <p>
     * For the list of supported classes, refer org.jooq.impl.SQLDataType
     * <p>
     * 
     * @param dataIRI   list of IRIs for the data provided as string
     * @param dataClass list with the corresponding Java class (typical String,
     *                  double or int) for each data IRI
     * @param tsIRI     IRI of the timeseries provided as string
     * @param conn      connection to the RDB
     */
    void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI,
            Connection conn);

    /**
     * Initialise a time series
     * <p>
     * For the list of supported classes, refer org.jooq.impl.SQLDataType
     * <p>
     * 
     * @param dataIRI   list of IRIs for the data provided as string
     * @param dataClass list with the corresponding Java class (typical String,
     *                  double or int) for each data IRI
     * @param tsIRI     IRI of the timeseries provided as string
     */
    void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI);

    /**
     * similar to above, but specifically to use with PostGIS
     * the additional argument srid is to restrict any geometry columns to the srid
     * if not needed, set to null, or use above the above method
     * 
     * @param dataIRI
     * @param dataClass
     * @param tsIRI
     * @param srid
     * @return
     */
    void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid);

    /**
     * similar to above, but specifically to use with PostGIS
     * the additional argument srid is to restrict any geometry columns to the srid
     * if not needed, set to null, or use above the above method
     * 
     * @param dataIRI
     * @param dataClass
     * @param tsIRI
     * @param srid
     * @param conn      connection to the RDB
     * @return
     */
    void initTimeSeriesTable(List<String> dataIRIList, List<Class<?>> dataClass, String tsIRI, Integer srid,
            Connection conn);

    /**
     * Append time series data to an already existing RDB table
     * If certain columns within the table are not provided, they will be nulls
     * 
     * @param ts_list TimeSeries object to add
     */
    void addTimeSeriesData(List<TimeSeries<T>> tsList);

    /**
     * Append time series data to an already existing RDB table
     * If certain columns within the table are not provided, they will be nulls
     * 
     * @param ts_list TimeSeries object to add
     * @param conn    connection to the RDB
     */
    public void addTimeSeriesData(List<TimeSeries<T>> tsList, Connection conn);

    /**
     * Delete time series data between lower and upper Bound
     * <p>
     * 
     * @param dataIRI    data IRI provided as string
     * @param lowerBound start timestamp from which to delete data
     * @param upperBound end timestamp until which to delete data
     * @param conn       connection to the RDB
     */
    void deleteRows(String dataIRI, T lowerBound, T upperBound, Connection conn);

    /**
     * Delete RDB time series table rows between lower and upper Bound
     * <p>
     * Note that this will delete the entire rows in the corresponding table, i.e.
     * all columns (in addition to the given data IRI)
     * 
     * @param dataIRI    data IRI provided as string
     * @param lowerBound start timestamp from which to delete data
     * @param upperBound end timestamp until which to delete data
     */
    void deleteRows(String dataIRI, T lowerBound, T upperBound);

    /**
     * Delete individual time series (i.e. data for one dataIRI only)
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    void deleteTimeSeries(String dataIRI, Connection conn);

    /**
     * Delete individual time series (i.e. data for one dataIRI only)
     * 
     * @param dataIRI data IRI provided as string
     */
    void deleteTimeSeries(String dataIRI);

    /**
     * Delete all time series information related to a dataIRI, including other
     * dataIRIs associated with this time series
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    void deleteEntireTimeSeries(String dataIRI, Connection conn);

    /**
     * Delete all time series information related to a dataIRI, including other
     * dataIRIs associated with this time series
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    void deleteEntireTimeSeries(String dataIRI);

    /**
     * Delete all time series RDB tables and central lookup table
     * 
     * @param conn connection to the RDB
     */
    void deleteAll(Connection conn);

    /**
     * Delete all time series RDB tables and central lookup table
     * 
     * @param conn connection to the RDB
     */
    void deleteAll();

    /**
     * Retrieve time series within bounds from RDB (time bounds are inclusive and
     * optional)
     * <p>
     * Returns all data series from dataIRI list as one time series object (with
     * potentially multiple related data series);
     * <br>
     * Returned time series are in ascending order with respect to time (from oldest
     * to newest)
     * <br>
     * Returned time series contain potential duplicates (i.e. multiple entries for
     * same time stamp)
     * 
     * @param dataIRI    list of data IRIs provided as string
     * @param lowerBound start timestamp from which to retrieve data (null if not
     *                   applicable)
     * @param upperBound end timestamp until which to retrieve data (null if not
     *                   applicable)
     * @param conn       connection to the RDB
     */
    TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound, Connection conn);

    /**
     * Retrieve entire time series from RDB
     * 
     * @param dataIRI list of data IRIs provided as string
     * @param conn    connection to the RDB
     */
    TimeSeries<T> getTimeSeries(List<String> dataIRI, Connection conn);

    /**
     * returns a TimeSeries object with the latest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    TimeSeries<T> getLatestData(String dataIRI, Connection conn);

    /**
     * returns a TimeSeries object with the oldest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    TimeSeries<T> getOldestData(String dataIRI, Connection conn);

    /**
     * Retrieve average value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The average of the provided data series as double
     */
    double getAverage(String dataIRI, Connection conn);

    /**
     * Retrieve maximum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The maximum of the provided data series as double
     */
    double getMaxValue(String dataIRI, Connection conn);

    /**
     * Retrieve minimum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The minimum of the provided data series as double
     */
    double getMinValue(String dataIRI, Connection conn);

    /**
     * Retrieve latest (maximum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The maximum (latest) timestamp of the provided data series
     */
    T getMaxTime(String dataIRI, Connection conn);

    /**
     * Retrieve earliest (minimum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The minimum (earliest) timestamp of the provided data series
     */
    T getMinTime(String dataIRI, Connection conn);

    /**
     * Get and set methods for private relational database properties (e.g.
     * PostgreSQL)
     */
    void setRdbURL(String rdbURL);

    String getRdbURL();

    void setRdbUser(String user);

    String getRdbUser();

    void setRdbPassword(String password);

    /**
     * Retrieve time series within bounds from RDB (time bounds are inclusive and
     * optional)
     * <p>
     * Returns all data series from dataIRI list as one time series object (with
     * potentially multiple related data series);
     * <br>
     * Returned time series are in ascending order with respect to time (from oldest
     * to newest)
     * <br>
     * Returned time series contain potential duplicates (i.e. multiple entries for
     * same time stamp)
     * 
     * @param dataIRI    list of data IRIs provided as string
     * @param lowerBound start timestamp from which to retrieve data (null if not
     *                   applicable)
     * @param upperBound end timestamp until which to retrieve data (null if not
     *                   applicable)
     */
    TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound);

    /**
     * Retrieve entire time series from RDB
     * 
     * @param dataIRI list of data IRIs provided as string
     */
    TimeSeries<T> getTimeSeries(List<String> dataIRI);

    /**
     * returns a TimeSeries object with the latest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    TimeSeries<T> getLatestData(String dataIRI);

    /**
     * returns a TimeSeries object with the oldest value of the given IRI
     * 
     * @param dataIRI
     */
    TimeSeries<T> getOldestData(String dataIRI);

    /**
     * Retrieve average value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The average of the provided data series as double
     */
    double getAverage(String dataIRI);

    /**
     * Retrieve maximum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The maximum of the provided data series as double
     */
    double getMaxValue(String dataIRI);

    /**
     * Retrieve minimum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The minimum of the provided data series as double
     */
    double getMinValue(String dataIRI);

    /**
     * Retrieve latest (maximum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @return The maximum (latest) timestamp of the provided data series
     */
    T getMaxTime(String dataIRI);

    /**
     * Retrieve earliest (minimum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @return The minimum (earliest) timestamp of the provided data series
     */
    T getMinTime(String dataIRI);

    /**
     * Check whether dataIRI has a tsIRI associated with it (i.e. dataIRI exists in
     * central lookup table)
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return True if the data IRI exists in central lookup table's dataIRI column,
     *         false otherwise
     */
    boolean checkDataHasTimeSeries(String dataIRI, Connection conn);

    boolean checkDataHasTimeSeries(String dataIRI);

    Connection getConnection() throws SQLException;
}