package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.Condition;
import org.jooq.CreateSchemaFinalStep;
import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Query;
import org.jooq.Field;
import org.jooq.InsertValuesStep4;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Record2;
import org.jooq.Result;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.jooq.impl.DefaultDataType;
import org.postgis.Geometry;

import static org.jooq.impl.DSL.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class uses the jooq library to interact with a relational database.
 * <T> is the class type for the time values, e.g. LocalDateTime, Timestamp,
 * Integer, Double etc.
 * 
 * @author Kok Foong Lee
 *
 *         Update:
 *         This class should not be used directly. It should only be accessed
 *         internally via {@link TimeSeriesClient TimeSeriesClient}
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 */

public class TimeSeriesRDBClient<T> implements TimeSeriesRDBClientInterface<T> {
    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeSeriesRDBClient.class);
    // URL and credentials for the relational database
    private String rdbURL = null;
    private String rdbUser = null;
    private String rdbPassword = null;
    private String schema = null;
    // Time series column field (for RDB)
    private final Field<T> timeColumn;
    // Constants
    // Central database table
    private static final String DB_TABLE_NAME = "dbTable";
    private static final Field<String> DATA_IRI_COLUMN = DSL.field(DSL.name("dataIRI"), String.class);
    private static final Field<String> TS_IRI_COLUMN = DSL.field(DSL.name("timeseriesIRI"), String.class);
    private static final Field<String> TABLENAME_COLUMN = DSL.field(DSL.name("tableName"), String.class);
    private static final Field<String> COLUMNNAME_COLUMN = DSL.field(DSL.name("columnName"), String.class);
    // Exception prefix
    private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";
    // Error message
    private static final String CONNECTION_ERROR = "Failed to connect to database";
    private static final String SQL_ERROR = "Error while executing SQL command";
    private static final String NO_TS_INST_ERROR = "%s<%s> does not have an assigned time series instance";

    // Java class of time values
    private Class<T> timeClass;

    // Allowed aggregation function
    protected enum AggregateFunction {
        AVERAGE,
        MAX,
        MIN
    }

    /**
     * Standard constructor
     * 
     * @param timeClass class of the timestamps of the time series
     */
    public TimeSeriesRDBClient(Class<T> timeClass) {
        timeColumn = DSL.field(DSL.name("time"), timeClass);
        this.timeClass = timeClass;
    }

    @Override
    public Class<T> getTimeClass() {
        return timeClass;
    }

    @Override
    public void setSchema(String schema) {
        this.schema = schema;
    }

    @Override
    public String getSchema() {
        if (schema == null) {
            return "public";
        } else {
            return schema;
        }
    }

    /**
     * Initialise RDB table for particular time series and add respective entries to
     * central lookup table
     * <p>
     * For the list of supported classes, refer org.jooq.impl.SQLDataType
     * <p>
     * The timeseries IRI needs to be provided. A unique uuid for the corresponding
     * table will be generated.
     * 
     * @param dataIRI   list of IRIs for the data provided as string
     * @param dataClass list with the corresponding Java class (typical String,
     *                  double or int) for each data IRI
     * @param tsIRI     IRI of the timeseries provided as string
     * @param conn      connection to the RDB
     */
    @Override
    public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI,
            Connection conn) {
        initTimeSeriesTable(dataIRI, dataClass, tsIRI, null, conn);
    }

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
    @Override
    public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid,
            Connection conn) {

        // Generate UUID as unique RDB table name
        String tsTableName = UUID.randomUUID().toString();

        // All database interactions in try-block to ensure closure of connection
        try {

            // initialise schema and central table
            if (schema != null) {
                initSchemaIfNotExists(conn);
            }
            if (!checkCentralTableExists(conn)) {
                initCentralTable(conn);
            }

            // Check if any data has already been initialised (i.e. is associated with
            // different tsIRI)
            String faultyDataIRI = checkAnyDataHasTimeSeries(dataIRI, conn);
            if (faultyDataIRI != null) {
                throw new JPSRuntimeException(
                        exceptionPrefix + "<" + faultyDataIRI + "> already has an assigned time series instance");
            }

            // Ensure that there is a class for each data IRI
            if (dataIRI.size() != dataClass.size()) {
                throw new JPSRuntimeException(
                        exceptionPrefix + "Length of dataClass is different from number of data IRIs");
            }

            // Assign column name for each dataIRI; name for time column is fixed
            Map<String, String> dataColumnNames = new HashMap<>();
            int i = 1;
            for (String s : dataIRI) {
                dataColumnNames.put(s, "column" + i);
                i++;
            }

            // Add corresponding entries in central lookup table
            populateCentralTable(tsTableName, dataIRI, dataColumnNames, tsIRI, conn);

            // Initialise RDB table for storing time series data
            createEmptyTimeSeriesTable(tsTableName, dataColumnNames, dataIRI, dataClass, srid, conn);
        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * bulk version to above
     * 
     * @param dataIRIs
     * @param dataClasses
     * @param tsIRIs
     * @param srid
     * @param conn        connection to the RDB
     * @return
     */
    @Override
    public List<Integer> bulkInitTimeSeriesTable(List<List<String>> dataIRIs, List<List<Class<?>>> dataClasses,
            List<String> tsIRIs, Integer srid, Connection conn) {

        // initialise schema and central table
        if (schema != null) {
            initSchemaIfNotExists(conn);
        }
        if (!checkCentralTableExists(conn)) {
            initCentralTable(conn);
        } else {
            // Check if any data has already been initialised (i.e. is associated with
            // different tsIRI)
            List<String> flatDataIRIs = dataIRIs.stream().flatMap(List::stream).collect(Collectors.toList());
            String faultyDataIRI = checkAnyDataHasTimeSeries(flatDataIRIs, conn);
            if (faultyDataIRI != null) {
                LOGGER.error("At least one data series already has an assigned time series instance");
                // Assume all data IRIs have failed at the moment
                return IntStream.range(0, dataIRIs.size()).boxed().collect(Collectors.toList());
            }
        }

        // Ensure that there is a class for each data IRI
        for (int i = 0; i < dataIRIs.size(); i++) {
            if (dataIRIs.get(i).size() != dataClasses.get(i).size()) {
                LOGGER.error("Length of dataClass is different from number of data IRIs");
                // Assume all data IRIs have failed at the moment
                return IntStream.range(0, dataIRIs.size()).boxed().collect(Collectors.toList());
            }
        }

        // Generate UUID as unique RDB table name

        List<String> tsTableNames = IntStream.range(0, dataIRIs.size()).mapToObj(i -> UUID.randomUUID().toString())
                .collect(Collectors.toList());

        // Assign column name for each dataIRI; name for time column is fixed

        List<Map<String, String>> dataColumnNamesMaps = new ArrayList<>();

        for (int i = 0; i < dataIRIs.size(); i++) {
            List<String> dataIRI = dataIRIs.get(i);
            Map<String, String> dataColumnNames = new HashMap<>();
            int j = 1;
            for (String s : dataIRI) {
                dataColumnNames.put(s, "column" + j);
                j++;
            }
            dataColumnNamesMaps.add(dataColumnNames);
        }

        // All database interactions in try-block to ensure closure of connection

        try {
            // Add corresponding entries in central lookup table
            bulkPopulateCentralTable(tsTableNames, dataIRIs, dataColumnNamesMaps, tsIRIs, conn);
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Assume all data IRIs have failed at the moment
            return IntStream.range(0, dataIRIs.size()).boxed().collect(Collectors.toList());
        }

        return bulkCreateEmptyTimeSeriesTable(tsTableNames, dataColumnNamesMaps, dataIRIs, dataClasses, srid, conn);

    }

    /**
     * Append time series data to an already existing RDB table
     * If certain columns within the table are not provided, they will be nulls
     * 
     * @param ts_list TimeSeries object to add
     * @param conn    connection to the RDB
     */
    @Override
    public void addTimeSeriesData(List<TimeSeries<T>> tsList, Connection conn) {

        // Check if central database lookup table exists
        if (!checkCentralTableExists(conn)) {
            throw new JPSRuntimeException(
                    exceptionPrefix + "Central RDB lookup table has not been initialised yet");
        }

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection

        try {
            Statement statement = conn.createStatement();

            List<String> listStmt = tsList.parallelStream().map(ts -> {
                List<String> dataIRI = ts.getDataIRIs();

                // Ensure that all provided dataIRIs/columns are located in the same RDB table
                // (throws Exception if not)
                checkDataIsInSameTable(dataIRI, context);

                String tsTableName = getTimeseriesTableName(dataIRI.get(0), context);
                // Assign column name for each dataIRI; name for time column is fixed
                Map<String, String> dataColumnNames = bulkGetColumnName(dataIRI, context);

                // Append time series data to time series table
                // if a row with the time value exists, that row will be updated instead of
                // creating a new row
                return populateTimeSeriesTable(tsTableName, ts, dataIRI, dataColumnNames, conn);
            }).collect(Collectors.toList());

            for (String stmt : listStmt) {
                statement.addBatch(stmt);
            }

            statement.executeBatch();

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }
    }

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
    public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {

            // Check if central database lookup table exists
            if (!checkCentralTableExists(conn)) {
                throw new JPSRuntimeException(
                        exceptionPrefix + "Central RDB lookup table has not been initialised yet");
            }

            // Ensure that all provided dataIRIs/columns are located in the same RDB table
            // (throws Exception if not)
            checkDataIsInSameTable(dataIRI, context);

            // Retrieve table corresponding to the time series connected to the data IRIs
            Table<?> table = getTimeseriesTable(dataIRI.get(0), context);

            // Create map between data IRI and the corresponding column field in the table
            Map<String, Field<Object>> dataColumnFields = new HashMap<>();
            for (String data : dataIRI) {
                String columnName = getColumnName(data, context);
                Field<Object> field = DSL.field(DSL.name(columnName));
                dataColumnFields.put(data, field);
            }

            // Retrieve list of column fields (including fixed time column)
            List<Field<?>> columnList = new ArrayList<>();
            columnList.add(timeColumn);
            for (String data : dataIRI) {
                columnList.add(dataColumnFields.get(data));
            }

            // Potentially update bounds (if no bounds were provided)
            if (lowerBound == null) {
                lowerBound = context.select(min(timeColumn)).from(table).fetch(min(timeColumn)).get(0);
            }
            if (upperBound == null) {
                upperBound = context.select(max(timeColumn)).from(table).fetch(max(timeColumn)).get(0);
            }

            // Perform query
            Result<? extends Record> queryResult = context.select(columnList).from(table)
                    .where(timeColumn.between(lowerBound, upperBound))
                    .orderBy(timeColumn.asc()).fetch();

            // Collect results and return a TimeSeries object
            List<T> timeValues = queryResult.getValues(timeColumn);
            List<List<?>> dataValues = new ArrayList<>();
            for (String data : dataIRI) {
                List<?> column = queryResult.getValues(dataColumnFields.get(data));
                dataValues.add(column);
            }

            return new TimeSeries<>(timeValues, dataIRI, dataValues);

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Retrieve entire time series from RDB
     * 
     * @param dataIRI list of data IRIs provided as string
     * @param conn    connection to the RDB
     */
    public TimeSeries<T> getTimeSeries(List<String> dataIRI, Connection conn) {
        return getTimeSeriesWithinBounds(dataIRI, null, null, conn);
    }

    /**
     * returns a TimeSeries object with the latest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    public TimeSeries<T> getLatestData(String dataIRI, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        try {
            Table<?> tsTable = getTimeseriesTable(dataIRI, context);
            String columnName = getColumnName(dataIRI, context);

            Field<Object> dataField = DSL.field(DSL.name(columnName));

            Result<? extends Record> queryResult = context.select(timeColumn, dataField).from(tsTable)
                    .where(dataField.isNotNull())
                    .orderBy(timeColumn.desc()).limit(1).fetch();

            List<T> timeValues = queryResult.getValues(timeColumn);
            List<?> dataValues = queryResult.getValues(dataField);

            return new TimeSeries<>(timeValues, Arrays.asList(dataIRI), Arrays.asList(dataValues));
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }
    }

    /**
     * returns a TimeSeries object with the oldest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    public TimeSeries<T> getOldestData(String dataIRI, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        try {
            Table<?> tsTable = getTimeseriesTable(dataIRI, context);
            String columnName = getColumnName(dataIRI, context);

            Field<Object> dataField = DSL.field(DSL.name(columnName));

            Result<? extends Record> queryResult = context.select(timeColumn, dataField).from(tsTable)
                    .where(dataField.isNotNull())
                    .orderBy(timeColumn.asc()).limit(1).fetch();

            List<T> timeValues = queryResult.getValues(timeColumn);
            List<?> dataValues = queryResult.getValues(dataField);

            return new TimeSeries<>(timeValues, Arrays.asList(dataIRI), Arrays.asList(dataValues));
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }
    }

    /**
     * Retrieve average value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The average of the provided data series as double
     */
    public double getAverage(String dataIRI, Connection conn) {
        return getAggregate(dataIRI, AggregateFunction.AVERAGE, conn);
    }

    /**
     * Retrieve maximum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The maximum of the provided data series as double
     */
    public double getMaxValue(String dataIRI, Connection conn) {
        return getAggregate(dataIRI, AggregateFunction.MAX, conn);
    }

    /**
     * Retrieve minimum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The minimum of the provided data series as double
     */
    public double getMinValue(String dataIRI, Connection conn) {
        return getAggregate(dataIRI, AggregateFunction.MIN, conn);
    }

    /**
     * Retrieve latest (maximum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The maximum (latest) timestamp of the provided data series
     */
    public T getMaxTime(String dataIRI, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {

            // Retrieve table corresponding to the time series connected to the data IRI
            Table<?> table = getTimeseriesTable(dataIRI, context);

            List<T> queryResult = context.select(max(timeColumn)).from(table).fetch(max(timeColumn));

            return queryResult.get(0);

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Retrieve earliest (minimum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     * @return The minimum (earliest) timestamp of the provided data series
     */
    public T getMinTime(String dataIRI, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {
            // Retrieve table corresponding to the time series connected to the data IRI
            Table<?> table = getTimeseriesTable(dataIRI, context);

            List<T> queryResult = context.select(min(timeColumn)).from(table).fetch(min(timeColumn));

            return queryResult.get(0);

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Delete RDB time series table rows between lower and upper Bound
     * <p>
     * Note that this will delete the entire rows in the corresponding table, i.e.
     * all columns (in addition to the given data IRI)
     * 
     * @param dataIRI    data IRI provided as string
     * @param lowerBound start timestamp from which to delete data
     * @param upperBound end timestamp until which to delete data
     * @param conn       connection to the RDB
     */
    @Override
    public void deleteRows(String dataIRI, T lowerBound, T upperBound, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {
            // Retrieve RDB table for dataIRI
            Table<?> table = getTimeseriesTable(dataIRI, context);

            // Delete rows between bounds (including bounds!)
            context.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Delete individual time series (i.e. data for one dataIRI only)
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    @Override
    public void deleteTimeSeries(String dataIRI, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {

            // Get time series RDB table
            String columnName = getColumnName(dataIRI, context);
            String tsTableName = getTimeseriesTableName(dataIRI, context);

            // Retrieve number of columns of time series table (i.e. number of dataIRI +
            // time column)
            if (getNumberOfColumns(conn, tsTableName) > 2) {

                // Delete only column for dataIRI from RDB table if further columns are present
                context.alterTable(getDSLTable(tsTableName)).drop(columnName).execute();

                // Delete entry in central lookup table
                context.delete(getDSLTable(DB_TABLE_NAME)).where(DATA_IRI_COLUMN.equal(dataIRI)).execute();

            } else {
                // Delete entire RDB table for single column time series (data column + time
                // column)
                deleteEntireTimeSeries(dataIRI, conn);
            }

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    private int getNumberOfColumns(Connection conn, String tsTableName) {
        DSLContext context = DSL.using(conn, DIALECT);
        Table<Record> columnsTable = DSL.table(DSL.name("information_schema", "columns"));
        Field<String> tableNameColumn = DSL.field("table_name", String.class);

        Condition condition = tableNameColumn.eq(tsTableName);
        if (schema != null) {
            Field<String> schemaColumn = DSL.field("table_schema", String.class);
            condition.and(schemaColumn.eq(schema));
        }

        return context.select(count()).from(columnsTable).where(condition).fetchOne(0, int.class);
    }

    /**
     * Delete all time series information related to a dataIRI (i.e. entire RDB
     * table and entries in central table)
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    @Override
    public void deleteEntireTimeSeries(String dataIRI, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {

            // Retrieve RDB table for dataIRI
            String tsIRI = getTimeSeriesIRI(dataIRI, context);
            String tsTableName = getTimeseriesTableName(dataIRI, context);

            // Delete time series RDB table
            context.dropTable(getDSLTable(tsTableName)).execute();

            // Delete entries in central lookup table
            context.delete(getDSLTable(DB_TABLE_NAME)).where(TS_IRI_COLUMN.equal(tsIRI)).execute();

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Delete all time series RDB tables and central lookup table
     * 
     * @param conn connection to the RDB
     */
    @Override
    public void deleteAll(Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {

            // Check if central database lookup table exists
            if (checkCentralTableExists(conn)) {

                // Retrieve all time series table names from central lookup table
                Table<?> dbTable = getDSLTable(DB_TABLE_NAME);
                List<String> queryResult = context.selectDistinct(TABLENAME_COLUMN).from(dbTable)
                        .fetch(TABLENAME_COLUMN);

                if (!queryResult.isEmpty()) {
                    for (String table : queryResult) {
                        // Delete time series RDB table
                        context.dropTable(getDSLTable(table)).execute();
                    }
                    // Delete central lookup table
                    context.dropTable(dbTable).execute();
                }
            }

        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(SQL_ERROR, e);
        }

    }

    /**
     * Initialise central database lookup table
     * 
     * @param context
     *                <p>
     *                Requires existing RDB connection
     */
    private void initCentralTable(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        // Initialise central lookup table: only creates empty table if it does not
        // exist, otherwise it is left unchanged
        try (CreateTableColumnStep createStep = context.createTableIfNotExists(getDSLTable(DB_TABLE_NAME))) {
            createStep.column(DATA_IRI_COLUMN).column(TS_IRI_COLUMN)
                    .column(TABLENAME_COLUMN).column(COLUMNNAME_COLUMN).execute();
        }
        context.createIndex().on(getDSLTable(DB_TABLE_NAME),
                Arrays.asList(DATA_IRI_COLUMN, TS_IRI_COLUMN, TABLENAME_COLUMN, COLUMNNAME_COLUMN)).execute();
    }

    private boolean checkCentralTableExists(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        Table<Record> tables = DSL.table(DSL.name("information_schema", "tables"));
        Field<String> tableNameColumn = DSL.field("table_name", String.class);

        Condition condition = tableNameColumn.eq(DB_TABLE_NAME);
        if (schema != null) {
            Field<String> schemaColumn = DSL.field("table_schema", String.class);
            condition = condition.and(schemaColumn.eq(schema));
        }
        return context.select(count()).from(tables).where(condition).fetchOne(0, int.class) == 1;
    }

    /**
     * Add new entries to central RDB lookup table
     * <p>
     * Requires existing RDB connection
     * 
     * @param tsTable         name of the timeseries table provided as string
     * @param dataIRI         list of data IRIs provided as string
     * @param dataColumnNames list of column names in the tsTable corresponding to
     *                        the data IRIs
     * @param tsIRI           timeseries IRI provided as string
     * @param context
     */
    private void populateCentralTable(String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames,
            String tsIRI, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        InsertValuesStep4<Record, String, String, String, String> insertValueStep = context.insertInto(
                getDSLTable(DB_TABLE_NAME),
                DATA_IRI_COLUMN, TS_IRI_COLUMN, TABLENAME_COLUMN, COLUMNNAME_COLUMN);

        // Populate columns row by row
        for (String s : dataIRI) {
            insertValueStep = insertValueStep.values(s, tsIRI, tsTable, dataColumnNames.get(s));
        }

        insertValueStep.execute();
    }

    /**
     * Add new entries to central RDB lookup table for multiple time series
     * <p>
     * Requires existing RDB connection
     * 
     * @param tsTables            name of the timeseries table provided as list of
     *                            string
     * @param dataIRIs            list of list of data IRIs provided as string
     * @param dataColumnNamesMaps list of maps of column names in the tsTable
     *                            corresponding to the data IRIs
     * @param tsIRIs              timeseries IRI provided as list of string
     * @param conn
     */
    private void bulkPopulateCentralTable(List<String> tsTables, List<List<String>> dataIRIs,
            List<Map<String, String>> dataColumnNamesMaps,
            List<String> tsIRIs, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        InsertValuesStep4<Record, String, String, String, String> insertValueStep = context.insertInto(
                getDSLTable(DB_TABLE_NAME),
                DATA_IRI_COLUMN, TS_IRI_COLUMN, TABLENAME_COLUMN, COLUMNNAME_COLUMN);

        // Populate columns row by row
        for (int i = 0; i < tsTables.size(); i++) {
            String tsTable = tsTables.get(i);
            String tsIRI = tsIRIs.get(i);
            List<String> dataIRI = dataIRIs.get(i);
            Map<String, String> dataColumnNames = dataColumnNamesMaps.get(i);
            for (String s : dataIRI) {
                insertValueStep = insertValueStep.values(s, tsIRI, tsTable, dataColumnNames.get(s));
            }
        }

        insertValueStep.execute();
    }

    /**
     * Create an empty RDB table with the given data types for the respective
     * columns
     * <p>
     * Requires existing RDB connection
     * 
     * @param tsTable         name of the timeseries table provided as string
     * @param dataColumnNames list of column names in the tsTable corresponding to
     *                        the data IRIs
     * @param dataIRI         list of data IRIs provided as string
     * @param dataClass       list with the corresponding Java class (typical
     *                        String, double or int) for each data IRI
     * @param srid
     * @param conn            connection to the RDB
     * @throws SQLException
     */
    private void createEmptyTimeSeriesTable(String tsTable, Map<String, String> dataColumnNames, List<String> dataIRI,
            List<Class<?>> dataClass, Integer srid, Connection conn) throws SQLException {

        DSLContext context = DSL.using(conn, DIALECT);

        List<String> additionalGeomColumns = new ArrayList<>();
        List<Class<?>> classForAdditionalGeomColumns = new ArrayList<>();
        CreateTableColumnStep createStep = null;
        try {
            // Create table
            createStep = context.createTableIfNotExists(getDSLTable(tsTable));

            // Create time column
            createStep = createStep.column(timeColumn);

            // Create 1 column for each value
            for (int i = 0; i < dataIRI.size(); i++) {
                if (Geometry.class.isAssignableFrom(dataClass.get(i))) {
                    // these columns will be added with their respective restrictions
                    additionalGeomColumns.add(dataColumnNames.get(dataIRI.get(i)));
                    classForAdditionalGeomColumns.add(dataClass.get(i));
                } else {
                    createStep = createStep.column(dataColumnNames.get(dataIRI.get(i)),
                            DefaultDataType.getDataType(DIALECT, dataClass.get(i)));
                }
            }

            // Send consolidated request to RDB
            createStep.execute();
        } finally {
            if (createStep != null) {
                createStep.close();
            }
        }

        // create index on time column for quicker searches
        context.createUniqueIndex().on(getDSLTable(tsTable), timeColumn).execute();

        // add remaining geometry columns with restrictions
        if (!additionalGeomColumns.isEmpty()) {
            addGeometryColumns(tsTable, additionalGeomColumns, classForAdditionalGeomColumns, srid, conn);
        }
    }

    /**
     * workaround because open source jOOQ DataType class does not support geometry
     * datatypes properly
     * rather than creating a generic geometry column, this will restrict the column
     * to the class provided
     * e.g. Polygon, Point, Multipolygon, along with the srid, if provided
     * 
     * @throws SQLException
     */
    private void addGeometryColumns(String tsTable, List<String> columnNames, List<Class<?>> dataTypes, Integer srid,
            Connection conn) throws SQLException {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("alter table %s ", getDSLTable(tsTable).toString()));

        for (int i = 0; i < columnNames.size(); i++) {
            sb.append(String.format("add %s geometry(%s", columnNames.get(i), dataTypes.get(i).getSimpleName()));

            // add srid if given
            if (srid != null) {
                sb.append(String.format(", %d)", srid));
            } else {
                sb.append(")");
            }

            if (i != columnNames.size() - 1) {
                sb.append(", ");
            } else {
                sb.append(";");
            }
        }

        String sql = sb.toString();
        try (PreparedStatement statement = conn.prepareStatement(sql)) {
            statement.executeUpdate();
        }
    }

    /**
     * Create an empty RDB table with the given data types for the respective
     * columns
     * <p>
     * Requires existing RDB connection
     * 
     * @param tsTables            name of the timeseries table provided as list of
     *                            string
     * @param dataColumnNamesMaps list of maps of column names in the tsTable
     *                            corresponding to the data IRIs
     * @param dataIRIs            list of list of data IRIs provided as string
     * @param dataClassrd         list of list with the corresponding Java class
     *                            (typical
     *                            String, double or int) for each data IRI
     * @param srid
     * @param conn                connection to the RDB
     */
    private List<Integer> bulkCreateEmptyTimeSeriesTable(List<String> tsTables,
            List<Map<String, String>> dataColumnNamesMaps,
            List<List<String>> dataIRIs,
            List<List<Class<?>>> dataClasses, Integer srid, Connection conn) {

        DSLContext context = DSL.using(conn, DIALECT);
        List<Integer> failedIndex = new ArrayList<>();

        // Initialise RDB table for storing time series data

        List<Query> allSteps = new ArrayList<>();
        Map<String, List<String>> geomColumnsMap = new HashMap<>();
        Map<String, List<Class<?>>> geomColumnsClassMap = new HashMap<>();

        for (int i = 0; i < dataIRIs.size(); i++) {

            List<String> dataIRI = dataIRIs.get(i);
            List<Class<?>> dataClass = dataClasses.get(i);
            String tsTable = tsTables.get(i);
            Map<String, String> dataColumnNames = dataColumnNamesMaps.get(i);

            List<String> additionalGeomColumns = new ArrayList<>();
            List<Class<?>> classForAdditionalGeomColumns = new ArrayList<>();
            // Create table
            CreateTableColumnStep createStep = context.createTableIfNotExists(getDSLTable(tsTable));

            // Create time column
            createStep = createStep.column(timeColumn);

            // Create 1 column for each value
            for (int j = 0; j < dataIRI.size(); j++) {
                if (Geometry.class.isAssignableFrom(dataClass.get(j))) {
                    // these columns will be added with their respective restrictions
                    additionalGeomColumns.add(dataColumnNames.get(dataIRI.get(j)));
                    classForAdditionalGeomColumns.add(dataClass.get(j));
                } else {
                    createStep = createStep.column(dataColumnNames.get(dataIRI.get(j)),
                            DefaultDataType.getDataType(DIALECT, dataClass.get(j)));
                }
            }

            allSteps.add(createStep);
            // create index on time column for quicker searches
            allSteps.add(context.createUniqueIndex().on(getDSLTable(tsTable), timeColumn));

            // geometry columns need to be handled separately
            if (!additionalGeomColumns.isEmpty()) {
                geomColumnsMap.put(tsTable, additionalGeomColumns);
                geomColumnsClassMap.put(tsTable, classForAdditionalGeomColumns);

            }

        }

        context.batch(allSteps).execute();

        // add remaining geometry columns with restrictions
        // TODO - re-factor this to be more efficient
        for (String tsTable : geomColumnsMap.keySet()) {
            List<String> additionalGeomColumns = geomColumnsMap.get(tsTable);
            List<Class<?>> classForAdditionalGeomColumns = geomColumnsClassMap.get(tsTable);
            try {
                addGeometryColumns(tsTable, additionalGeomColumns, classForAdditionalGeomColumns, srid, conn);
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
                // Assume all data IRIs have failed at the moment
                return IntStream.range(0, dataIRIs.size()).boxed().collect(Collectors.toList());
            }
        }

        return failedIndex;

    }

    /**
     * Return SQL commands for appending time series data from TimeSeries object to
     * (existing) RDB table
     * <p>
     * Requires existing RDB connection
     * 
     * @param tsTable         name of the timeseries table provided as string
     * @param ts              time series to write into the table
     * @param dataIRIs        list of strings of data IRIs
     * @param dataColumnNames list of column names in the tsTable corresponding to
     *                        the data in the ts
     * @param conn            connection to the RDB
     */
    private String populateTimeSeriesTable(String tsTable, TimeSeries<T> ts, List<String> dataIRIs,
            Map<String, String> dataColumnNames, Connection conn) {

        DSLContext context = DSL.using(conn, DIALECT);

        // Retrieve RDB table from table name
        Table<?> table = getDSLTable(tsTable);

        List<Field<?>> columnList = new ArrayList<>();
        // Retrieve list of corresponding column names for dataIRIs
        columnList.add(timeColumn);
        for (String data : dataIRIs) {
            columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
        }

        // collect the list of time values that exist in the table
        // these rows are treated specially to avoid duplicates
        // Populate columns row by row

        InsertValuesStepN<?> insertValueStep = context.insertInto(table, columnList);

        List<Object[]> listNewValues = new ArrayList<>();

        List<T> tsTime = ts.getTimes();
        List<List<?>> tsValues = new ArrayList<>();
        int numDataIRI = dataIRIs.size();

        for (int j = 0; j < numDataIRI; j++) {
            tsValues.add(ts.getValues(dataIRIs.get(j)));
        }

        for (int i = 0; i < tsTime.size(); i++) {
            Object[] newValues = new Object[numDataIRI + 1];
            newValues[0] = tsTime.get(i);
            for (int j = 0; j < numDataIRI; j++) {
                newValues[j + 1] = tsValues.get(j).get(i);
            }
            listNewValues.add(newValues);
        }

        for (int i = 0; i < tsTime.size(); i++) {
            // newValues is the row elements
            insertValueStep = insertValueStep.values(listNewValues.get(i));
        }

        // Manually modify the query to upsert
        // TODO: use jOOQ 3.18 or above which support upsert (requires Java 17 or above)
        StringBuilder insertStatement = new StringBuilder(insertValueStep.toString());

        insertStatement.append(System.lineSeparator())
                .append("on conflict (\"")
                .append(timeColumn.getName())
                .append("\")")
                .append(System.lineSeparator())
                .append("do update")
                .append(System.lineSeparator())
                .append("set");

        for (String data : dataIRIs) {
            insertStatement.append(System.lineSeparator())
                    .append("\"")
                    .append(dataColumnNames.get(data))
                    .append("\" = EXCLUDED.\"")
                    .append(dataColumnNames.get(data))
                    .append("\",");
        }

        // Remove the last comma
        insertStatement.setLength(insertStatement.length() - 1);

        return insertStatement.toString();

    }

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
    @Override
    public boolean checkDataHasTimeSeries(String dataIRI, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        if (!checkCentralTableExists(conn)) {
            return false;
        }

        // Look for the entry dataIRI in dbTable
        Table<?> table = getDSLTable(DB_TABLE_NAME);
        return context.fetchExists(selectFrom(table).where(DATA_IRI_COLUMN.eq(dataIRI)));
    }

    @Override
    public boolean checkDataHasTimeSeries(String dataIRI) {
        try (Connection conn = getConnection()) {
            return checkDataHasTimeSeries(dataIRI, conn);
        } catch (SQLException e) {
            throw new JPSRuntimeException(String.format("Error making connection to %s", rdbURL), e);
        }
    }

    /**
     * Check if all given data IRI is attached to a time series in kb
     * 
     * @param dataIRIs data IRIs provided as list of string
     * @param conn     connection to the RDB
     * @return True if any dataIRIs exist and are attached to a time series, false
     *         otherwise
     */
    @Override
    public String checkAnyDataHasTimeSeries(List<String> dataIRIs, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        Table<?> table = getDSLTable(DB_TABLE_NAME);

        List<Condition> conditions = dataIRIs.stream().map(DATA_IRI_COLUMN::eq).collect(Collectors.toList());

        Condition combinedCondition = DSL.or(conditions);

        Result<Record> result = context.select().from(table).where(combinedCondition).fetch();

        // Check if the result is non-empty and return the first element
        if (!result.isEmpty()) {
            return result.get(0).getValue(DATA_IRI_COLUMN.getName(), String.class);
        }

        return null; // If no data IRI exists
    }

    /**
     * Ensure that all dataIRIs are associated with same RDB table (i.e. have same
     * time series IRI)
     * <br>
     * Throws JPSRuntime Exception if not all dataIRIs are attached to same table in
     * the database
     * <p>
     * Requires existing RDB connection;
     * 
     * @param dataIRI list of data IRIs provided as string
     * @param context
     */
    private void checkDataIsInSameTable(List<String> dataIRI, DSLContext context) {

        Table<?> table = getDSLTable(DB_TABLE_NAME);

        List<Condition> conditions = dataIRI.stream().map(DATA_IRI_COLUMN::eq).collect(Collectors.toList());

        Condition combinedCondition = DSL.or(conditions);

        List<String> queryResult = context.select(TS_IRI_COLUMN).from(table).where(combinedCondition)
                .fetch(TS_IRI_COLUMN);

        boolean allIdentical = queryResult.stream().allMatch(s -> s.equals(queryResult.get(0)));

        if (!allIdentical) {
            throw new JPSRuntimeException(exceptionPrefix + "Provided data is not within the same RDB table");
        }
    }

    /**
     * Retrieve tsIRI for provided dataIRI from central database lookup table (if it
     * exists)
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param context
     * @return The attached time series IRI as string
     */
    private String getTimeSeriesIRI(String dataIRI, DSLContext context) {
        try {
            // Look for the entry dataIRI in dbTable
            Table<?> table = getDSLTable(DB_TABLE_NAME);
            List<String> queryResult = context.select(TS_IRI_COLUMN).from(table).where(DATA_IRI_COLUMN.eq(dataIRI))
                    .fetch(TS_IRI_COLUMN);
            // Throws IndexOutOfBoundsException if dataIRI is not present in central lookup
            // table (i.e. queryResult is empty)
            return queryResult.get(0);
        } catch (IndexOutOfBoundsException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(String.format(NO_TS_INST_ERROR, exceptionPrefix, dataIRI));
        }
    }

    /**
     * Retrieve column name for provided dataIRI from central database lookup table
     * (if it exists)
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param context
     * @return Corresponding column name in the RDB table related to the data IRI
     */
    private String getColumnName(String dataIRI, DSLContext context) {
        try {
            // Look for the entry dataIRI in dbTable
            Table<?> table = getDSLTable(DB_TABLE_NAME);
            List<String> queryResult = context.select(COLUMNNAME_COLUMN).from(table).where(DATA_IRI_COLUMN.eq(dataIRI))
                    .fetch(COLUMNNAME_COLUMN);
            // Throws IndexOutOfBoundsException if dataIRI is not present in central lookup
            // table (i.e. queryResult is empty)
            return queryResult.get(0);
        } catch (IndexOutOfBoundsException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(String.format(NO_TS_INST_ERROR, exceptionPrefix, dataIRI));
        }
    }

    /**
     * Retrieve column name for provided dataIRI from central database lookup table
     * (if it exists)
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param context
     * @return Corresponding column name in the RDB table related to the data IRI
     */
    private Map<String, String> bulkGetColumnName(List<String> dataIRIs, DSLContext context) {
        Map<String, String> dataIRIColumnMap = new HashMap<>();
        try {
            // Look for the entry dataIRI in dbTable
            Table<?> table = getDSLTable(DB_TABLE_NAME);
            Result<Record2<String, String>> queryResult = context.select(COLUMNNAME_COLUMN, DATA_IRI_COLUMN)
                    .from(table).where(DATA_IRI_COLUMN.in(dataIRIs)).fetch();
            for (Record result : queryResult) {
                String dataIRI = result.getValue(DATA_IRI_COLUMN);
                String columnValue = result.getValue(COLUMNNAME_COLUMN);
                dataIRIColumnMap.put(dataIRI, columnValue);
            }
            return dataIRIColumnMap;
        } catch (IndexOutOfBoundsException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(
                    exceptionPrefix + "At least 1 data IRI does not have an assigned time series instance");
        }
    }

    /**
     * Retrieve table name for provided dataIRI from central database lookup table
     * (if it exists)
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param context
     * @return Corresponding table name as string
     */
    private String getTimeseriesTableName(String dataIRI, DSLContext context) {
        try {
            // Look for the entry dataIRI in dbTable
            Table<?> table = getDSLTable(DB_TABLE_NAME);
            List<String> queryResult = context.select(TABLENAME_COLUMN).from(table).where(DATA_IRI_COLUMN.eq(dataIRI))
                    .fetch(TABLENAME_COLUMN);
            // Throws IndexOutOfBoundsException if dataIRI is not present in central lookup
            // table (i.e. queryResult is empty)
            return queryResult.get(0);
        } catch (IndexOutOfBoundsException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(String.format(NO_TS_INST_ERROR, exceptionPrefix, dataIRI));
        }
    }

    /**
     * Retrieve time series table for provided dataIRI in database
     * <p>
     * Requires existing RDB connection
     * 
     * @param dataIRI data IRI provided as string
     * @param context
     * @return Table object corresponding to the time series
     */
    private Table<?> getTimeseriesTable(String dataIRI, DSLContext context) {
        // Retrieve the table name attached to the data IRI
        String tableName = getTimeseriesTableName(dataIRI, context);

        return getDSLTable(tableName);
    }

    /**
     * Retrieve aggregate value of a column; stored data should be in numerics
     * 
     * @param dataIRI           data IRI provided as string
     * @param aggregateFunction enumerator for the wanted type of aggregation
     *                          (AVERAGE, MAX, MIN)
     * @param conn              connection to the RDB
     * @return The aggregate value of the whole time series corresponding to the
     *         dataIRI.
     */
    protected double getAggregate(String dataIRI, AggregateFunction aggregateFunction, Connection conn) {

        // Initialise connection and set jOOQ DSL context
        DSLContext context = DSL.using(conn, DIALECT);

        // All database interactions in try-block to ensure closure of connection
        try {
            // Retrieve table corresponding to the time series connected to the data IRI
            Table<?> table = getTimeseriesTable(dataIRI, context);

            // Create map between the data IRI and the corresponding column field in the
            // table
            String columnName = getColumnName(dataIRI, context);
            Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);

            switch (aggregateFunction) {
                case AVERAGE:
                    return context.select(avg(columnField)).from(table).fetch(avg(columnField)).get(0).doubleValue();
                case MAX:
                    return context.select(max(columnField)).from(table).fetch(max(columnField)).get(0);
                case MIN:
                    return context.select(min(columnField)).from(table).fetch(min(columnField)).get(0);
                default:
                    throw new JPSRuntimeException(
                            exceptionPrefix + "Aggregate function " + aggregateFunction.name() + " not valid!");
            }

        } catch (JPSRuntimeException e) {
            // Re-throw JPSRuntimeExceptions
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            // Throw all exceptions incurred by jooq (i.e. by SQL interactions with
            // database) as JPSRuntimeException with respective message
            throw new JPSRuntimeException(exceptionPrefix + SQL_ERROR, e);
        }

    }

    /**
     * Get and set methods for private relational database properties (e.g.
     * PostgreSQL)
     */
    public void setRdbURL(String rdbURL) {
        this.rdbURL = rdbURL;
    }

    public String getRdbURL() {
        return rdbURL;
    }

    public void setRdbUser(String user) {
        this.rdbUser = user;
    }

    public String getRdbUser() {
        return rdbUser;
    }

    public void setRdbPassword(String password) {
        this.rdbPassword = password;
    }

    /**
     * Initialise RDB table for particular time series and add respective entries to
     * central lookup table
     * <p>
     * For the list of supported classes, refer org.jooq.impl.SQLDataType
     * <p>
     * The timeseries IRI needs to be provided. A unique uuid for the corresponding
     * table will be generated.
     * 
     * @param dataIRI   list of IRIs for the data provided as string
     * @param dataClass list with the corresponding Java class (typical String,
     *                  double or int) for each data IRI
     * @param tsIRI     IRI of the timeseries provided as string
     */
    @Override
    public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI) {
        initTimeSeriesTable(dataIRI, dataClass, tsIRI, (Integer) null);
    }

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
    @Override
    public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid) {
        try (Connection conn = getConnection()) {
            initTimeSeriesTable(dataIRI, dataClass, tsIRI, srid, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Append time series data to an already existing RDB table
     * If certain columns within the table are not provided, they will be nulls
     * 
     * @param tsList TimeSeries object to add
     */
    @Override
    public void addTimeSeriesData(List<TimeSeries<T>> tsList) {
        try (Connection conn = getConnection()) {
            addTimeSeriesData(tsList, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

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
     * 
     * @param dataIRI    list of data IRIs provided as string
     * @param lowerBound start timestamp from which to retrieve data (null if not
     *                   applicable)
     * @param upperBound end timestamp until which to retrieve data (null if not
     *                   applicable)
     */
    public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            return getTimeSeriesWithinBounds(dataIRI, lowerBound, upperBound, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }

    }

    /**
     * Retrieve entire time series from RDB
     * 
     * @param dataIRI list of data IRIs provided as string
     */
    public TimeSeries<T> getTimeSeries(List<String> dataIRI) {
        return getTimeSeriesWithinBounds(dataIRI, null, null);
    }

    /**
     * returns a TimeSeries object with the latest value of the given IRI
     * 
     * @param dataIRI
     * @param conn    connection to the RDB
     */
    public TimeSeries<T> getLatestData(String dataIRI) {
        try (Connection conn = getConnection()) {
            return getLatestData(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * returns a TimeSeries object with the oldest value of the given IRI
     * 
     * @param dataIRI
     */
    public TimeSeries<T> getOldestData(String dataIRI) {
        try (Connection conn = getConnection()) {
            return getOldestData(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Retrieve average value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The average of the provided data series as double
     */
    public double getAverage(String dataIRI) {
        try (Connection conn = getConnection()) {
            return getAggregate(dataIRI, AggregateFunction.AVERAGE, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Retrieve maximum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The maximum of the provided data series as double
     */
    public double getMaxValue(String dataIRI) {
        try (Connection conn = getConnection()) {
            return getAggregate(dataIRI, AggregateFunction.MAX, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Retrieve minimum value of a column; stored data should be in numerics
     * 
     * @param dataIRI data IRI provided as string
     * @return The minimum of the provided data series as double
     */
    public double getMinValue(String dataIRI) {
        try (Connection conn = getConnection()) {
            return getAggregate(dataIRI, AggregateFunction.MIN, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Retrieve latest (maximum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @return The maximum (latest) timestamp of the provided data series
     */
    public T getMaxTime(String dataIRI) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            return getMaxTime(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Retrieve earliest (minimum) time entry for a given dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @return The minimum (earliest) timestamp of the provided data series
     */
    public T getMinTime(String dataIRI) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            return getMinTime(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

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
    @Override
    public void deleteRows(String dataIRI, T lowerBound, T upperBound) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            deleteRows(dataIRI, lowerBound, upperBound, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Delete individual time series (i.e. data for one dataIRI only)
     * 
     * @param dataIRI data IRI provided as string
     * @param conn    connection to the RDB
     */
    @Override
    public void deleteTimeSeries(String dataIRI) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            deleteTimeSeries(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Delete all time series information related to a dataIRI (i.e. entire RDB
     * table and entries in central table)
     * 
     * @param dataIRI data IRI provided as string
     */
    @Override
    public void deleteEntireTimeSeries(String dataIRI) {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            deleteEntireTimeSeries(dataIRI, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Delete all time series RDB tables and central lookup table
     */
    @Override
    public void deleteAll() {
        // All database interactions in try-block to ensure closure of connection
        try (Connection conn = getConnection()) {
            deleteAll(conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
        }
    }

    /**
     * Adds columns to an existing time series table, set srid to null if no
     * geometry columns are required
     */
    @Override
    public void addColumnsToExistingTimeSeries(List<String> dataIRIs, List<Class<?>> dataClasses, String tsIri,
            Integer srid, Connection conn) {
        DSLContext context = DSL.using(conn);

        // there is already a check on whether time series exists before this method is
        // called
        String tsTableName = context.select(TABLENAME_COLUMN).from(getDSLTable(DB_TABLE_NAME))
                .where(TS_IRI_COLUMN.eq(tsIri)).fetch(TABLENAME_COLUMN).get(0);

        List<String> existingColumns = context.select(COLUMNNAME_COLUMN).from(getDSLTable(DB_TABLE_NAME))
                .where(TS_IRI_COLUMN.eq(tsIri)).fetch(COLUMNNAME_COLUMN);

        int largestNum = 0;
        for (String column : existingColumns) {
            // extract number
            int number = Integer.parseInt(column.split("column")[1]);
            if (number > largestNum) {
                largestNum = number;
            }
        }

        // Assign column name for each dataIRI; name for time column is fixed
        Map<String, String> dataColumnNames = new HashMap<>();
        largestNum++;
        for (String s : dataIRIs) {
            dataColumnNames.put(s, "column" + largestNum);
            largestNum++;
        }

        // Add corresponding entries in central lookup table
        populateCentralTable(tsTableName, dataIRIs, dataColumnNames, tsIri, conn);

        List<String> additionalGeomColumns = new ArrayList<>();
        List<Class<?>> classForAdditionalGeomColumns = new ArrayList<>();

        List<Query> allSteps = new ArrayList<>();
        for (int i = 0; i < dataIRIs.size(); i++) {
            if (Geometry.class.isAssignableFrom(dataClasses.get(i))) {
                // these columns will be added with their respective restrictions
                additionalGeomColumns.add(dataColumnNames.get(dataIRIs.get(i)));
                classForAdditionalGeomColumns.add(dataClasses.get(i));
            } else {
                allSteps.add(context.alterTable(getDSLTable(tsTableName)).add(dataColumnNames.get(dataIRIs.get(i)),
                        DefaultDataType.getDataType(DIALECT, dataClasses.get(i))));
            }
        }

        context.batch(allSteps).execute();

        // add remaining geometry columns with restrictions
        try {
            if (!additionalGeomColumns.isEmpty()) {
                addGeometryColumns(tsTableName, additionalGeomColumns, classForAdditionalGeomColumns, srid, conn);
            }
        } catch (SQLException e) {
            String errmsg = "Failed to add geometry columns";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(errmsg, e);
        }
    }

    @Override
    public boolean timeSeriesExists(String tsIRI, Connection conn) {
        DSLContext context = DSL.using(conn);
        return context.fetchExists(selectFrom(getDSLTable(DB_TABLE_NAME)).where(TS_IRI_COLUMN.eq(tsIRI)));
    }

    @Override
    public Connection getConnection() throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
            return DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        } catch (ClassNotFoundException e) {
            throw new JPSRuntimeException(exceptionPrefix + "driver not found", e);
        }
    }

    private void initSchemaIfNotExists(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        try (CreateSchemaFinalStep createStep = context.createSchemaIfNotExists(DSL.name(schema))) {
            createStep.execute();
        }
    }

    private Table<Record> getDSLTable(String tableName) {
        if (schema != null) {
            return DSL.table(DSL.name(schema, tableName));
        } else {
            return DSL.table(DSL.name(tableName));
        }
    }
}