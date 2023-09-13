package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Properties;
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.Condition;
import org.jooq.CreateSchemaFinalStep;
import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.InsertValuesStep4;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.Row2;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.exception.DataAccessException;
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

public class TimeSeriesRDBClient<T> {
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
	static final SQLDialect DIALECT = SQLDialect.POSTGRES;
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
	}

	public void setSchema(String schema) {
		this.schema = schema;
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
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI,
			Connection conn) {
		return initTimeSeriesTable(dataIRI, dataClass, tsIRI, null, conn);
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
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid,
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
			for (String s : dataIRI) {
				if (checkDataHasTimeSeries(s, conn)) {
					throw new JPSRuntimeException(
							exceptionPrefix + "<" + s + "> already has an assigned time series instance");
				}
			}

			// Ensure that there is a class for each data IRI
			if (dataIRI.size() != dataClass.size()) {
				throw new JPSRuntimeException(
						exceptionPrefix + "Length of dataClass is different from number of data IRIs");
			}

			TimeSeriesDatabaseMetadata timeSeriesDatabaseMetadata = getTimeSeriesDatabaseMetadata(conn);
			String existingSuitableTable = timeSeriesDatabaseMetadata.getExistingSuitableTable(dataClass, srid);

			// Assign column name for each dataIRI; name for time column is fixed
			Map<String, String> dataColumnNames = new HashMap<>();
			if (existingSuitableTable != null) {
				for (int i = 0; i < dataIRI.size(); i++) {
					String existingSuitableColumn = timeSeriesDatabaseMetadata.getExistingSuitableColumn(
							existingSuitableTable, dataClass.get(i), srid);
					dataColumnNames.put(dataIRI.get(i), existingSuitableColumn);
				}
			} else {
				int i = 1;
				for (String s : dataIRI) {
					dataColumnNames.put(s, "column" + i);
					i++;
				}
			}

			// Add corresponding entries in central lookup table
			populateCentralTable(tsTableName, dataIRI, dataColumnNames, tsIRI, conn);

			// Initialise RDB table for storing time series data
			if (existingSuitableTable == null) {
				createEmptyTimeSeriesTable(tsTableName, dataColumnNames, dataIRI, dataClass, srid, conn);
			}

			return tsTableName;
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with
			// database) as JPSRuntimeException with respective message
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}

	}

	/**
	 * Append time series data to an already existing RDB table
	 * If certain columns within the table are not provided, they will be nulls
	 * if a row with the equivalent time value exists, the values provided will
	 * overwrite
	 * the existing data in the table
	 * 
	 * @param ts_list TimeSeries object to add
	 * @param conn    connection to the RDB
	 */
	protected void addTimeSeriesData(List<TimeSeries<T>> tsList, Connection conn) {

		// Initialise connection and set jOOQ DSL context
		DSLContext context = DSL.using(conn, DIALECT);

		// Loop over all time series in list
		for (TimeSeries<T> ts : tsList) {

			List<String> dataIRI = ts.getDataIRIs();

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

				String tsTableName = getTimeseriesTableName(dataIRI.get(0), context);
				String tsIri = getTimeSeriesIRI(dataIRI.get(0), context);
				// Assign column name for each dataIRI; name for time column is fixed
				Map<String, String> dataColumnNames = new HashMap<>();
				for (String s : dataIRI) {
					dataColumnNames.put(s, getColumnName(s, context));
				}

				// Append time series data to time series table
				// if a row with the time value exists, that row will be updated instead of
				// creating a new row
				populateTimeSeriesTable(tsTableName, ts, dataColumnNames, tsIri, conn);
			} catch (JPSRuntimeException e) {
				// Re-throw JPSRuntimeExceptions
				throw e;
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
				// Throw all exceptions incurred by jooq (i.e. by SQL interactions with
				// database) as JPSRuntimeException with respective message
				throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
			}

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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
	protected void deleteRows(String dataIRI, T lowerBound, T upperBound, Connection conn) {

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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}

	}

	/**
	 * Delete individual time series (i.e. data for one dataIRI only)
	 * 
	 * @param dataIRI data IRI provided as string
	 * @param conn    connection to the RDB
	 */
	protected void deleteTimeSeries(String dataIRI, Connection conn) {

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
				deleteTimeSeriesTable(dataIRI, conn);
			}

		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with
			// database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
	protected void deleteTimeSeriesTable(String dataIRI, Connection conn) {

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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}

	}

	/**
	 * Delete all time series RDB tables and central lookup table
	 * 
	 * @param conn connection to the RDB
	 */
	protected void deleteAll(Connection conn) {

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
			throw new JPSRuntimeException("Error while executing SQL command", e);
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
			createStep = createStep.column(timeColumn).column(TS_IRI_COLUMN);

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
			createStep.constraints(unique(timeColumn, TS_IRI_COLUMN)).execute();
		} finally {
			if (createStep != null) {
				createStep.close();
			}
		}

		// create index on time column and time series IRI columns for quicker searches
		context.createIndex().on(getDSLTable(tsTable), timeColumn, TS_IRI_COLUMN).execute();

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
	 * Append time series data from TimeSeries object to (existing) RDB table
	 * <p>
	 * Requires existing RDB connection
	 * 
	 * @param tsTable         name of the timeseries table provided as string
	 * @param ts              time series to write into the table
	 * @param dataColumnNames list of column names in the tsTable corresponding to
	 *                        the data in the ts
	 * @param conn            connection to the RDB
	 * @throws SQLException
	 */
	private void populateTimeSeriesTable(String tsTable, TimeSeries<T> ts, Map<String, String> dataColumnNames,
			String tsIri, Connection conn) throws SQLException {

		DSLContext context = DSL.using(conn, DIALECT);

		List<String> dataIRIs = ts.getDataIRIs();

		// Retrieve RDB table from table name
		Table<?> table = getDSLTable(tsTable);

		List<Field<?>> columnList = new ArrayList<>();
		// Retrieve list of corresponding column names for dataIRIs
		columnList.add(timeColumn);
		for (String data : dataIRIs) {
			columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
		}
		columnList.add(TS_IRI_COLUMN);

		// collect the list of time values that exist in the table
		// these rows are treated specially to avoid duplicates
		// Populate columns row by row
		InsertValuesStepN<?> insertValueStep = context.insertInto(table, columnList);

		for (int i = 0; i < ts.getTimes().size(); i++) {
			// newValues is the row elements
			Object[] newValues = new Object[dataIRIs.size() + 2];
			newValues[0] = ts.getTimes().get(i);
			for (int j = 0; j < ts.getDataIRIs().size(); j++) {
				newValues[j + 1] = (ts.getValues(dataIRIs.get(j)).get(i));
			}
			newValues[ts.getDataIRIs().size() + 1] = tsIri;
			insertValueStep = insertValueStep.values(newValues);

		}
		// open source jOOQ does not support postgis, hence not using execute() directly
		// if this gets executed when it's 0, null values will be added
		try (PreparedStatement statement = conn.prepareStatement(insertValueStep.toString())) {
			statement.executeUpdate();
		}
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
	boolean checkDataHasTimeSeries(String dataIRI, Connection conn) {
		DSLContext context = DSL.using(conn, DIALECT);
		// Look for the entry dataIRI in dbTable
		Table<?> table = getDSLTable(DB_TABLE_NAME);
		return context.fetchExists(selectFrom(table).where(DATA_IRI_COLUMN.eq(dataIRI)));
	}

	boolean checkDataHasTimeSeries(String dataIRI) {
		try (Connection conn = getConnection()) {
			return checkDataHasTimeSeries(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(String.format("Error making connection to %s", rdbURL), e);
		}
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
		// Get time series IRI of first dataIRI
		String tsIRI = getTimeSeriesIRI(dataIRI.get(0), context);
		// Check that all further dataIRI share this time series IRI
		if (dataIRI.size() > 1) {
			for (int i = 1; i < dataIRI.size(); i++) {
				String curTsIRI = getTimeSeriesIRI(dataIRI.get(i), context);
				if (!curTsIRI.contentEquals(tsIRI)) {
					throw new JPSRuntimeException(exceptionPrefix + "Provided data is not within the same RDB table");
				}
			}
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
			throw new JPSRuntimeException(
					exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance");
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
			throw new JPSRuntimeException(
					exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance");
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
			throw new JPSRuntimeException(
					exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance");
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
	 * check if a row exists to prevent duplicate rows with the same time value
	 * 
	 * @param tsTableName
	 * @param time
	 * @param context
	 * @return
	 */
	private boolean checkTimeRowExists(String tsTableName, T time, DSLContext context) {
		try {
			return context.fetchExists(selectFrom(getDSLTable(tsTableName)).where(timeColumn.eq(time)));
		} catch (DataAccessException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error in checking if a row exists for a given time value");
		}
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
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
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
	 * Load RDB URL, username and password from properties file
	 * ("timeseries.properties") at specified path
	 * 
	 * @param filepath absolute path to timeseries properties file
	 */
	protected void loadRdbConfigs(String filepath) throws IOException {

		// Check whether properties file exists at specified location
		File file = new File(filepath);
		if (!file.exists()) {
			throw new JPSRuntimeException(
					exceptionPrefix + "No properties file found at specified filepath: " + filepath);
		}

		// Try-with-resource to ensure closure of input stream
		try (InputStream input = new FileInputStream(file)) {

			// Load properties file from specified path
			Properties prop = new Properties();
			prop.load(input);

			// Get the property values and assign
			if (prop.containsKey("db.url")) {
				setRdbURL(prop.getProperty("db.url"));
			} else {
				throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"db.url=<rdb_url>\" ");
			}
			if (prop.containsKey("db.user")) {
				setRdbUser(prop.getProperty("db.user"));
			} else {
				throw new JPSRuntimeException(
						exceptionPrefix + "Properties file is missing \"db.user=<rdb_username>\" ");
			}
			if (prop.containsKey("db.password")) {
				setRdbPassword(prop.getProperty("db.password"));
			} else {
				throw new JPSRuntimeException(
						exceptionPrefix + "Properties file is missing \"db.password=<rdb_password>\" ");
			}
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
	 */
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI) {
		return initTimeSeriesTable(dataIRI, dataClass, tsIRI, (Integer) null);
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
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid) {
		try (Connection conn = getConnection()) {
			return initTimeSeriesTable(dataIRI, dataClass, tsIRI, srid, conn);
		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Append time series data to an already existing RDB table
	 * If certain columns within the table are not provided, they will be nulls
	 * if a row with the equivalent time value exists, the values provided will
	 * overwrite
	 * the existing data in the table
	 * 
	 * @param ts_list TimeSeries object to add
	 */
	protected void addTimeSeriesData(List<TimeSeries<T>> tsList) {
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
	 * Returned time series contain potential duplicates (i.e. multiple entries for
	 * same time stamp)
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
	protected void deleteRows(String dataIRI, T lowerBound, T upperBound) {
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
	protected void deleteTimeSeries(String dataIRI) {
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
	protected void deleteTimeSeriesTable(String dataIRI) {
		// All database interactions in try-block to ensure closure of connection
		try (Connection conn = getConnection()) {
			deleteTimeSeriesTable(dataIRI, conn);
		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Delete all time series RDB tables and central lookup table
	 */
	protected void deleteAll() {
		// All database interactions in try-block to ensure closure of connection
		try (Connection conn = getConnection()) {
			deleteAll(conn);
		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	protected Connection getConnection() throws SQLException {
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

	TimeSeriesDatabaseMetadata getTimeSeriesDatabaseMetadata(Connection conn) {
		TimeSeriesDatabaseMetadata timeSeriesDatabaseMetadata = new TimeSeriesDatabaseMetadata();
		addDataTypes(conn, timeSeriesDatabaseMetadata);
		addSpecificGeometryClass(conn, timeSeriesDatabaseMetadata);
		return timeSeriesDatabaseMetadata;
	}

	void addDataTypes(Connection conn, TimeSeriesDatabaseMetadata timeSeriesDatabaseMetadata) {
		DSLContext context = DSL.using(conn, DIALECT);
		Table<Record> columnsTable = DSL.table(DSL.name("information_schema", "columns"));
		Field<String> tableNameColumn = DSL.field("table_name", String.class);
		Field<String> dataTypeColumn = DSL.field("data_type", String.class);
		Field<String> udtNameColumn = DSL.field("udt_name", String.class);
		Field<String> columnNameColumn = DSL.field("column_name", String.class);

		Condition condition = tableNameColumn
				.in(context.select(TABLENAME_COLUMN).from(getDSLTable(DB_TABLE_NAME)).fetch(TABLENAME_COLUMN))
				.and(columnNameColumn.ne("time"));

		if (schema != null) {
			Field<String> schemaColumn = DSL.field("table_schema", String.class);
			condition.and(schemaColumn.eq(schema));
		}

		Result<? extends Record> queryResult = context
				.select(tableNameColumn, dataTypeColumn, udtNameColumn, columnNameColumn).from(columnsTable)
				.where(condition).fetch();

		queryResult.forEach(rec -> {
			String tableName = rec.get(tableNameColumn);
			String dataType = rec.get(dataTypeColumn);
			String udtType = rec.get(udtNameColumn);
			String columnName = rec.get(columnNameColumn);
			timeSeriesDatabaseMetadata.addDataType(tableName, columnName, dataType);
			timeSeriesDatabaseMetadata.addUdtType(tableName, columnName, udtType);
		});
	}

	void addSpecificGeometryClass(Connection conn, TimeSeriesDatabaseMetadata timeSeriesMetadata) {
		List<List<String>> geometryRows = timeSeriesMetadata.getGeometryRows();
		if (!geometryRows.isEmpty()) {
			DSLContext context = DSL.using(conn, DIALECT);
			Field<String> tableNameColumn = DSL.field("f_table_name", String.class);
			Field<String> columnNameColumn = DSL.field("f_geometry_column", String.class);
			Field<String> typeColumn = DSL.field("type", String.class);
			Field<Integer> sridColumn = DSL.field("srid", Integer.class);

			List<Row2<String, String>> rows = new ArrayList<>();
			geometryRows.forEach(geometryRow -> rows.add(row(geometryRow.get(0), geometryRow.get(1))));

			Condition condition = row(tableNameColumn, columnNameColumn).in(rows);

			Result<? extends Record> queryResult = context
					.select(sridColumn, typeColumn, tableNameColumn, columnNameColumn)
					.from(getDSLTable("geometry_columns")).where(condition).fetch();

			queryResult.forEach(rec -> {
				String tableName = rec.get(tableNameColumn);
				String columnName = rec.get(columnNameColumn);
				String geometryName = rec.get(typeColumn);
				int srid = rec.get(sridColumn);

				timeSeriesMetadata.addGeometryType(tableName, columnName, geometryName);
				timeSeriesMetadata.addSrid(tableName, columnName, srid);
			});
		}
	}
}