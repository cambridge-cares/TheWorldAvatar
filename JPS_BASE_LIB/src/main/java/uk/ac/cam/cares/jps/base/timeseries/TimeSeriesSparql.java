package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.literalOf;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.regex.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.*;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

/**
 * This class contains a collection of methods to interact with a triple store/knowledge base (kb).
 * This class should not be used directly. It should only be accessed internally via {@link TimeSeriesClient TimeSeriesClient}
 * @author Kok Foong Lee
 */

public class TimeSeriesSparql {
	// kbClient with the endpoint (triplestore/owl file) specified
	private TripleStoreClientInterface kbClient;

	// Namespaces for ontology and knowledge base
	public static final String ns_ontology = "https://www.theworldavatar.com/kg/ontotimeseries/";
	public static final String ns_kb = "https://www.theworldavatar.com/kg/ontotimeseries/";
	public static final String ns_time = "http://www.w3.org/2006/time#";

	// Prefixes
	private static final Prefix prefix_ontology = SparqlBuilder.prefix("ts", iri(ns_ontology));
	private static final Prefix prefix_kb = SparqlBuilder.prefix("kb", iri(ns_kb));
	private static final Prefix prefix_time = SparqlBuilder.prefix("time", iri(ns_time));

	// RDF type
	public static final Iri TimeSeries = prefix_ontology.iri("TimeSeries");
	public static final Iri StepwiseCumulativeTimeSeries = prefix_ontology.iri("StepwiseCumulativeTimeSeries");
	public static final Iri CumulativeTotalTimeSeries = prefix_ontology.iri("CumulativeTotalTimeSeries");
	public static final Iri AverageTimeSeries = prefix_ontology.iri("AverageTimeSeries");
	public static final Iri InstantaneousTimeSeries = prefix_ontology.iri("InstantaneousTimeSeries");
	private static final Iri Duration = prefix_time.iri("Duration");

	// Relationships
	private static final Iri hasTimeSeries = prefix_ontology.iri("hasTimeSeries");
	private static final Iri hasRDB = prefix_ontology.iri("hasRDB");
	private static final Iri hasTimeUnit = prefix_ontology.iri("hasTimeUnit");
	private static final Iri hasAveragingPeriod = prefix_ontology.iri("hasAveragingPeriod");
	private static final Iri numericDuration = prefix_time.iri("numericDuration");
	private static final Iri unitType = prefix_time.iri("unitType");

	// Fields for class specific exceptions
	private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";

	//Time Series Type
	public static final String AverageTypeString = ns_ontology+"AverageTimeSeries";
	public static final String StepwiseCumulativeTypeString = ns_ontology+"StepwiseCumulativeTimeSeries";
	public static final String CumulativeTotalTypeString = ns_ontology+"CumulativeTotalTimeSeries";
	public static final String InstantaneousTypeString = ns_ontology+"InstantaneousTimeSeries";
	public static final String TimeSeriesTypeString = ns_ontology+"TimeSeries";

	//Timeseries IRI prefix
	public static final String AverageIRIString = ns_ontology + "AverageTimeseries";
	public static final String StepwiseCumulativeIRIString = ns_ontology + "StepwiseCumulativeTimeseries";
	public static final String CumulativeTotalIRIString = ns_ontology + "CumulativeTotalTimeseries";
	public static final String InstantaneousIRIString = ns_ontology + "InstantaneousTimeseries";
	public static final String TimeSeriesIRIString = ns_ontology+"Timeseries";


	//Values Pattern for TimeSeries types
	private static final List<RdfObject> types = Arrays.asList(TimeSeries, CumulativeTotalTimeSeries, StepwiseCumulativeTimeSeries, AverageTimeSeries, InstantaneousTimeSeries);
	private static final Variable tsType = Queries.SELECT().var();
	private static final ValuesPattern vp = new ValuesPattern(tsType, types);

	private final ArrayList<String> temporalUnitType = new ArrayList<>(Arrays.asList("unitSecond", "unitMinute", "unitHour", "unitDay", "unitWeek", "unitMonth", "unitYear"));

	//EnumMap of allowed temporalUnit types
	private static EnumMap<ChronoUnit, String> temporalUnitMap = new EnumMap<>(ChronoUnit.class);
	static {
		temporalUnitMap.put(ChronoUnit.SECONDS, ns_time+"unitSecond");
		temporalUnitMap.put(ChronoUnit.MINUTES, ns_time+"unitMinute");
		temporalUnitMap.put(ChronoUnit.HOURS, ns_time+"unitHour");
		temporalUnitMap.put(ChronoUnit.DAYS, ns_time+"unitDay");
		temporalUnitMap.put(ChronoUnit.WEEKS, ns_time+"unitWeek");
		temporalUnitMap.put(ChronoUnit.MONTHS, ns_time+"unitMonth");
		temporalUnitMap.put(ChronoUnit.YEARS, ns_time+"unitYear");
	}

	/**
	 * Logger for error output.
	 */
	private static final Logger LOGGER = LogManager.getLogger(TimeSeriesSparql.class);

	//Custom Class to store numeric duration and the corresponding temporal unit pair to be able to map the pair to the averaging period iri
	protected class CustomDuration {

		private final Double value;
		private final String unit;
		private int hashCode;

		public CustomDuration(Double value, String unit) {
			this.value = value;
			this.unit = unit;
			this.hashCode = Objects.hash(value, unit);
		}

		public double getValue() {
			return value;
		}

		public String getUnit() {
			return unit;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			if (o == null || getClass() != o.getClass())
				return false;
			CustomDuration that = (CustomDuration) o;
			return value.equals(that.value) && unit.equals(that.unit);
		}

		@Override
		public int hashCode() {
			return this.hashCode;
		}
	}

	/**
     * Standard constructor
     * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information
     */
    public TimeSeriesSparql(TripleStoreClientInterface kbClient) {
    	this.kbClient = kbClient;
    }

    /**
	 * Setter for the knowledge base client
	 * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information
	*/
	public void setKBClient(TripleStoreClientInterface kbClient) {
        this.kbClient = kbClient;
	}

	/**
	 * Load SPARQL query and update endpoints from properties file ("timeseries.properties") at specified path
	 * @param filepath absolute path to timeseries properties file
	 */
	protected void loadSparqlConfigs(String filepath) throws IOException {

		// Check whether properties file exists at specified location
		File file = new File(filepath);
		if (!file.exists()) {
			throw new JPSRuntimeException(exceptionPrefix + "No properties file found at specified filepath: " + filepath);
		}

		// Try-with-resource to ensure closure of input stream
		try (InputStream input = new FileInputStream(file)) {

			// Load properties file from specified path
	        Properties prop = new Properties();
	        prop.load(input);

	        // Get the property values and assign
	        if (prop.containsKey("sparql.query.endpoint")) {
	        	kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
	        } else {
	        	throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ");
	        }
	        if (prop.containsKey("sparql.update.endpoint")) {
	        	kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
	        } else {
	        	throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ");
	        }
		}

	}

	/////////////////////////////////
//	AVERAGE TIME SERIES UTILITIES
	////////////////////////////////

	/**
	 * Returns the custom duration object containing the numerical duration value and temporal unit
	 * associated with the avergae time series iri.
	 * Returns null if the average time series does not exist or if the given time series is not of average time series type.
	 * @param tsIRI Average time series
	 * @return custom duration with numerical duration and temporal unit
	 */
	protected CustomDuration getCustomDuration(String tsIRI){

		CustomDuration duration = null;
		SelectQuery query = Queries.SELECT();
		Variable value = SparqlBuilder.var("value");
		Variable unit = SparqlBuilder.var("unit");
		Variable dur = SparqlBuilder.var("dur");
		TriplePattern queryPattern1 = iri(tsIRI).has(hasAveragingPeriod, dur);
		TriplePattern queryPattern2 = dur.has(numericDuration, value).andHas(unitType, unit);

		query.select(value, unit).where(queryPattern1, queryPattern2).prefix(prefix_time).prefix(prefix_ontology);

		JSONArray result = kbClient.executeQuery(query.getQueryString());
		if(!result.isEmpty()){
			duration = new CustomDuration(Double.valueOf(result.getJSONObject(0).getString("value")), result.getJSONObject(0).getString("unit"));
		}

		return duration;
	}

	/**
	 * Check if an averaging period with given temporalUnit and numericDuration already exists in the knowledge graph.
	 * Returns null if such an averaging period doesn't exist.
	 * @param temporalUnit unit type of the averaging period
	 * @param numericValue numerical duration of the averaging period
	 * @return Averaging period IRI attached to the given temporalUnit and numericDuration in the knowledge graph
	 */
	public String getDurationIRI(String temporalUnit, Double numericValue) {

		String durationIRI = null;
		String queryString = "periodIRI";

		SelectQuery query = Queries.SELECT();
		Variable periodIRI = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = periodIRI.has(numericDuration, numericValue).andHas(unitType, iri(temporalUnit));

		query.select(periodIRI).where(queryPattern).prefix(prefix_time);

		JSONArray result = kbClient.executeQuery(query.getQueryString());
		if(!result.isEmpty()){
			durationIRI = result.getJSONObject(0).getString(queryString);
		}

		return durationIRI;
	}

	/**
	 * Creates a hashMap containing the (numericDuration, temporalUnit) mapping to averaging period IRI
	 * for all the averaging period Iris in the kb
	 * @return hashMap containing (numericDuration, temporalUnit) mapping to averaging period IRI
	 */
	public Map<CustomDuration, String> createDurationIRIMapping(){

		HashMap<CustomDuration, String> durationMap = new HashMap<>();

		SelectQuery query = Queries.SELECT();
		Variable durIRI = SparqlBuilder.var("durIRI");
		Variable value = SparqlBuilder.var("value");
		Variable unit = SparqlBuilder.var("unit");

		TriplePattern queryPattern = durIRI.has(numericDuration, value).andHas(unitType, unit);
		query.select(durIRI, value, unit).where(queryPattern).prefix(prefix_time);
		JSONArray result = kbClient.executeQuery(query.getQueryString());

		for (int i=0; i < result.length(); i++){
			CustomDuration duration = new CustomDuration(Double.valueOf(result.getJSONObject(i).getString("value")), result.getJSONObject(i).getString("unit"));
			durationMap.put(duration, result.getJSONObject(i).getString("durIRI"));
		}
		return durationMap;

	}

	/**
	 * * Removes average time series and all associated connections from kb (i.e. remove all triples with tsIRI as subject or object)
	 * @param tsIRI
	 */
	protected void removeAverageTimeSeries(String tsIRI){
		// sub query to search for all triples with tsIRI as the subject/object
		SubSelect sub = GraphPatterns.select();
		Variable predicate1 = SparqlBuilder.var("a");
		Variable predicate2 = SparqlBuilder.var("b");
		Variable subject = SparqlBuilder.var("c");
		Variable object1 = SparqlBuilder.var("d");
		Variable predicate3 = SparqlBuilder.var("e");
		Variable object2 = SparqlBuilder.var("f");
		Variable durIRI = SparqlBuilder.var("durIRI");

		TriplePattern deleteTp1 = iri(tsIRI).has(predicate1, object1);
		TriplePattern deleteTp2 = subject.has(predicate2, iri(tsIRI));
		TriplePattern deleteTp3 = iri(tsIRI).has(hasAveragingPeriod, durIRI);
		TriplePattern deleteTp4 = durIRI.has(predicate3, object2);
		sub.select(predicate1, predicate2, predicate3, subject, object1, object2, durIRI).where(deleteTp1, deleteTp2, deleteTp3, deleteTp4);

		// insert subquery into main sparql update
		ModifyQuery modify = Queries.MODIFY();
		modify.delete(deleteTp1, deleteTp2, deleteTp3, deleteTp4).where(sub).prefix(prefix_ontology);

		kbClient.setQuery(modify.getQueryString());
		kbClient.executeUpdate();
	}

	protected String getAveragingPeriod(String tsIRI){

		String averagingPeriodIRI = null;

		SelectQuery query = Queries.SELECT();
		String queryString = "durIRI";
		Variable durIRI = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = iri(tsIRI).has(hasAveragingPeriod, durIRI);

		query.select(durIRI).where(queryPattern).prefix(prefix_ontology);
		kbClient.setQuery(query.getQueryString());
		JSONArray result = kbClient.executeQuery();
		if(!result.isEmpty()){
			averagingPeriodIRI = kbClient.executeQuery().getJSONObject(0).getString(queryString);
		}
		return averagingPeriodIRI;

	}

	/**
	 * Check whether a particular time series (i.e. tsIRI) exists
	 * @param timeSeriesIRI timeseries IRI provided as string
	 * @return True if a time series instance with the tsIRI exists, false otherwise
	 */
    public boolean checkTimeSeriesExists(String timeSeriesIRI) {

		SelectQuery query = Queries.SELECT();
		TriplePattern queryPattern = iri(timeSeriesIRI).isA(tsType);

		query.select(tsType).where(queryPattern, vp).prefix(prefix_ontology);
		kbClient.setQuery(query.getQueryString());

		return (!kbClient.executeQuery(query.getQueryString()).isEmpty());
    }

	/**
	 * Check whether given data IRI is attached to a time series
	 * @param dataIRI data IRI provided as string
	 * @return True if dataIRI exists and is attached to a time series, false otherwise
	 */
    public boolean checkDataHasTimeSeries(String dataIRI) {
    	String query = String.format("ask {<%s> <%s> ?a}", dataIRI, (ns_ontology + "hasTimeSeries"));
    	kbClient.setQuery(query);
    	return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }

	/**
	 * Check whether time series IRI has time unit
	 * @param tsIRI timeseries IRI provided as string
	 * @return True if the timeseries instance exists and has a defined time unit, false otherwise
	 */
    public boolean checkTimeUnitExists(String tsIRI) {
    	String query = String.format("ask {<%s> <%s> ?a}", tsIRI, (ns_ontology + "hasTimeUnit"));
    	kbClient.setQuery(query);
    	return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }

	/**
	 * Instantiate the time series instance in the knowledge base (time unit is optional)
	 * @param timeSeriesIRI timeseries IRI provided as string
	 * @param dataIRI list of data IRI provided as string that should be attached to the timeseries
	 * @param dbURL URL of the database where the timeseries data is stored provided as string
	 * @param timeUnit the time unit of the time series (optional)
	 * @param type type of TimeSeries data to be instantiated.
	 *             Allowed values of type enum: Type.AVERAGE, Type.INSTANTANEOUS, Type.STEPWISECUMULATIVE, Type.CUMULATIVETOTAL
	 * @param duration Required for Average Time Series. Numeric duration of the averaging period for Average TimeSeries of type Duration. Only positive values are allowed. (optional)
	 * @param unit Required for Average Time Series. Temporal unit type of the averaging period for Average TimeSeries. (optional)
	 *             Allowed values of type ChronoUnit:
	 *             ChronoUnit.SECONDS, ChronoUnit.MINUTES, ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS, ChronoUnit.YEARS
	 *
	 */

	protected void initTS(String timeSeriesIRI, List<String> dataIRI, String dbURL, String timeUnit, Iri type, Duration duration, ChronoUnit unit) {
		// Construct time series IRI
		Iri tsIRI;
		// Check whether timeseriesIRI follows IRI naming convention of namespace & local_name
		// If only local name is provided, iri() function would add filepath as default prefix
		// Every legal (full) IRI contains at least one ':' character to separate the scheme from the rest of the IRI
		if (Pattern.compile("\\w+\\S+:\\S+\\w+").matcher(timeSeriesIRI).matches()) {
			tsIRI = iri(timeSeriesIRI);
		} else {
			throw new JPSRuntimeException(exceptionPrefix + "Time series IRI does not have valid IRI format");
		}

		ModifyQuery modify = Queries.MODIFY();
		// set prefix declarations
		modify.prefix(prefix_ontology, prefix_kb, prefix_time);

		// define type
		modify.insert(tsIRI.isA(type));

		if(type.equals(AverageTimeSeries)){

			if(duration.getNano()!=0){
				LOGGER.warn("Nano is ignored");
			}

			//numeric Duration
			if(duration.getSeconds() <=0.0){
				throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
			}

			//Check if the given temporal unit is one of the allowed values.
			// Where, allowed values are of type ChronoUnit.SECONDS, ChronoUnit.MINUTES, ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS, ChronoUnit.YEARS
			if(!temporalUnitMap.containsKey(unit)){
				throw new JPSRuntimeException(exceptionPrefix + "Temporal Unit: " + unit.toString() + " of invalid type");
			}

			Double numericValue = Double.valueOf(duration.getSeconds()/ unit.getDuration().getSeconds());
			String temporalUnit = temporalUnitMap.get(unit);

			//Check if a duration iri with given temporalUnit and numericDuration exists in the knowledge graph.
			//If true, attach the Average TimeSeries to the existing duration IRI. Otherwise, create a new duration IRI.
			String durationIRI = getDurationIRI(temporalUnit, numericValue);

			if(durationIRI!=null){
				modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
			}
			else {
				//Duration IRI
				durationIRI = ns_kb + "AveragingPeriod_" + UUID.randomUUID();
				modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
				modify.insert(iri(durationIRI).isA(Duration));
				modify.insert(iri(durationIRI).has(unitType, iri(temporalUnit)));
				modify.insert(iri(durationIRI).has(numericDuration, numericValue));
			}
		}

		// Check that the data IRIs are not attached to a different time series IRI already
		for (String iri: dataIRI) {
			String ts = getTimeSeries(iri);
			if(ts != null) {
				throw new JPSRuntimeException(exceptionPrefix + "The data IRI " + iri + " is already attached to time series " + ts);
			}
		}

		// relational database URL
		modify.insert(tsIRI.has(hasRDB, literalOf(dbURL)));

		// link each data to time series
		for (String data : dataIRI) {
			TriplePattern ts_tp = iri(data).has(hasTimeSeries, tsIRI);
			modify.insert(ts_tp);
		}

		// optional: define time unit
		if (timeUnit != null) {
			modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeUnit)));
			//modify.insert(tsIRI.has(hasTimeUnit, iri(timeUnit)));
		}

		kbClient.executeUpdate(modify.getQueryString());
	}

	protected void bulkInitTS(List<String> tsIRIs, List<List<String>> dataIRIs, String rdbURL, List<String> timeUnit, List<Iri> types, List<Duration> durations, List<ChronoUnit> units) {
		ModifyQuery modify = Queries.MODIFY();
		// set prefix declarations
		modify.prefix(prefix_ontology, prefix_kb, prefix_time);

		Map<CustomDuration, String> durationMap = createDurationIRIMapping();

		for (int i = 0; i < tsIRIs.size(); i++) {
			Iri tsIRI;
			if (Pattern.compile("\\w+\\S+:\\S+\\w+").matcher(tsIRIs.get(i)).matches()) {
				tsIRI = iri(tsIRIs.get(i));
			} else {
				throw new JPSRuntimeException(exceptionPrefix + "Time series IRI does not have valid IRI format");
			}

			modify.insert(tsIRI.isA(types.get(i)));

			if(types.get(i).equals(AverageTimeSeries)){

				if(durations.get(i).getNano()!=0){
					LOGGER.warn("Nano is ignored");
				}

				//numeric Duration
				if(durations.get(i).getSeconds() <=0.0){
					throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
				}

				if(!temporalUnitMap.containsKey(units.get(i))){
					throw new JPSRuntimeException(exceptionPrefix + "Temporal Unit: " + units.get(i).toString() + " of invalid type");
				}

				Double numericValue = Double.valueOf(durations.get(i).getSeconds()/ units.get(i).getDuration().getSeconds());
				String temporalUnit = temporalUnitMap.get(units.get(i));

				CustomDuration key = new CustomDuration(numericValue, temporalUnit);

				//Check if a duration iri with given temporalUnit and numericDuration exists in the knowledge graph.
				//If true, attach the Average TimeSeries to the existing duration IRI. Otherwise, create a new duration IRI.
				String durationIRI = null;

				if (durationMap.containsKey(key)){
					durationIRI = durationMap.get(key);
				}

				if(durationIRI!=null){
					modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
				}
				else {
					//Duration IRI
					durationIRI = ns_kb + "AveragingPeriod_" + UUID.randomUUID();
					modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
					modify.insert(iri(durationIRI).isA(Duration));
					modify.insert(iri(durationIRI).has(unitType, iri(temporalUnit)));
					modify.insert(iri(durationIRI).has(numericDuration, numericValue));
				}
			}

			// Check that the data IRIs are not attached to a different time series IRI already
			for (String iri: dataIRIs.get(i)) {
				String ts = getTimeSeries(iri);
				if(ts!=null) {
					throw new JPSRuntimeException(exceptionPrefix + "The data IRI " + iri + " is already attached to time series " + ts);
				}
			}

			// relational database URL
			modify.insert(tsIRI.has(hasRDB, literalOf(rdbURL)));

			// link each data to time series
			for (String data : dataIRIs.get(i)) {
				TriplePattern tsTp = iri(data).has(hasTimeSeries, tsIRI);
				modify.insert(tsTp);
			}

			// optional: define time unit
			if (timeUnit.get(i) != null) {
				modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeUnit.get(i))));
				//modify.insert(tsIRI.has(hasTimeUnit, iri(timeUnit)));
			}

		}
		kbClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Count number of time series IRIs in kb
	 * @return Total number of time series instances in the knowledge base as int
	 */
	public int countTS() {
		SelectQuery query = Queries.SELECT();
    	String queryKey = "numtimeseries";

    	Variable ts = SparqlBuilder.var("ts");
    	Variable numtimeseries = SparqlBuilder.var(queryKey);

    	GraphPattern querypattern = ts.isA(tsType);
    	Assignment count = Expressions.count(ts).as(numtimeseries);

    	query.select(count).where(querypattern, vp).prefix(prefix_ontology);
    	kbClient.setQuery(query.getQueryString());

    	return kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
	}

    /**
     * Attach data IRI to existing time series IRI in kb (i.e. insert "hasTimeSeries" relationship)
     * @param dataIRI data IRI provided as string
     * @param tsIRI time series IRI provided as string
     */
	protected void insertTimeSeriesAssociation(String dataIRI, String tsIRI) {

		// Check that the data IRI is not attached to a different time series IRI already
		String ts = getTimeSeries(dataIRI);
		if(ts != null) {
			throw new JPSRuntimeException(exceptionPrefix + "The data IRI " + dataIRI + " is already attached to time series " + ts);
		}

		// Check whether time series IRI exists
		if (!checkTimeSeriesExists(tsIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "Time series " + tsIRI + " does not exists in the Knowledge Graph");
		}

		// Add triple with "hasTimeSeries" relationship between dataIRI and tsIRI
		InsertDataQuery insert = Queries.INSERT_DATA(iri(dataIRI).has(hasTimeSeries, iri(tsIRI)));
		insert.prefix(prefix_ontology);
		kbClient.executeUpdate(insert.getQueryString());

	}

    /**
     * Remove relationship between dataIRI and associated time series from kb
     * @param dataIRI data IRI provided as string
     */
	protected void removeTimeSeriesAssociation(String dataIRI) {

		// Check whether associated time series has further data associated with it
		String tsIRI = getTimeSeries(dataIRI);

		if (tsIRI != null) {
			List<String> data = getAssociatedData(tsIRI);

			if (data.size() == 1) {
				// Remove entire time series if no further data is associated with it
				removeTimeSeries(tsIRI);
			} else {
				// Remove only relationship between dataIRI and tsIRI
				DeleteDataQuery delete = Queries.DELETE_DATA(iri(dataIRI).has(hasTimeSeries, iri(tsIRI)));
				delete.prefix(prefix_ontology);
				kbClient.executeUpdate(delete.getQueryString());
			}

		}
	}

    /**
     * Remove time series and all associated connections from kb (i.e. remove all triples with tsIRI as subject or object)
     * @param tsIRI timeseries IRI provided as string
     */
	protected void removeTimeSeries(String tsIRI) {

		if (checkTimeSeriesExists(tsIRI)) {

			SelectQuery query = Queries.SELECT();
			String queryKey = "numTs";

			Variable ts = SparqlBuilder.var("ts");
			Variable durIRI = SparqlBuilder.var("durIRI");
			Variable numTs = SparqlBuilder.var(queryKey);

			GraphPattern queryPattern1 = iri(tsIRI).has(hasAveragingPeriod, durIRI);
			GraphPattern queryPattern2 = ts.has(hasAveragingPeriod, durIRI);

			Assignment count = Expressions.count(ts).as(numTs);

			query.select(count).where(queryPattern1, queryPattern2).prefix(prefix_ontology);
			kbClient.setQuery(query.getQueryString());

			Integer avgTsIris = kbClient.executeQuery().getJSONObject(0).getInt(queryKey);

			if(avgTsIris==1){
				removeAverageTimeSeries(tsIRI);
			}
			else {
				// sub query to search for all triples with tsIRI as the subject/object
				SubSelect sub = GraphPatterns.select();
				Variable predicate1 = SparqlBuilder.var("a");
				Variable predicate2 = SparqlBuilder.var("b");
				Variable subject = SparqlBuilder.var("c");
				Variable object = SparqlBuilder.var("d");

				TriplePattern deleteTp1 = iri(tsIRI).has(predicate1, object);
				TriplePattern deleteTp2 = subject.has(predicate2, iri(tsIRI));
				sub.select(predicate1, predicate2, subject, object).where(deleteTp1, deleteTp2);

				// insert subquery into main sparql update
				ModifyQuery modify = Queries.MODIFY();
				modify.delete(deleteTp1, deleteTp2).where(sub);

				kbClient.setQuery(modify.getQueryString());
				kbClient.executeUpdate();
			}
		}
	}

	/**
	 * Returns the time series type IRI as a string for a given time series IRI
	 * @param tsIRI timeSeries IRI
	 * @return
	 */
	protected String getTimeSeriesType(String tsIRI){

		SelectQuery query = Queries.SELECT();
		TriplePattern queryPattern = iri(tsIRI).isA(tsType);

		query.select(tsType).where(queryPattern, vp).prefix(prefix_ontology);
		kbClient.setQuery(query.getQueryString());

		return kbClient.executeQuery().getJSONObject(0).getString(tsType.getQueryString().substring(1));
	}

	/**
	 * Remove all time series from kb
	 */
	protected void removeAllTimeSeries() {

		Variable predicate1 = SparqlBuilder.var("p1");
		Variable predicate2 = SparqlBuilder.var("p2");
		Variable predicate3 = SparqlBuilder.var("p3");
		Variable subject1 = SparqlBuilder.var("s1");
		Variable object1 = SparqlBuilder.var("o1");
		Variable object2 = SparqlBuilder.var("o2");
		Variable timeseries = SparqlBuilder.var("ts");
		Variable dur = SparqlBuilder.var("dur");

		TriplePattern deleteTp1 = timeseries.has(predicate1, object1);
		TriplePattern deleteTp2 = subject1.has(predicate2, timeseries);
		TriplePattern deleteTp3 = timeseries.has(hasAveragingPeriod, dur);
		TriplePattern deleteTp4 = dur.has(predicate3, object2);

		GraphPatternNotTriples optional1 = GraphPatterns.optional(deleteTp3);
		GraphPatternNotTriples optional2 = GraphPatterns.optional(deleteTp4);

		// insert subquery into main sparql update
		ModifyQuery modify = Queries.MODIFY();
		modify.delete(deleteTp1, deleteTp2, deleteTp3, deleteTp4).where(timeseries.isA(tsType), optional1, optional2, vp).prefix(prefix_ontology);

		kbClient.executeUpdate(modify.getQueryString());
	}

	/**
	 * Get time series IRI associated with given data IRI
	 * <p>Returns null if dataIRI does not exist or no time series is attached to dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The corresponding timeseries IRI as string
	 */
	public String getTimeSeries(String dataIRI) {

		String result = null;
		String queryString = "tsIRI";

		SelectQuery query = Queries.SELECT();
		Variable tsIRI = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = iri(dataIRI).has(hasTimeSeries, tsIRI);

		query.select(tsIRI).where(queryPattern).prefix(prefix_ontology);

		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		if(!queryResult.isEmpty()){
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
		}
		return result;
	}

	/**
	 * Get database URL associated with time series IRI
	 * <p>Returns null if time series does not exist or does not have associated database URL
	 * @param tsIRI timeseries IRI provided as string
	 * @return The URL to the database where data from that timeseries is stored as string
	 */
	public String getDbUrl(String tsIRI) {

		String result = null;
		String queryString = "dbURL";

		SelectQuery query = Queries.SELECT();
		Variable dbURL = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = iri(tsIRI).has(hasRDB, dbURL);

		query.select(dbURL).where(queryPattern).prefix(prefix_ontology);

		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		if(!queryResult.isEmpty()){
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
		}
		return result;
	}

	/**
	 * Get time unit associated with time series IRI
	 * <p>Returns null if time series does not exist or does not have associated time unit
	 * @param tsIRI timeseries IRI provided as string
	 * @return The time unit of timeseries as string
	 */
	public String getTimeUnit(String tsIRI) {

		String result = null;
		String queryString = "timeUnit";

		SelectQuery query = Queries.SELECT();
		Variable timeUnit = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = iri(tsIRI).has(hasTimeUnit, timeUnit);

		query.select(timeUnit).where(queryPattern).prefix(prefix_ontology);

		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		if(!queryResult.isEmpty()){
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
		}
		return result;
	}

	/**
	 * Get data IRIs associated with a given time series IRI
	 * <p>Returns empty List if time series does not exist or does not have associated data
	 * @param tsIRI timeseries IRI provided as string
	 * @return List of data IRIs attached to the time series as string
	 */
	public List<String>  getAssociatedData(String tsIRI) {
		String queryString = "dataIRI";
		SelectQuery query = Queries.SELECT();

		Variable data = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = data.has(hasTimeSeries, iri(tsIRI));

		query.select(data).where(queryPattern).prefix(prefix_ontology);

		return getInstances(query, queryString);
	}

    /**
     * Extract all time series IRIs from kb
     * <p>Returns empty List if no time series exist in kb
     * @return List of all time series IRI in the knowledge base provided as string
     */
	public List<String> getAllTimeSeries() {
		String queryString = "ts";
		SelectQuery query = Queries.SELECT();

		Variable ts = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = ts.isA(tsType);

		query.select(ts).where(queryPattern, vp).prefix(prefix_ontology);

		return getInstances(query, queryString);
	}

	/**
	 * Execute a query that selects all individuals in the kb that fulfill a certain pattern
	 * @param instanceSelectQuery the query to execute. Should be a select query with a single triple pattern where
	 *                            the subject represents the instance to retrieve.
	 * @param placeholder the placeholder used in the select query for the instance to retrieve provided as string without ?
	 * @return List of all instances' IRIs in the knowledge base provided as string
	 */
	private List<String> getInstances(SelectQuery instanceSelectQuery, String placeholder) {
		kbClient.setQuery(instanceSelectQuery.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();

		List<String> instanceIRIs = new ArrayList<>();
		for (int i = 0; i < queryResult.length(); i++) {
			instanceIRIs.add(queryResult.getJSONObject(i).getString(placeholder));
		}

		return instanceIRIs;
	}

	protected void initTS(String timeSeriesIRI, List<String> dataIRI, String dbURL, String timeUnit, Iri type, String durationIRI, Double duration, String unit){
		// Construct time series IRI
		Iri tsIRI;
		// Check whether timeseriesIRI follows IRI naming convention of namespace & local_name
		// If only local name is provided, iri() function would add filepath as default prefix
		// Every legal (full) IRI contains at least one ':' character to separate the scheme from the rest of the IRI
		if (Pattern.compile("\\w+\\S+:\\S+\\w+").matcher(timeSeriesIRI).matches()) {
			tsIRI = iri(timeSeriesIRI);
		} else {
			throw new JPSRuntimeException(exceptionPrefix + "Time series IRI does not have valid IRI format");
		}

		ModifyQuery modify = Queries.MODIFY();
		// set prefix declarations
		modify.prefix(prefix_ontology, prefix_kb, prefix_time);

		// define type
		modify.insert(tsIRI.isA(type));

		if(type.equals(AverageTimeSeries)){

			//numeric Duration
			if(duration <=0.0){
				throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
			}

			//Check if the given temporal unit is one of the allowed values.
			// Where, allowed values are of type ChronoUnit.SECONDS, ChronoUnit.MINUTES, ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS, ChronoUnit.YEARS
			if(!temporalUnitType.contains(unit)){
				throw new JPSRuntimeException(exceptionPrefix + "Temporal Unit: " + unit + " of invalid type");
			}

			modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
			modify.insert(iri(durationIRI).isA(Duration));
			modify.insert(iri(durationIRI).has(unitType, iri(unit)));
			modify.insert(iri(durationIRI).has(numericDuration, duration));

		}

		// Check that the data IRIs are not attached to a different time series IRI already
		for (String iri: dataIRI) {
			String ts = getTimeSeries(iri);
			if(ts != null) {
				throw new JPSRuntimeException(exceptionPrefix + "The data IRI " + iri + " is already attached to time series " + ts);
			}
		}

		// relational database URL
		modify.insert(tsIRI.has(hasRDB, literalOf(dbURL)));

		// link each data to time series
		for (String data : dataIRI) {
			TriplePattern tsTp = iri(data).has(hasTimeSeries, tsIRI);
			modify.insert(tsTp);
		}

		// optional: define time unit
		if (timeUnit != null) {
			modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeUnit)));
		}

		kbClient.executeUpdate(modify.getQueryString());
	}
}
