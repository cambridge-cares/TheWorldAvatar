package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.literalOf;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.regex.*;

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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * This class contains a collection of methods to interact with kb.
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesSparql {
	// kbClient with the endpoint (triplestore/owl file) specified
	private StoreClientInterface kbClient;
	
	// Namespaces for ontology and kb
	public static final String ns_ontology = "http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#";
	public static final String ns_kb = "http://www.theworldavatar.com/kb/ontotimeseries/";
	
	// Prefixes
	private static final Prefix prefix_ontology = SparqlBuilder.prefix("ts", iri(ns_ontology));
	private static final Prefix prefix_kb = SparqlBuilder.prefix("kb", iri(ns_kb));
	
	// RDF type
    private static final Iri TimeSeries = prefix_ontology.iri("TimeSeries");
    
    // Relationships
    private static final Iri hasTimeSeries = prefix_ontology.iri("hasTimeSeries");
    private static final Iri hasRDB = prefix_ontology.iri("hasRDB");
    private static final Iri hasTimeUnit = prefix_ontology.iri("hasTimeUnit");

    // Fields for class specific exceptions
	private final String exceptionPrefix = this.getClass().toString() + ": ";

    /**
     * Standard constructor
     * @param kbClient: The knowledge base client used to query and update the knowledge base containing timeseries information
     */
    public TimeSeriesSparql(StoreClientInterface kbClient) {
    	this.kbClient = kbClient;
    }

    /**
	 * Setter for the client
	 * @param kbClient: The knowledge base client used to query and update the knowledge base containing timeseries information
	*/
	public void setKBClient(StoreClientInterface kbClient) {
        this.kbClient = kbClient;
	}
	
	/**
	 * Load SPARQL query and update endpoints from properties file ("timeseries.properties") at specified path
	 * @param filepath: Absolute path to timeseries properties file
	 */
	public void loadSparqlConfigs(String filepath) throws IOException {
		
		try {
			File file = new File(filepath);
			
			if (!file.exists()) {
				throw new JPSRuntimeException("1");
			}
			
			// Load properties in try-with-resource block to ensure closing of InputStream
			try (InputStream input = new FileInputStream(file)) {
				
				// Load properties file from specified path
	            Properties prop = new Properties();
	            prop.load(input);
            
	            // Get the property values and assign
	            if (prop.containsKey("sparql.query.endpoint")) {
	            	kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
	            } else {
	            	throw new JPSRuntimeException("2");
	            }
	            if (prop.containsKey("sparql.update.endpoint")) {
	            	kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
	            } else {
	            	throw new JPSRuntimeException("3");
	            }
			} catch (Exception e) {
				throw e;
			}
		} catch (Exception e) {
			if (e instanceof JPSRuntimeException) {
				String m = "";
				switch (e.getMessage()) {
					case "1":
						m = "TimeSeriesSparql: No properties file found at specified filepath: " + filepath;
						break;
					case "2":
						m = "TimeSeriesSparql: Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ";
						break;
					case "3":
						m = "TimeSeriesSparql: Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ";
						break;
					default:
						throw new JPSRuntimeException(e.getMessage());							
				}
				throw new JPSRuntimeException(m);
			} else {
				throw e;
			}			
		}
	}
    
	/**
	 * Check whether a particular time series (i.e. tsIRI) exists
	 * @param timeSeriesIRI: timeseries IRI provided as string
	 * @return True if an instance with the IRI exists, false otherwise
	 */
    public boolean checkTimeSeriesExists(String timeSeriesIRI) {
    	String query = String.format("ask {<%s> a <%s>}", timeSeriesIRI, (ns_ontology + "TimeSeries"));
    	kbClient.setQuery(query);
    	return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }
    
	/**
	 * Check whether a particular data IRI is attached to a time series
	 * @param dataIRI: data IRI provided as string
	 * @return True if an instance with the IRI exists, false otherwise
	 */
    public boolean checkDataExists(String dataIRI) {
    	String query = String.format("ask {<%s> <%s> ?a}", dataIRI, (ns_ontology + "hasTimeSeries"));
    	kbClient.setQuery(query);
    	return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }
    
	/**
	 * Check whether time series IRI has time unit
	 * @param tsIRI: timeseries IRI provided as string
	 * @return True if an the timeseries instance has a defined time unit, false otherwise
	 */
    private boolean checkTimeUnitExists(String tsIRI) {
    	String query = String.format("ask {<%s> <%s> ?a}", tsIRI, (ns_ontology + "hasTimeUnit"));
    	kbClient.setQuery(query);
    	return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }
    
    /**
     * Instantiate the time series instance (time unit is optional)
     * @param timeSeriesIRI: timeseries IRI provided as string
     * @param dataIRI: list of data IRI provided as string that should be attached to the timeseries
     * @param dbURL: URL of the database where the timeseries data is stored provided as string
     * @param timeUnit: the time unit of the time series (optional)
     */    
    public void initTS(String timeSeriesIRI, List<String> dataIRI, String dbURL, String timeUnit) {
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

		// Check that the time series IRI is not yet in the Knowledge Graph
		if (checkTimeSeriesExists(timeSeriesIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "Time series " + timeSeriesIRI + " already in the Knowledge Graph");
		}
		// Check that the data IRIs are not attached to a different time series IRI already
		for (String iri: dataIRI) {
			String ts = getTimeSeries(iri);
			if(!(ts == null)) {
				throw new JPSRuntimeException(exceptionPrefix + "The data IRI " + iri + " is already attached to time series " + ts);
			}
		}

    	ModifyQuery modify = Queries.MODIFY();

    	// set prefix declarations
    	modify.prefix(prefix_ontology, prefix_kb);
    	// define type
    	modify.insert(tsIRI.isA(TimeSeries));
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
    
    /**
     * Count number of time series IRIs in kb
     * <p>Previously used to generate a new unique time series IRI
     * @return Number of timeseries instances in the knowledge base as int
     */
	public int countTS() {
		SelectQuery query = Queries.SELECT();
    	String queryKey = "numtimeseries";
    	Variable ts = query.var();
    	Variable numtimeseries = SparqlBuilder.var(queryKey);
    	GraphPattern querypattern = ts.isA(TimeSeries);
    	Assignment count = Expressions.count(ts).as(numtimeseries);
    	
    	// set prefix declaration
    	query.prefix(prefix_ontology);
    	query.select(count).where(querypattern);
    	kbClient.setQuery(query.getQueryString());
    	
    	return kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
	}
	
    /**
     * Attach data IRI to existing time series IRI in kb (i.e. insert "hasTimeSeries" relationship)
     * @param dataIRI: data IRI provided as string
     * @param tsIRI: time series IRI provided as string
     */
	public void insertTimeSeriesAssociation(String dataIRI, String tsIRI) {
		
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
     * @param dataIRI: data IRI provided as string
     */
	public void removeTimeSeriesAssociation(String dataIRI) {
		
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
     * Remove time series and all associated connections from kb
     * @param tsIRI: timeseries IRI provided as string
     */
	public void removeTimeSeries(String tsIRI) {

		if (checkTimeSeriesExists(tsIRI)) {
			
			// sub query to search for all triples with tsIRI as the subject/object
			SubSelect sub = GraphPatterns.select();
			Variable predicate1 = SparqlBuilder.var("a");
			Variable predicate2 = SparqlBuilder.var("b");
			Variable subject = SparqlBuilder.var("c");
			Variable object = SparqlBuilder.var("d");
			
			TriplePattern delete_tp1 = iri(tsIRI).has(predicate1, object);
			TriplePattern delete_tp2 = subject.has(predicate2, iri(tsIRI));		
			sub.select(predicate1, predicate2, subject, object).where(delete_tp1, delete_tp2);
			
			// insert subquery into main sparql update
			ModifyQuery modify = Queries.MODIFY();
			modify.delete(delete_tp1, delete_tp2).where(sub);
			
			kbClient.setQuery(modify.getQueryString());
			kbClient.executeUpdate();
		}
	}
	
	/**
	 * Remove all time series from kb
	 */
	public void removeAllTimeSeries() {
		// Get all time series in kb
		List<String> tsIRI = getAllTimeSeries();
		
		// Remove all time series
		if (!tsIRI.isEmpty()) {
			for (String ts : tsIRI) {
				removeTimeSeries(ts);
			}
		}
	}
	
	/**
	 * Get time series IRI associated with given data IRI
	 * <p>Returns null if no time series is attached to dataIRI
	 * @param dataIRI: data IRI provided as string
	 * @return The corresponding timeseries IRI as string
	 */
	public String getTimeSeries(String dataIRI) {
		
		String result = null;
		
		if (checkDataExists(dataIRI)) {		

			String queryString = "tsIRI";
			
			SelectQuery query = Queries.SELECT();
			Variable tsIRI = SparqlBuilder.var(queryString);
			TriplePattern queryPattern = iri(dataIRI).has(hasTimeSeries, tsIRI);
			
			query.select(tsIRI).where(queryPattern).prefix(prefix_ontology);
		
			kbClient.setQuery(query.getQueryString());
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);					
		}
		
		return result;
		
	}
	
	/**
	 * Get database URL associated with time series IRI
	 * <p>Returns null if time series does not exist
	 * @param tsIRI: timeseries IRI provided as string
	 * @return The URL to the database where data from that timeseries is stored as string
	 */
	public String getDbUrl(String tsIRI) {
		
		String result = null;
		
		if (checkTimeSeriesExists(tsIRI)) {		

			String queryString = "dbURL";
			
			SelectQuery query = Queries.SELECT();
			Variable dbURL = SparqlBuilder.var(queryString);
			TriplePattern queryPattern = iri(tsIRI).has(hasRDB, dbURL);
			
			query.select(dbURL).where(queryPattern).prefix(prefix_ontology);
		
			kbClient.setQuery(query.getQueryString());
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);					
		}
		
		return result;		
	}
	
	/**
	 * Get time unit associated with time series IRI
	 * <p>Returns null if time series does not exist or does not have associated time unit
	 * @param tsIRI: timeseries IRI provided as string
	 * @return The time unit of timeseries as string
	 */
	public String getTimeUnit(String tsIRI) {
		
		String result = null;
		
		if (checkTimeSeriesExists(tsIRI) && checkTimeUnitExists(tsIRI)) {		

			String queryString = "timeUnit";
			
			SelectQuery query = Queries.SELECT();
			Variable timeUnit = SparqlBuilder.var(queryString);
			TriplePattern queryPattern = iri(tsIRI).has(hasTimeUnit, timeUnit);
			
			query.select(timeUnit).where(queryPattern).prefix(prefix_ontology);
			
			kbClient.setQuery(query.getQueryString());
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
		}

		return result;		
	}
	
	/**
	 * Get data IRIs associated with given time series IRI
	 * @param tsIRI: timeseries IRI provided as string
	 * @return List of data IRIs attached to the time series as string
	 */
	public List<String>  getAssociatedData(String tsIRI) {
		String queryString = "dataIRI";		
		SelectQuery query = Queries.SELECT();
		
		Variable data = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = data.has(hasTimeSeries, iri(tsIRI));
		
		query.select(data).where(queryPattern).prefix(prefix_ontology);
		
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		List<String> dataIRI = new ArrayList<String>();
		for (int i = 0; i < queryResult.length(); i++) {
			dataIRI.add(queryResult.getJSONObject(i).getString(queryString));
		}
		
		return dataIRI;
	}
	
    /**
     * Extract all time series IRIs from kb
     * @return List of all time series IRI in the knowledge base provided as string
     */
	public List<String> getAllTimeSeries() {
		String queryString = "ts";
		SelectQuery query = Queries.SELECT();
		
		Variable ts = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = ts.isA(TimeSeries);
		
		query.select(ts).where(queryPattern).prefix(prefix_ontology);
		
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		List<String> tsIRI = new ArrayList<String>();
		for (int i = 0; i < queryResult.length(); i++) {
			tsIRI.add(queryResult.getJSONObject(i).getString(queryString));
		}
		
		return tsIRI;
	}
}
