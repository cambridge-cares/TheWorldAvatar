package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.literalOf;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This class contains a collection of methods to interact with kb.
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesSparql {
	// kbClient with the endpoint (triplestore/owl file) specified
	private KnowledgeBaseClientInterface kbClient = null; 
	
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
    
    /**
     * Standard constructor
     * @param kbClient
     */
    public TimeSeriesSparql(KnowledgeBaseClientInterface kbClient) {
    	this.kbClient = kbClient;
    }
    
	public void setKBClient(KnowledgeBaseClientInterface kbClient) {
        this.kbClient = kbClient;
	}
    
	/**
	 * Check whether a particular time series (i.e. tsIRI) exists
	 * @param timeSeriesIRI
	 * @return
	 */
    public boolean checkTimeSeriesExists(String timeSeriesIRI) {
    	String query = String.format("ask {<%s> a <%s>}", timeSeriesIRI, (ns_ontology + "TimeSeries"));
    	kbClient.setQuery(query);
    	boolean timeSeriesExists = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return timeSeriesExists;
    }
    
	/**
	 * Check whether a particular data IRI exists
	 * @param dataIRI
	 * @return
	 */
    public boolean checkDataExists(String dataIRI) {
    	String query = String.format("ask {<%s> <%s> ?a}", dataIRI, (ns_ontology + "hasTimeSeries"));
    	kbClient.setQuery(query);
    	boolean timeSeriesExists = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return timeSeriesExists;
    }
    
    /**
     * Instantiate the time series instance (time unit is optional)
     * @param timeSeriesIRI
     * @param uuid
     * @param dataIRI
     * @param dbURL
     * @param timeUnit
     */    
    public void initTS(String timeSeriesIRI, String uuid, List<String> dataIRI, String dbURL, String timeUnit) {
    	//Construct time series IRI
    	Iri tsIRI;
    	if ((timeSeriesIRI != null) && (uuid == null)) {
    		// Check whether timeseriesIRI starts with "http://" to avoid default prefix of "file://" by iri() function
    		if (timeSeriesIRI.contains("http://")) {
    			tsIRI = iri(timeSeriesIRI);
    		} else {
    			throw new JPSRuntimeException("TimeSeriesSparql: Time series IRI needs to start with http://");
    		}
    		
    	} else if ((timeSeriesIRI == null) && (uuid != null)) {
    		tsIRI = iri(ns_kb + "TimeSeries_" + uuid);
    	} else {
    		throw new JPSRuntimeException("TimeSeriesSparql: Either the timeseries IRI OR the uuid shall be provided");
    	}
    	
    	ModifyQuery modify = Queries.MODIFY();

    	// set prefix declarations
    	modify.prefix(prefix_ontology, prefix_kb);
    	// define type
    	modify.insert(tsIRI.isA(TimeSeries));
    	// relational database URL
    	// mh807: definition of dbURL as Rdf.literalOf(dbURL) needed?
    	modify.insert(tsIRI.has(hasRDB, literalOf(dbURL)));
    	
    	// link each data to time series
    	for (String data : dataIRI) {
    		TriplePattern ts_tp = iri(data).has(hasTimeSeries, tsIRI);
    		modify.insert(ts_tp);
    	}

    	// optional: define time unit
    	if (timeUnit != null) {
    		// mh807: definition of timeUnit as literal or iri?
    		modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeUnit)));
    		//modify.insert(tsIRI.has(hasTimeUnit, iri(timeUnit)));
    	}

    	kbClient.executeUpdate(modify.getQueryString());
    }
    
    /**
     * Count number of time series IRIs in kb
     * <p>Previously used to generate a new unique time series IRI
     * @return
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
    	
    	int queryresult = kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
    	
    	return queryresult;
	}
	
    /**
     * Remove relationship between dataIRI and associated time series from kb
     * @param dataIRI
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
     * @param tsIRI
     */
	public void removeTimeSeries(String tsIRI) {
		
		// mh807: Necessary to check whether tsIRI (still) exists in kb?
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
	 * @param dataIRI
	 * @return
	 */
	public String getTimeSeries(String dataIRI) {
		
		String result = null;
		
		if (checkDataExists(dataIRI)) {		

			String queryString = "tsIRI";
			
			SelectQuery query = Queries.SELECT();
			Variable tsIRI = SparqlBuilder.var(queryString);
			//TriplePattern queryPattern = iri(dataIRI).has(hasTimeSeries, tsIRI);
			GraphPattern queryPattern = iri(dataIRI).has(hasTimeSeries, tsIRI);
			
			query.select(tsIRI).where(queryPattern).prefix(prefix_ontology);
		
			kbClient.setQuery(query.getQueryString());
			result = kbClient.executeQuery().getJSONObject(0).getString(queryString);					
		}
		
		return result;
		
	}
	
	/**
	 * Get data IRIs associated with given time series IRI
	 * @param tsIRI
	 * @return
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
     * @return
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
