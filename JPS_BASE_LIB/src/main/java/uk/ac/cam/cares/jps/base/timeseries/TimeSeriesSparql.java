package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This class contains a collection of methods to interact with kb
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesSparql {
	//namespace
	public static final String namespace = "http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#";
	
	//prefix
	private static final Prefix p_timeseries = SparqlBuilder.prefix(iri(namespace));
	
	// RDF type
	private static final String TimeSeriesString = namespace + "TimeSeries";
    private static final Iri TimeSeries = iri(TimeSeriesString);
    
    // relation
    private static final Iri hasTimeSeries = p_timeseries.iri("hasTimeSeries");
    private static final Iri hasRDB = p_timeseries.iri("hasRDB");
    private static final Iri hasTimeUnit = p_timeseries.iri("hasTimeUnit");
    
    public static boolean checkTimeSeriesExists(KnowledgeBaseClientInterface kbClient,String timeSeriesIRI) {
    	String query = String.format("ask {<%s> a <%s>}",timeSeriesIRI,TimeSeriesString);
    	kbClient.setQuery(query);
    	boolean timeSeriesExists = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    	return timeSeriesExists;
    }
    
    /**
     * Instantiates the time series instance, named graph and time unit are optional
     * @param kbClient
     * @param timeSeriesIRI
     * @param dataIRI
     * @param namedGraph
     */
    
    public static void initTS(KnowledgeBaseClientInterface kbClient, String timeSeriesIRI, List<String> dataIRI, String dbURL, String timeUnit) {
        Iri tsIRI = iri(timeSeriesIRI);
    	
    	ModifyQuery modify = Queries.MODIFY();

    	//set prefix
    	modify.prefix(p_timeseries);
    	// define type
    	modify.insert(tsIRI.isA(TimeSeries));
    	// db URL
    	modify.insert(tsIRI.has(hasRDB,dbURL));
    	
    	// link each data to time series
    	for (String data : dataIRI) {
    		TriplePattern ts_tp = iri(data).has(hasTimeSeries,tsIRI);
    		modify.insert(ts_tp);
    	}

    	// optional to define time unit
    	if (timeUnit != null) {
    		modify.insert(tsIRI.has(hasTimeUnit, iri(timeUnit)));
    	}

    	kbClient.executeUpdate(modify.getQueryString());
    }
    
    /**
     * counts number of time series IRI in kb, mainly used to generate a new unique IRI
     * @param kbClient
     * @return
     */
	public static int countTS(KnowledgeBaseClientInterface kbClient) {
		SelectQuery query = Queries.SELECT();
    	String queryKey = "numtimeseries";
    	Variable ts = query.var();
    	Variable numtimeseries = SparqlBuilder.var(queryKey);
    	GraphPattern querypattern = ts.isA(TimeSeries);
    	Assignment count = Expressions.count(ts).as(numtimeseries);
    	
    	query.select(count).where(querypattern);
    	kbClient.setQuery(query.getQueryString());
    	
    	int queryresult = kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
    	
    	return queryresult;
	}
	
	public static void removeTimeSeries(KnowledgeBaseClientInterface kbClient, String tsIRI) {
		// sub query to search for all triples with tsIRI as the subject/object
		SubSelect sub = GraphPatterns.select();
		Variable predicate1 = SparqlBuilder.var("a");
		Variable predicate2 = SparqlBuilder.var("b");
		Variable subject = SparqlBuilder.var("c");
		Variable object = SparqlBuilder.var("d");
		
		TriplePattern delete_tp1 = iri(tsIRI).has(predicate1,object);
		TriplePattern delete_tp2 = subject.has(predicate2,iri(tsIRI));		
		sub.select(predicate1,predicate2,subject,object).where(delete_tp1,delete_tp2);
		
		// insert subquery into main sparql update
		ModifyQuery modify = Queries.MODIFY();
		modify.delete(delete_tp1,delete_tp2).where(sub);
		
		kbClient.setQuery(modify.getQueryString());
		kbClient.executeUpdate();
	}
	
	public static List<String> getAllTimeSeries(KnowledgeBaseClientInterface kbClient) {
		String queryString = "ts";
		SelectQuery query = Queries.SELECT();
		
		Variable ts = SparqlBuilder.var(queryString);
		TriplePattern queryPattern = ts.isA(TimeSeries);
		
		query.select(ts).where(queryPattern).prefix(p_timeseries);
		
		kbClient.setQuery(query.getQueryString());
		JSONArray queryResult = kbClient.executeQuery();
		
		List<String> tsIRI = new ArrayList<String>();
		for (int i = 0; i < queryResult.length(); i++) {
			tsIRI.add(queryResult.getJSONObject(i).getString(queryString));
		}
		
		return tsIRI;
	}
}
