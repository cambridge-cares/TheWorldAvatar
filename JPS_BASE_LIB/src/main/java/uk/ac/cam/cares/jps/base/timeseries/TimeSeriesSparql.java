package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This class contains a collection of methods to interact with kb
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesSparql {
	// RDF type
    private static final Iri TimeSeries = iri("http://www.theworldavatar.com/kb/ontotimeseries/OntoTimeSeries.owl#TimeSeries");
    
	public static int countTimeSeriesInstances(KnowledgeBaseClientInterface kbClient) {
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

}
