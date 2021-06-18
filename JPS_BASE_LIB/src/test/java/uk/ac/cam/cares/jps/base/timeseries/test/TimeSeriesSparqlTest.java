package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

public class TimeSeriesSparqlTest extends TestCase{
	
	public void testTimeSeriesExists() {
		String timeseries = "http://www.theworldavatar.com/kb/ontotimeseries/OntoTimeSeries.owl#ts1";

		String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setQueryEndpoint(endpoint);

    	TimeSeriesSparql.checkTimeSeriesExists(kbClient, timeseries);
	}
	
	public void testInstantiateInNamedGraph() {
		String namedGraph = "http://namedgraph";
		String timeseries = "http://www.theworldavatar.com/kb/ontotimeseries/OntoTimeSeries.owl#ts2";
		String url = "jdbc:postgresql:timeseries";
		
		List<String> data = new ArrayList<String>();
		data.add("http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#value1");
		data.add("http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#value2");
		
		String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	
		TimeSeriesSparql.initTS(kbClient, timeseries, data, url, null);
	}
	
    public void testCountTS() {
    	String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setQueryEndpoint(endpoint);
        TimeSeriesSparql.countTS(kbClient);
    }
    
    public void testCountTSinNamedGraph() {
    	String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	String namedGraph = "http://namedgraph";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setQueryEndpoint(endpoint);
        TimeSeriesSparql.countTS(kbClient,namedGraph);
    }
}
