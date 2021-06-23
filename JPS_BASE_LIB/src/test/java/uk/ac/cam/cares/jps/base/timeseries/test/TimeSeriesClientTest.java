package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

public class TimeSeriesClientTest extends TestCase{
	String dbURL = "jdbc:postgresql:timeseries";
	String user = "postgres";
	String password = "postgres";
	
    public void testInit() {
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
        List<String> dataIRI = List.of("http://data1", "http://data2", "http://data3");
        List<Class<?>> dataClass = List.of(Double.class,Double.class,Integer.class);

        tsClient.init(Integer.class, dataIRI, dataClass);
    }
    
    public void testAddData() {
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
    	// mock TimeSeries object
    	List<Integer> timestamps = List.of(4, 5, 6);
        List<String> dataIRI = List.of("http://data1", "http://data3");
        List<Double> data1 = List.of(1.0,2.0,3.0);
        List<Integer> data2 = List.of(1,2,3);
        
        List<List<?>> data = List.of(data1,data2);
        
        TimeSeries ts = new TimeSeries(timestamps,dataIRI,data);
        
        tsClient.addData(ts);
    }
    
    public void testGetData() {
    	List<String> dataIRI = List.of("http://data1", "http://data2");
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
    	tsClient.getTimeSeries(dataIRI);
    }
    
    public void testGetDataWithinBounds() {
    	List<String> dataIRI = List.of("http://data1", "http://data2");
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
    	tsClient.getTimeSeriesWithinBounds(dataIRI,4,5);
    }
    
    public void testDeleteRows() {
    	String dataIRI = "http://data1";
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
    	tsClient.deleteRows(dataIRI,4,5);
    }
    
    public void testDeleteTimeSeries() {
    	String dataIRI = "http://data1";
    	// kb client
        String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	kbClient.setUpdateEndpoint(endpoint);
    	kbClient.setQueryEndpoint(endpoint);
    	
    	TimeSeriesRDBClient tsClient = new TimeSeriesRDBClient();
    	tsClient.setRdbURL(dbURL);
    	tsClient.setRdbUser(user);
    	tsClient.setRdbPassword(password);
    	tsClient.setKBClient(kbClient);
    	
    	tsClient.deleteTimeSeries(dataIRI);
    }
}
