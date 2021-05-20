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
    	
    	// mock TimeSeries object
    	List<Integer> timestamps = List.of(1, 2, 3);
        List<String> dataIRI = List.of("http://data1", "http://data2", "http://data3");
        List<Double> data1 = List.of(1.0,2.0,3.0);
        List<Double> data2 = List.of(1.0,2.0,3.0);
        List<Double> data3 = List.of(1.0,2.0,3.0);
        
        TimeSeries<Integer,Double> ts = new TimeSeries<Integer,Double>(timestamps,dataIRI,data1,data2,data3);
        
        tsClient.init(ts);
    }
}
