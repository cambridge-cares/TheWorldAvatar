package uk.ac.cam.cares.jps.base.derivedquantity.test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantityClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;

public class DerivedQuantityClientTest extends TestCase{
	String endpoint = "http://localhost:8080/blazegraph/namespace/derivedquantity/sparql";
	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    
	public void testUpdateInstance() {
		String derivedIRI = "http://a";
		kbClient.setQueryEndpoint(endpoint);
		kbClient.setUpdateEndpoint(endpoint);
		DerivedQuantityClient devClient = new DerivedQuantityClient(kbClient);
		devClient.updateInstance(derivedIRI);
	}
	
	public void test() {
		List<String> derivedList = new ArrayList<>(); 
		derivedList.add("Ab");
		derivedList.contains("Ab");
	}
}
