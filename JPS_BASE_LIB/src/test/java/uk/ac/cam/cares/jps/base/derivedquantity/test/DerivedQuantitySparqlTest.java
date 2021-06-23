package uk.ac.cam.cares.jps.base.derivedquantity.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantitySparql;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;

public class DerivedQuantitySparqlTest extends TestCase{
	String endpoint = "http://localhost:8080/blazegraph/namespace/derivedquantity/sparql";
	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
	
	public void testInitParameters() {
		String agentIRIa = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#SimpleAgentA";
		String agentURLa = "http://localhost:8080/JPS_VIRTUALSENSOR/SimpleAgentA";
		
		String agentIRIb = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#SimpleAgentB";
		String agentURLb = "http://localhost:8080/JPS_VIRTUALSENSOR/SimpleAgentB";
		
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		String a = "http://a";
		String b = "http://b";
		String c = "http://c";
		
		DerivedQuantitySparql.initDerivedQuantity(kbClient, a, agentIRIa, agentURLa, b);
		DerivedQuantitySparql.initDerivedQuantity(kbClient, b, agentIRIb, agentURLb, c);
		DerivedQuantitySparql.initInputTimeStamp(kbClient, c);
	}
	
	public void testGetUrl() {
		String derivedQuantity = "http://derivedQuantity";
		kbClient.setQueryEndpoint(endpoint);
        DerivedQuantitySparql.getAgentUrl(kbClient, derivedQuantity);
	}
	
	public void testGetInputs() {
		String iri = "http://input1";
		kbClient.setQueryEndpoint(endpoint);
		DerivedQuantitySparql.getInputs(kbClient, iri);
	}
	
	public void testUpdateTimestamp() {
		String iri = "http://input3";
		kbClient.setUpdateEndpoint(endpoint);
		DerivedQuantitySparql.updateTimeStamp(kbClient,iri);
	}
	
	public void testReplaceDerivedTriples() {
		String oldIRI = "http://b";
		String newIRI = "http://e";
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		DerivedQuantitySparql.replaceDerivedTriples(kbClient, oldIRI, newIRI);
	}
	
	public void testCountTime() {
		kbClient.setQueryEndpoint(endpoint);
		DerivedQuantitySparql.countTimeInstance(kbClient);
	}
}
