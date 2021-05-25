package uk.ac.cam.cares.jps.base.derivedquantity;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

public class DerivedQuantityClient {
	// defines the endpoint DerivedQuantityClient should act on
    KnowledgeBaseClientInterface kbClient;
    
    public DerivedQuantityClient(KnowledgeBaseClientInterface kbClient) {
    	this.kbClient = kbClient;
    }

    public void initDerivedQuantityAgent(String agentIRI, String agentURL) {
    	// pure triples if namedGraph = null
    	DerivedQuantitySparql.initAgent(this.kbClient, agentIRI, agentURL);
    }
    
    /**
     * Links the given derived quantity to its inputs and the agent used to derived it
     * @param derivedQuantityIRI
     * @param inputsIRI
     * @param agentIRI
     */
    public void initParameters(String derivedQuantityIRI, String[] inputsIRI, String agentIRI) {
    	DerivedQuantitySparql.initParameters(this.kbClient, derivedQuantityIRI, inputsIRI, agentIRI);
    }
}
