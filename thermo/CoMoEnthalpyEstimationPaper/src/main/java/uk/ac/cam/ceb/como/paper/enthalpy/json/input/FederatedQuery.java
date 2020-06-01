package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

import java.util.Arrays;
import java.util.LinkedList;

import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

public class FederatedQuery {

	public static void runFederatedSPARQLTest(String ontocompchemServerUrl, String ontospecieskbServerUrl, String ontospeciesServerUrls,String query) throws Exception {
		
	    Repository repository = FedXFactory.newFederation()
	    		
	    		/**
	    		 * 
	    		 * @author NK510 
	    		 * ontocompchem server url
	    		 * 
	    		 */
		    .withSparqlEndpoint(ontocompchemServerUrl)
		    
		    /**
		     * 
		     * @author NK510
		     * ontospecieskb server url.
		     * 
		     */
	        .withSparqlEndpoint(ontospecieskbServerUrl)
		    .create();
		
		
        
	    
	    try {
			
		RepositoryConnection conn = repository.getConnection();
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Returns a result of federated sparql query via ontospecieskb repositories stored on local host and on Claudius server.
		 * 
		 */
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		
		try{
			
		TupleQueryResult tqRes = tq.evaluate();
		
		
		while (tqRes.hasNext()) {
					
		BindingSet bSet = tqRes.next();
		
		
		if((bSet.getValue("species").stringValue()!=null) && (bSet.getValue("compchemspecies").stringValue()!=null)) {
			
		System.out.println(" - ontocompchem species iri: "+ bSet.getValue("compchemspecies").stringValue());
		System.out.println(" - unique ontospecies : " + bSet.getValue("species").stringValue());
		
		}
		
//		System.out.println(i + ". ontocompchem species iri: "+ bSet.getValue("compchemspecies").stringValue() + " , unique ontospecies : " + bSet.getValue("species").stringValue() + " , cas-reg-id:" + bSet.getValue("crid").stringValue());
		
		}
		
		}catch(TupleQueryResultHandlerException e) {
		
		e.printStackTrace();
		
		}
		
		conn.close();
		
		}catch(RepositoryException e) {
		
			e.printStackTrace();
		}
	    
		repository.shutDown();	

		}
	
	
	
	public static LinkedList<SpeciesBean> runFederatedSPARQLSpeciesBean(String ontocompchemServerUrl, String ontospecieskbServerUrl, String ontospeciesServerUrls,String query) throws Exception {

		LinkedList<SpeciesBean> speciesBeanLinkedList = new LinkedList<SpeciesBean>();
		
		
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Federated query via three repositories: Claudius ontocompchem and ontospecieskb, and localhost ontospecies
		 *  
		 */
		
		Repository repository = FedXFactory.createSparqlFederation(Arrays.asList(ontocompchemServerUrl,ontospecieskbServerUrl,ontospeciesServerUrls));
		
        
	    
	    try {
			
		RepositoryConnection conn = repository.getConnection();
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Returns a result of federated sparql query via ontospecieskb repositories stored on local host and on Claudius server.
		 * 
		 */
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		
		try{
			
		TupleQueryResult tqRes = tq.evaluate();
		
		
		while (tqRes.hasNext()) {
					
		BindingSet bSet = tqRes.next();
		
		if((bSet.getValue("species").stringValue()!=null) && (bSet.getValue("compchemspecies").stringValue()!=null)) {
	
		SpeciesBean jsonBean = new SpeciesBean(bSet.getValue("compchemspecies").stringValue(),bSet.getValue("species").stringValue());

		/**
		 * Adds only one pair of ontocomcphem species iri and ontospecies iri.
		 */
		if(speciesBeanLinkedList.size()==0) {
		
		System.out.println(" - ontocompchem species iri: "+ bSet.getValue("compchemspecies").stringValue());
		System.out.println(" - unique ontospecies iri : " + bSet.getValue("species").stringValue());
		

		speciesBeanLinkedList.add(jsonBean);
		
		}
		
		}
		
//		System.out.println(i + ". ontocompchem species iri: "+ bSet.getValue("compchemspecies").stringValue() + " , unique ontospecies : " + bSet.getValue("species").stringValue() + " , cas-reg-id:" + bSet.getValue("crid").stringValue());
		
		}
		
		}catch(TupleQueryResultHandlerException e) {
		
		e.printStackTrace();
		
		}
		
		conn.close();
		
		}catch(RepositoryException e) {
		
			e.printStackTrace();
		}
	    
		repository.shutDown();	

		return speciesBeanLinkedList;
		
		}
	
}