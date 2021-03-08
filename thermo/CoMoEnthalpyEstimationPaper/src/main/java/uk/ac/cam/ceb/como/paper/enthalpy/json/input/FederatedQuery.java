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

/**
 * 
 * @author NK510 Nenad Krdzavac (caresssd@hermes.cam.ac.uk
 * @author msff2 Feroz Farazi (msff2@cam.ac.uk)
 *
 * Sometimes connections time out with remote repositories.
 */
public class FederatedQuery {

	/**
	 * 
	 * @param ontocompchemServerUrl the ontocomcphem server url
	 * @param ontospecieskbServerUrl the ontospecies server url on the remote repository
	 * @param ontospeciesServerUrls  the ontospecies server url
	 * @param query the query string
	 * @return linked list of species beans that include level of theory, ontocompchem iri, ontospecies iri.
	 * @throws Exception
	 */
public static LinkedList<SpeciesBean> runFederatedSPARQLSpeciesBean(String ontocompchemServerUrl, String ontospecieskbServerUrl, String ontospeciesServerUrls,String query) throws Exception {
	   
	    LinkedList<SpeciesBean> speciesBeanLinkedList = new LinkedList<SpeciesBean>();
	   
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Federated query via three repositories: Claudius ontocompchem and ontospecies, and localhost ontospecies
		 *  
		 */
		
		Repository repository = FedXFactory.createSparqlFederation(Arrays.asList(ontocompchemServerUrl,ontospecieskbServerUrl,ontospeciesServerUrls));
		
	    try {
			
		RepositoryConnection conn = repository.getConnection();
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Returns a result of federated sparql query via ontospecies repositories stored on local host and on Claudius server.
		 * 
		 */
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		
		try{
			
		TupleQueryResult tqRes = tq.evaluate();
		
		while (tqRes.hasNext()) {
					
		BindingSet bSet = tqRes.next();
		
		if((bSet.getValue("species").stringValue()!=null) && (bSet.getValue("compchemspecies").stringValue()!=null)) {
	
		SpeciesBean jsonBean = new SpeciesBean(bSet.getValue("levelOfTheory").stringValue(), bSet.getValue("compchemspecies").stringValue(),bSet.getValue("species").stringValue());
			
//		SpeciesBean jsonBean = new SpeciesBean(bSet.getValue("compchemspecies").stringValue(),bSet.getValue("species").stringValue());

		/**
		 * 
		 * Adds only one pair of ontocomcphem species iri and ontospecies iri.
		 * 
		 */

		speciesBeanLinkedList.add(jsonBean);
		
//		}
		
		}
		
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

