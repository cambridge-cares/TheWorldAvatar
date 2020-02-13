package uk.ac.cam.cares.query;


import java.util.ArrayList;

import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

/**
 * 
 * @author NK510
 * Federated SPARQL query via localhost and remote repository stored on Claudius server.
 *
 */
public class FederatedQuery {
	
	public static void runFederatedSPARQL(String localHostSparqlEndPoint, String claudiusServerSparqlEndPoint, String query) throws Exception {
	
    Repository repository = FedXFactory.newFederation()
    		
    		/**
    		 * 
    		 * @author NK510 
    		 * a Sparql endpoint on localhost
    		 * 
    		 */
	    .withSparqlEndpoint(localHostSparqlEndPoint)
	    
	    /**
	     * 
	     * @author NK510
	     * a sparql endpoint on Caludius server .
	     * 
	     */
        .withSparqlEndpoint(claudiusServerSparqlEndPoint)
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
	
	int count = 0;
	
	ArrayList<NISTSpeciesId> nistSpeciesIdList = new ArrayList<NISTSpeciesId>();
	
	while (tqRes.hasNext()) {
				
				BindingSet bSet = tqRes.next();
				
//				System.out.println(b);
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * Stores query results into NISTSpeciesId bean: species identifier, cas reg number, atomic bond, geometry.
				 * 
				 */
				
				NISTSpeciesId nistSpeciesId = new NISTSpeciesId(bSet.getValue("species").stringValue(), bSet.getValue("crid").stringValue(), bSet.getValue("atomicBond").stringValue(), bSet.getValue("geometry").stringValue());
				
				nistSpeciesIdList.add(nistSpeciesId);
				
				System.out.println(bSet.getValue("species").stringValue() + " , " + bSet.getValue("crid").stringValue() + " , " + bSet.getValue("atomicBond").stringValue() + " , " + bSet.getValue("geometry").stringValue());
				
				count++;
	}
	
	System.out.println("Results: " + count);
	
	}catch(TupleQueryResultHandlerException e) {
	
		e.printStackTrace();
	}
	
	}catch(RepositoryException e) {
	
		e.printStackTrace();
	}
	
	repository.shutDown();

	}
	
}