package uk.ac.cam.cares.ebr.query;

import java.util.LinkedList;

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
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param localHostSparqlEndPoint the localhost sparql endpoint.
	 * @param claudiusServerSparqlEndPoint the remote repository sparql endpoint.
	 * @param query the query string.
	 * @return the set of sparql results.
	 * @throws Exception 
	 */
	public LinkedList<NISTSpeciesId> runFederatedSPARQL(String localHostSparqlEndPoint, String claudiusServerSparqlEndPoint, String query) throws Exception {
		
	LinkedList<NISTSpeciesId> nistSpeciesIdList = new LinkedList<NISTSpeciesId>();
		
    Repository repository = FedXFactory.newFederation()
    		
    		/**
    		 * 
    		 * @author NK510 
    		 * a sparql endpoint on localhost
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
	 * Returns a result of federated sparql query via ontospecies repositories stored on local host and on Claudius server.
	 * 
	 */
	
	TupleQuery tq = conn.prepareTupleQuery(query);
	
	try{
		
	TupleQueryResult tqRes = tq.evaluate();
	
	while (tqRes.hasNext()) {
				
	BindingSet bSet = tqRes.next();				
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * Stores query results into NISTSpeciesId bean: species identifier, cas reg number, atomic bond, geometry, enthalpy of formation, scf energy, zero point energy.
				 * 
				 */		
				
				NISTSpeciesId nistSpeciesId = new NISTSpeciesId(
						bSet.getValue("species").stringValue(), 
						bSet.getValue("crid").stringValue(), 
						bSet.getValue("atomicBond").stringValue(), 
						bSet.getValue("geometry").stringValue(),
						bSet.getValue("enthalpyOfFormationValue").stringValue(),
						bSet.getValue("scfEnergyValue").stringValue(),
						bSet.getValue("zeroEnergyValue").stringValue());
				
				nistSpeciesIdList.add(nistSpeciesId);
	}
	
	}catch(TupleQueryResultHandlerException e) {
	
		e.printStackTrace();		
	}
	
	conn.close();
	
	}catch(RepositoryException e) {
	
		e.printStackTrace();
	}
    
	repository.shutDown();	

	return nistSpeciesIdList;
	}	
	
	public static void runFederatedSPARQLTest(String localHostSparqlEndPoint, String claudiusServerSparqlEndPoint, String query) throws Exception {
		
		
			
	    Repository repository = FedXFactory.newFederation()
	    		
	    		/**
	    		 * 
	    		 * @author NK510 
	    		 * a sparql endpoint on localhost
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
		 * Returns a result of federated sparql query via ontospecies repositories stored on local host and on Claudius server.
		 * 
		 */
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		
		try{
			
		TupleQueryResult tqRes = tq.evaluate();
		
		
		while (tqRes.hasNext()) {
					
					BindingSet bSet = tqRes.next();
					
					
					/**
					 * 
					 * @author NK510 (caresssd@hermes.cam.ac.uk)
					 * Stores query results into NISTSpeciesId bean: species identifier, cas reg number, atomic bond, geometry.
					 * 
					 */
					
					System.out.println(bSet.getValue("species").stringValue()+ " " + bSet.getValue("crid").stringValue() +" " + 
							bSet.getValue("atomicBond").stringValue() + " " +
							bSet.getValue("geometry").stringValue() + " "+bSet.getValue("enthalpyOfFormationValue").stringValue() +" "+bSet.getValue("scfEnergyValue").stringValue() +" " + bSet.getValue("zeroEnergyValue").stringValue());
					
					System.out.println("onto species iri: "+ bSet.getValue("species").stringValue() + " ontocompchem iri: " + bSet.getValue("compchemspecies").stringValue());
					
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
	
}