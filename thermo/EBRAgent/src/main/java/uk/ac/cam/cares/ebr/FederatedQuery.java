package uk.ac.cam.cares.ebr;


import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;

import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;

/**
 * 
 * @author NK510
 * Federated SPARQL query via localhost and remote repository stored on Claudius server.
 *
 */
public class FederatedQuery {
	
	public static void runFederatedSPARQLOnDbpediaWikipedia() throws Exception {
	
    Repository repository = FedXFactory.newFederation()
    		/**
    		 * 
    		 * @author NK510 
    		 * Sparql endpoint on localhost for ontocompchem knowledge base
    		 * 
    		 */
	    .withSparqlEndpoint("http://localhost:8080/rdf4j-server/repositories/ontocompchem")
	    /**
	     * 
	     * @author NK510
	     * Sparql endpoint on Caludius server for ontospecies knowledge base.
	     * 
	     */
        .withSparqlEndpoint("http://theworldavatar.com/rdf4j-server/repositories/ontospecieskb")        
		.create();
			
	try (RepositoryConnection conn = repository.getConnection()) {
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Returns a list of instances of G16 (G09) and species IRI from "ontocompchem" repository that has web link name "species_3_weblink" stored in "ontospecieskb" repository on Claudius.
	 */
	String query = 
				"SELECT distinct ?s ?speciesIri WHERE { "				
				+ "?s <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#hasUniqueSpeciesIRI> ?speciesIri."										
				+ "?speciesIri <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasWebLink> <http://www.theworldavatar.com/kb/ontospecies/ontospecies.owl#species_3_weblink> ." 
				+ "}";
				
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		
		try (TupleQueryResult tqRes = tq.evaluate()) {


			int count = 0;
			
			while (tqRes.hasNext()) {
				BindingSet b = tqRes.next();
				System.out.println(b);
				count++;
			}

			System.out.println("Results: " + count);
		}
	}
	
	repository.shutDown();
	
	}
}
