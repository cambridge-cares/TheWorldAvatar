package uk.ac.cam.cares.ebr;

import uk.ac.cam.cares.query.FederatedQuery;
import uk.ac.cam.cares.query.QueryTemplate;

/**
 * 
 * Hello world!
 *
 */
public class App {
	
    public static void main( String[] args ) throws Exception {
    
     System.out.println( "Hello World!" );
     
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Uploads owl files into ontospecieskb repository stored on localhost.
      * 
      */
//     RepositoryManager repositoryManager = new RepositoryManager();
//	   repositoryManager.getUploadOwlFiles("http://localhost:8080/rdf4j-server/repositories/ontospecieskb", "C:\\Users\\NK\\Documents\\species-abox-feroz\\species-aboxes", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#");
	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Federated query via given sparql endpoints, and query string.
      * 
      */
 FederatedQuery.runFederatedSPARQL("http://localhost:8080/rdf4j-server/repositories/ontospecieskb","http://theworldavatar.com/rdf4j-server/repositories/ontospecieskb",QueryTemplate.getSpeciesRegistryIDAtomicBondAndGeometry());
	
    }
}