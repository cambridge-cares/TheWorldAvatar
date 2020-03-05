package uk.ac.cam.cares.ebr;

import uk.ac.cam.cares.ebr.query.FederatedQuery;
import uk.ac.cam.cares.ebr.query.QueryTemplate;
import uk.ac.cam.cares.ebr.tools.CSVGenerator;



/**
 * @author NK510 (caressd@hermes.cam.ac.uk)
 * 
 * 
 * Generates csv file that represents information about target species.
 *
 */
public class App {
	
	static String localhostUrl = "http://localhost:8080/rdf4j-server/repositories/ontospecieskb";	
	static String claudiusUrl = "http://theworldavatar.com/rdf4j-server/repositories/ontospecieskb";
	static String csvFilePath = "C:\\Users\\NK\\Documents\\species_test_2.csv";	
	
	static FederatedQuery fq = new FederatedQuery();
	static CSVGenerator csvGenerator = new CSVGenerator(); 
//  static RepositoryManager repositoryManager = new RepositoryManager();
	
    public static void main( String[] args ) throws Exception {
    	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Uploads owl files into ontospecieskb repository stored on localhost and generates context in rdf4j repository.
      * 
      */

//	  repositoryManager.getUploadOwlFiles("http://localhost:8080/rdf4j-server/repositories/ontospecieskb", "C:\\Users\\NK\\Documents\\species-abox-feroz\\species-aboxes", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#");
	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Federated query via given sparql endpoints. Generates csv file.
      * 
      */
csvGenerator.generateCSVFile(fq.runFederatedSPARQL(localhostUrl,claudiusUrl,QueryTemplate.getSpeciesRegistryIDAtomicBondAndGeometry()), csvFilePath);

}

}