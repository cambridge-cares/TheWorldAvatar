package uk.ac.cam.cares.ebr;


import java.util.LinkedList;


import uk.ac.cam.cares.query.FederatedQuery;
import uk.ac.cam.cares.query.QueryTemplate;

import uk.ac.cam.cares.tools.SpeciesGenerator;
import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

/**
 * 
 * Hello world!
 *
 */
public class App {
	
	static String localhostUrl = "http://localhost:8080/rdf4j-server/repositories/ontospecieskb";
	static String claudiusUrl = "http://theworldavatar.com/rdf4j-server/repositories/ontospecieskb";
	
	static LinkedList<NISTSpeciesId> linkedSpeciesSet =new LinkedList<NISTSpeciesId>();	
	
	static FederatedQuery fq = new FederatedQuery();	
	static SpeciesGenerator sg = new SpeciesGenerator();
	
    public static void main( String[] args ) throws Exception {
    
     System.out.println( "Hello World!" );
     
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Uploads owl files into ontospecieskb repository stored on localhost and generates context in rdf4j repository.
      * 
      */
//    RepositoryManager repositoryManager = new RepositoryManager();
//	  repositoryManager.getUploadOwlFiles("http://localhost:8080/rdf4j-server/repositories/ontospecieskb", "C:\\Users\\NK\\Documents\\species-abox-feroz\\species-aboxes", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#");
	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Federated query via given sparql endpoints.
      * 
      */
     
linkedSpeciesSet.addAll(fq.runFederatedSPARQL(localhostUrl,claudiusUrl,QueryTemplate.getSpeciesRegistryIDAtomicBondAndGeometry()));

sg.generateSpeciesNameFromGeometryRepresentation(linkedSpeciesSet);

System.out.println("Bond connections: ");

for(NISTSpeciesId speciesId: linkedSpeciesSet) {
	
System.out.println(speciesId.getBond());

}


//CSVGenerator.generateCSVFile(linkedSpeciesSet, csvFilePath);


}
    
}