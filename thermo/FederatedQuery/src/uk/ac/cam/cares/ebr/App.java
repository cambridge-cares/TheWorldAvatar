package uk.ac.cam.cares.ebr;

import java.util.ArrayList;
import java.util.LinkedList;

import uk.ac.cam.cares.ebr.query.FederatedQuery;
import uk.ac.cam.cares.ebr.query.QueryTemplate;
import uk.ac.cam.cares.ebr.repository.RepositoryManager;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

/**
 * @author NK510 (caressd@hermes.cam.ac.uk)
 * 
 * 
 * Generates csv file that represents information about target species.
 *
 */

public class App {
	
	static String localhostUrlOntoSpecies = "http://localhost:8080/rdf4j-server/repositories/ontospecies";
	static String claudiusUrlOntoCompChem = "http://theworldavatar.com/rdf4j-server/repositories/ontocompchem"; //http://localhost:8080/rdf4j-server/repositories/ontocompchem http://theworldavatar.com/rdf4j-server/repositories/ontospecies
	static String csvFilePath = "C:\\Users\\NK\\Documents\\species_test_8.csv";
	
	static FederatedQuery fq = new FederatedQuery();
	
    static RepositoryManager repositoryManager = new RepositoryManager();
	
    public static void main( String[] args ) throws Exception {
    	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Uploads owl files into ontospecies repository stored on localhost and generates context in rdf4j repository.
      * 
      * 
      */

	repositoryManager.getUploadOwlFiles("http://localhost:8080/rdf4j-server/repositories/ontospecies", "C:\\Users\\NK\\Documents\\species-abox-feroz\\owl-Feroz\\missing-21-species\\owl\\53130-19-1", "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#");
	
     /**
      * 
      * @author NK510 (caresssd@hermes.cam.ac.uk)
      * Federated query via given sparql endpoints. Generates csv file.
      * 
      */    
// FederatedQuery.runFederatedSPARQLTest(localhostUrlOntoSpecies,claudiusUrlOntoCompChem,QueryTemplate.getSpeciesRegistryIDAtomicBondAndGeometryScfZeroEnergy());    

    }	
}