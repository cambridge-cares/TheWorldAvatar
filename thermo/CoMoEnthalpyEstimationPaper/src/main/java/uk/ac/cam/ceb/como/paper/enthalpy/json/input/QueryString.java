package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

public class QueryString {
	
	/**
	 * 
	 * @param casRegID tje cas registry id. The query string is used in federated query.
	 * @return tuples that contain ontocompchem iri and ontospecies iri.
	 */
public static String getSpecies(String casRegID) {
		
		String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "				
				+ "SELECT DISTINCT ?compchemspecies ?species "
				+ "WHERE { "
				+ "?compchemspecies ontocompchem:hasUniqueSpecies ?species . "					
				+ "?species OntoSpecies:casRegistryID '" + casRegID+ "' . "						
				+ "}";
		
		return query;
	}

/**
 * 
 * @param casRegID the cas registry id. The query string is used in federated query.
 * @return tuples that contain level of theory, ontocompchem species iri, onto species iri.  
 */
public static String getSpeciesIRIWithLevelOfTheory(String casRegID) {
	
	String query ="PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>"
			+ "PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
			+ "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
			+ "PREFIX gc: <http://purl.org/gc/> "
			+ "SELECT ?levelOfTheory  ?compchemspecies ?species "
			+ "WHERE { "
			+ "?compchemspecies ontocompchem:hasInitialization ?initialization . "
			+ "?initialization rdf:type ontocompchem:InitializationModule . "
			+ "?initialization gc:hasParameter ?parameter . "
			+ "?parameter rdf:type ontocompchem:LevelOfTheory . "
			+ "?parameter ontocompchem:hasLevelOfTheory ?levelOfTheory . "
			+ "?compchemspecies ontocompchem:hasUniqueSpecies ?species . "
			+ "?species OntoSpecies:casRegistryID '" + casRegID+ "' . "
			+ "}";
	 
	return query;
}

}