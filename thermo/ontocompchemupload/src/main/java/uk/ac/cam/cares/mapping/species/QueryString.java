package uk.ac.cam.cares.mapping.species;

public class QueryString {
	/**
	 * 
	 * @param casRegId the cas registry id for a species .
	 * @return the query string . 
	 */
	public static String getSpeciesIRI(String casRegId) {
		
		String queryString = "PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "					
				+ "SELECT DISTINCT ?species  "
				+ "WHERE { "
				+ "?species OntoSpecies:casRegistryID  '"+ casRegId+ "' . "
				+ "}";
	
	return queryString;
	
	}

}