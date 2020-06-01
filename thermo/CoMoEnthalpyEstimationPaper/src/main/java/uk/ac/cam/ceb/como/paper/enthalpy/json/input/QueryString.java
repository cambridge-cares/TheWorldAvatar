package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

public class QueryString {
	
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
 
public static String getNumberOfSpeciesInOntoSpecies() {

	String query = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
			+ "SELECT ?g ?type "
			+ "WHERE { "
			+ "?g rdf:type ?type . "
			+ "FILTER((str(?type)='http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species') || (str(?type)='http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Species')) . "
			+ "}";

return query;

}
public static String getUniqueSpecies() {

	String query = "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
			+ "SELECT ?s ?species "
			+ "WHERE { "
			+ "?s ontocompchem:hasUniqueSpecies ?species . "
			+ "}";

return query;

}


}