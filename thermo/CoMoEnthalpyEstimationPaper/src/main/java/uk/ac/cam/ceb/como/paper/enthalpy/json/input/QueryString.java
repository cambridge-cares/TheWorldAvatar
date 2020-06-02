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

}