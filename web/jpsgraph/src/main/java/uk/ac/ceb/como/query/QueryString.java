package uk.ac.ceb.como.query;

/**
 * The Class QueryString.
 *
 * @author nk510
 * <p>The Class QueryString. Class implements methods for parametrised SPARQL queries.</p>
 */
public class QueryString {	

	
	/**
	 * 
	 * @return number of Gaussian calculations in OntoCompChem
	 */
	public static String getNumberOfGaussianCalculations() {

		
		String query = "PREFIX gc: <http://purl.org/gc/>"
				+ "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>"
				+ "SELECT (count(?g) as ?sum) "
				+ "WHERE { "
				+ "?g rdf:type ?type . "
				+ "FILTER((str(?type)='http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#G16') ||(str(?type)='http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#G09')) . "
				+ "}"; 	
				

		return query;
	}
	
	/**
	 * 
	 * @return the number of species in OntoSpecies
	 */
	public static String getNumberOfSpeciesInOntoSpecies() {

		String query = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
				+ "SELECT (count(?g) AS ?sum) "
				+ "WHERE { "
				+ "?g rdf:type ?type . "
				+ "FILTER((str(?type)='http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species') || (str(?type)='http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Species')) . "
				+ "}"; 	
				

		return query;
	}
/**
 * 
 * @return the number of reaction mechanisms in OntoKin
 */
 public static String getNumberOfReactionMechanisms() {
	 
	 String query ="PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX ontokin: <http://theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "SELECT (count(?s) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type ontokin:ReactionMechanism ."
	 		+ "}";
	 		
	 		return query;
 }
 
 /**
  * 
  * @return the number of species in OntoKin
  */
 public static String getNumberOfSpeciesInOntoKin() {
	 
	 String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX ontokin: <http://theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "SELECT (count(?s) AS ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type ontokin:Species . "
	 		+ "}";
	 
	 return query;
 }
	
 /**
  * 
  * @return the number of chemical reactions in OntoKin.
  */
 public static String getNumberOfChemicalReactionsInOntoKin() {
	 
	 String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "SELECT DISTINCT (count(?s) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
	 		+ "}";
	 
	 return query;
 }
 
 /**
  * 
  * @return the number of agents in OntoAgent. 
  * 
  */
 public static String getNumberOfOntoAgents() {
	 
	 String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "SELECT DISTINCT (count(?a) as ?sum) "
	 		+ "WHERE { "
	 		+ "?a rdf:type <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service> . "
	 		+ "}";
	 
	 return query;
 }
 
 /**
  * 
  * @return the number of synonyms in OntoSpecies knowledge base
  */
 public static String getNumberOfSynonymsInOntoSpecies() {
	 
	 String query = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> "
	 		+ "SELECT (count(?label) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s  skos:altLabel ?label . "
	 		+ "}";
	 
	 return query;
 }
	
}