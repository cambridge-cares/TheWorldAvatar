package uk.ac.ceb.como.query;

/**
 * The Class QueryString.
 *
 * @author nk510 (caresssd@hermes.cam.ac.uk)
 * <p>The Class QueryString. Class implements methods for parametrised SPARQL queries.</p>
 */
public class QueryString {	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
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
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
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
 * @author NK510 (caresssd@hermes.cam.ac.uk)
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
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * @return the number of species in OntoKin
  */
 public static String getNumberOfSpeciesInOntoKin() {
	 
	 String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "PREFIX ontokin1: <http://theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "SELECT (count(?speciesIRI) as ?sum) "
	 		+ "WHERE { "
	 		+ "{"
	 		+ "?speciesIRI rdf:type ontokin:Species . "
	 		+ "}"
	 		+ "UNION "
	 		+ "{"
	 		+ "?speciesIRI rdf:type ontokin1:Species . "
	 		+ "}"
	 		+ "}"; 
	 		
	 
	 return query;
 }
	
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
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
  * @author NK510 (caresssd@hermes.cam.ac.uk)
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
  * @author NK510 (caresssd@hermes.cam.ac.uk)
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
 
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * @return the number of species in OntoKin that contains ONLY cabron (C) and hydrogen (H) atoms.
  */
 public static String getCabronHydrogenSpeciesInOntoKin() {
	 
	 String query = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
	 		+ "SELECT (count(?o) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type ontokin:Species . "
	 		+ "?s rdfs:label ?o . "
	 		+ "FILTER((REGEX(str(?o),'^C([1-9]*)H([1-9]*)$')) || (REGEX(str(?o),'^H([1-9]*)C([1-9]*)$'))) . " 
	 		+"}";
	 
	 return query;
 }
 
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * @return the number of species in OntoKin that contains ONLY cabron (C) and hydrogen (H) atoms.
  */
 public static String getCabronHydrogenOxygenSpeciesInOntoKin() {
	 
	 String query = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
	 		+ "SELECT (count(?o) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type ontokin:Species . "
	 		+ "?s rdfs:label ?o . "
	 		+ "FILTER((REGEX(str(?o),'^C([1-9]*)H([1-9]*)O([1-9]*)$')) || (REGEX(str(?o),'^C([1-9]*)O([1-9]*)H([1-9]*)$')) || (REGEX(str(?o),'^H([1-9]*)C([1-9]*)O([1-9]*)$')) || (REGEX(str(?o),'^H([1-9]*)O([1-9]*)C([1-9]*)$')) || (REGEX(str(?o),'^O([1-9]*)H([1-9]*)C([1-9]*)$')) ||  (REGEX(str(?o),'^O([1-9]*)H([1-9]*)C([1-9]*)$'))) . "            
	 		+"}";
	 
	 return query;
 } 
 
 /**
  * 
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * @return the number of nitrogen atoms in OntoKin 
  */
 public static String getNumberNitrogenSpeciesInOntoKin() {
	 
	 String query ="PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>  "
	 		+ "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> "
	 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
	 		+ "SELECT (count(?o) as ?sum) "
	 		+ "WHERE { "
	 		+ "?s rdf:type ontokin:Species . "
	 		+ "?s rdfs:label ?o . "
	 		+ "FILTER(CONTAINS(str(?o),'N2')) . "
	 		+ "}";
	 		
	 		return query;
 }
	
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * @return the number of chemical reactions that involve hydrocarbon species
  */
 public static String getNumberOfReactionsThatInvolveHydrocarbonSpecies() {
	 
	 String query="PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
		 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
		 		+ "PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#> "
		 		+ "SELECT (count(?s) as ?sum) "
		 		+ "WHERE { "
		 		+ "{"
		 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
		 		+ "?s reaction_mechanism:hasProduct ?p . "
		 		+ "?p rdfs:label ?o1 . "
		 		+ "FILTER((REGEX(str(?o1),'^C([1-9]*)H([1-9]*)$')) || (REGEX(str(?o1),'^H([1-9]*)C([1-9]*)$'))) . "
		 		+ "}"
		 		+ "UNION"
		 		+ "{"
		 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
		 		+ "?s reaction_mechanism:hasReactant ?r . "
		 		+ "?r rdfs:label ?o2 . "
		 		+ "FILTER((REGEX(str(?o2),'^C([1-9]*)H([1-9]*)$')) || (REGEX(str(?o2),'^H([1-9]*)C([1-9]*)$'))) . "

		 		+ "}"
		 		+ "}";
	 
	 return query;
 }
 
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * return the number of reactions that involce oxygen hydorcarbon species.
  */
 public static String getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies() {
	 
	 String query="PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
	 		+ "PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#> "
	 		+ "SELECT DISTINCT (count(?s) as ?sum) "
	 		+ "WHERE { "
	 		+ "{"
	 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
	 		+ "?s reaction_mechanism:hasProduct ?p . "
	 		+ "?p rdfs:label ?o1 . "
	 		+ "FILTER((REGEX(str(?o1),'^C([1-9]*)H([1-9]*)O([1-9]*)$')) || (REGEX(str(?o1),'^C([1-9]*)O([1-9]*)H([1-9]*)$')) || (REGEX(str(?o1),'^H([1-9]*)C([1-9]*)O([1-9]*)$')) || (REGEX(str(?o1),'^H([1-9]*)O([1-9]*)C([1-9]*)$')) || (REGEX(str(?o1),'^O([1-9]*)H([1-9]*)C([1-9]*)$')) ||  (REGEX(str(?o1),'^O([1-9]*)H([1-9]*)C([1-9]*)$')))   . "
	 		+ "}"
	 		+ "UNION "
	 		+ "{"
	 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
	 		+ "?s reaction_mechanism:hasReactant ?r . "
	 		+ "?r rdfs:label ?o2 . "
	 		+ "FILTER((REGEX(str(?o2),'^C([1-9]*)H([1-9]*)O([1-9]*)$')) || (REGEX(str(?o2),'^C([1-9]*)O([1-9]*)H([1-9]*)$')) || (REGEX(str(?o2),'^H([1-9]*)C([1-9]*)O([1-9]*)$')) || (REGEX(str(?o2),'^H([1-9]*)O([1-9]*)C([1-9]*)$')) || (REGEX(str(?o2),'^O([1-9]*)H([1-9]*)C([1-9]*)$')) ||  (REGEX(str(?o2),'^O([1-9]*)H([1-9]*)C([1-9]*)$')))  . "	
	 		+ "}"
	 		+ "}";
	 
	 return query;
 }
 
 /**
  * @author NK510 (caresssd@hermes.cam.ac.uk)
  * 
  * @return the number of reactions that involve nitrogen species.
  */
 public static String getNumberOfReactionsThatInvolveNitrogenSpecies() {
	 
	 String query="PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
	 		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
	 		+ "PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#> "
	 		+ "SELECT DISTINCT (count(?s) as ?sum) "
	 		+ "WHERE { "
	 		+ "{"
	 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
	 		+ "?s reaction_mechanism:hasProduct ?p . "
	 		+ "?p rdfs:label ?o1 . "
	 		+ "FILTER(CONTAINS(str(?o1),'N2')) . "
	 		+ "}"
	 		+ "UNION "
	 		+ "{"
	 		+ "?s rdf:type <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction> . "
	 		+ "?s reaction_mechanism:hasReactant ?r . "
	 		+ "?r rdfs:label ?o2 . "
	 		+ "FILTER(CONTAINS(str(?o2),'N2')) . "
	 		+ "}"
	 		+ "}";
	 
	 return query;
 }
 
 public static String getSpeciesIRIFromOntoCompChem() {
	 
		String query = "PREFIX gc: <http://purl.org/gc/>"
				+ "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>"
				+ "SELECT ?s  "
				+ "WHERE { "
				+ "?s rdf:type ?type . "
				+ "FILTER((str(?type)='http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#G16') ||(str(?type)='http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#G09')) . "
				+ "}";
		
		return query;
	}
 
 public static String getSpeciesIRIFromOntoKin() {
	 
	 String query= "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> "
		 		+ "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> "
		 		+ "PREFIX ontokin1: <http://theworldavatar.com/kb/ontokin/ontokin.owl#> "
		 		+ "SELECT ?s "
		 		+ "WHERE { "
		 		+ "{"
		 		+ "?s rdf:type ontokin:Species . "
		 		+ "}"
		 		+ "UNION "
		 		+ "{"
		 		+ "?s rdf:type ontokin1:Species . "
		 		+ "}"
		 		+ "}"; 
	 		
	 		return query;
 }
}