package uk.ac.cam.cares.jps.thermo.sparql;

import java.util.Properties;

import uk.ac.cam.cares.jps.thermo.manager.PropertiesManager;

public class QueryString {

	private static Properties jpsThermoProperties  = PropertiesManager.loadProperties(QueryString.class.getClassLoader().getResourceAsStream("jps_thermo.management.properties"));
	
	private static String ontoCompChemUri = jpsThermoProperties.getProperty("ontocompchem.kb.tbox.uri");	
	private static String ontoCompChemNS = jpsThermoProperties.getProperty("ontocompchem.kb.tbox.ns");
	
	/**
	 * 
	 * @author NK510
	 * @param uuid
	 * @return <p>String sparql query that contains given uuid as input parameter. 
	 * Result of this query are data that are input for thermo calculations implemented in Python.</p> 
	 * 
	 */
	public static String getAllTriplesForThermoCalculation(String uuid) {
		
		String query= "PREFIX "+ontoCompChemNS+": <"+ontoCompChemUri+">"
			+"PREFIX gc: <http://purl.org/gc/>"
				+"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
				+"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
				+"SELECT ?g09 ?speciesIRI ?moleculeName ?atomName ?atomNumber ?levelOfTheory ?basisSetValue ?frequenciesSize ?frequenciesValue ?frequenciesUnit ?spinMultiplicityValue  ?coordinateX ?coordinateY ?coordinateZ ?massValue ?massUnit ?geometryTypeValue ?rotationalConstantsSize ?rotationalConstantsUnit ?rotationalConstantsValue ?rotationalSymmetryNumber ?programName ?programVersion ?runDate " 
				+"WHERE { " 
				+"{ "
				+"?g09 "+ontoCompChemNS+":hasInitialization ?mn0 . " 
				+"?mn0 gc:hasMoleculeProperty ?mp0 . " 
				+"?mp0 gc:hasName ?moleculeName . " 
				+"} "
				+ "UNION"
				+ "{"
				+"?g09 "+ontoCompChemNS+":hasUniqueSpeciesIRI ?speciesIRI . "
				+ "}"
				+"UNION "
				+"{ "
				+"?g09 "+ontoCompChemNS+":hasInitialization ?mn1 . " 
				+"?mn1 gc:hasMoleculeProperty ?mp1 .  "
				+"?mp1 gc:hasMolecule ?mol0 . "
				+"?mol0 gc:hasNumberOfAtoms ?atomNumber. "
				+"?mol0 gc:hasAtom ?at0 . "
				+"?at0 gc:isElement ?atomName . " 
				+"}"
				+"UNION"
				+"{"
				+"?g09 "+ontoCompChemNS+":hasInitialization ?mn2 . "
				+"?mn2 gc:hasParameter ?p1 . "
				+"?p1 rdf:type <"+ontoCompChemUri+"LevelOfTheory> . "
				+"?p1 "+ontoCompChemNS+":hasLevelOfTheory ?levelOfTheory . "
				+"?mn2 gc:hasParameter ?p2 . "
				+"?p2 rdf:type <http://purl.org/gc/BasisSet> . "
				+"?p2 gc:hasBasisSet ?basisSetValue . "
				+"}"
				+"UNION "
				+"{ "
				+"?g09 gc:isCalculationOn ?go. " 
				+"?go rdf:type gc:GeometryOptimization . "
				+"?go gc:hasMolecule ?mol1. "
				+"?mol1 gc:hasAtom ?at1 . "
				+"?at1 gc:isElement ?atomName . " 
				+"?at1 gc:hasAtomCoordinateX ?x . ?x gc:hasValue ?coordinateX. "
				+"?at1 gc:hasAtomCoordinateY ?y . ?y gc:hasValue ?coordinateY. "
				+"?at1 gc:hasAtomCoordinateZ ?z . ?z gc:hasValue ?coordinateZ. "
				+"?at1 gc:hasMass ?mass . ?mass gc:hasValue ?massValue .  "
				+"?mass gc:hasUnit ?massUnit . "
				+"}"
				+ "UNION"
				+"{"
				+"?g09 gc:isCalculationOn ?go1. "
				+"?go1 rdf:type gc:GeometryOptimization . "
				+"?go1 gc:hasMolecule ?mol2. "
				+"?mol2 "+ontoCompChemNS+":hasSpinMultiplicity ?spinMultiplicityValue . "
				+"} "
				+"UNION"
				+"{"
				+"?g09 gc:isCalculationOn ?go2. "
				+"?go2 rdf:type gc:VibrationalAnalysis. " 
				+"?go2 gc:hasResult ?r . "
				+"?r rdf:type gc:Frequency . "
				+"?r gc:hasVibrationCount ?frequenciesSize . "
				+"?r "+ontoCompChemNS+":hasFrequencies ?frequenciesValue . "
				+"?r gc:hasUnit ?frequenciesUnit. "
				+"}"
				+"UNION"
				+"{"
				+"?g09 gc:isCalculationOn ?go3. " 
				+"?go3 a "+ontoCompChemNS+":GeometryType . "
				+"?go3 "+ontoCompChemNS+":hasGeometryType ?geometryTypeValue. "
				+"}"
				+"UNION"
				+"{"
				+"?g09 gc:isCalculationOn ?go4. "
				+"?go4 rdf:type "+ontoCompChemNS+":RotationalConstants . "
				+"?go4 "+ontoCompChemNS+":hasRotationalConstants ?rotationalConstantsValue . "
				+"?go4 gc:hasUnit ?rotationalConstantsUnit .  "
				+"?go "+ontoCompChemNS+":hasRotationalConstantsCount ?rotationalConstantsSize . "
				+"}"
				+"UNION"
				+"{"
				+"?g09 gc:isCalculationOn ?go5. "
				+"?go5 rdf:type "+ontoCompChemNS+":RotationalSymmetry . "
				+"?go5 "+ontoCompChemNS+":hasRotationalSymmetryNumber ?rotationalSymmetryNumber . "
				+"}"
				+"UNION"
				+"{"
				+"?g09 "+ontoCompChemNS+":hasEnvironment ?env . "
				+"?env "+ontoCompChemNS+":hasProgram ?programName . "  
				+"?env "+ontoCompChemNS+":hasProgramVersion ?programVersion . "
				+"?env "+ontoCompChemNS+":hasRunDate ?runDate . "
				+"} "
				+ "FILTER(str(?g09)='"+uuid+"')"
				+ "}";
		
		return query;
	}
	/**
	 * 
	 * @param gaussianIRI The Gaussian URI
	 * @return The unique species URI
	 */
	public static String getUniqueSpeciesUriSPARQL(String gaussianIRI) {
		
		/**
		 * @author NK510
		 * Comment: Namespace for property 'hasUniqueSpeciesIRI' should be changed to new one 
		 */
		String query = "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>" + 
				"SELECT  ?speciesUri " + 
				"WHERE { " + 
				"<"+gaussianIRI+"> ontocompchem:hasUniqueSpecies ?speciesUri . " +
				"}";
		
		return query ;
	}
	
public static String getOntoSpeciesEnthalpyAndTemperatureSPARQL(String speciesUri) {
		
	    
		String query = "PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>" + 
				"SELECT  (str(?enthalpy_value) AS ?enthalpyValue)  (str(?temp_value) AS ?tempValue)  " + 
				"WHERE {" + 
				"<"+ speciesUri +"> OntoSpecies:hasStandardEnthalpyOfFormation ?se ." + 
				"?se OntoSpecies:value ?enthalpy_value ." + 
				"?se OntoSpecies:hasReferenceTemperature ?temp ." + 
				"?temp OntoSpecies:value ?temp_value ." + 
				"}";
		
		return query;
	}


}