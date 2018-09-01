package uk.ac.ceb.como.molhub.bean;

public class QueryString {
	/**
	 * @author nk510
	 * @param atomName
	 *            name of literal that represents atom name from periodic table
	 * @param atomNumber
	 *            number of atoms which appear in a molecule for give atom name
	 * @return Query as a String. Result of that query should be list of all molecule names
	 *         which contain selected atom name and selected number of atoms.
	 */

	public static String getAllTriplesForPositiveLiteral(String atomName, int atomNumber) {

		String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>" 
		        + "PREFIX gc: <http://purl.org/gc/>"
				+ "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>" 
		        + "SELECT ?name "
				+ "WHERE { " 
		        + "?s ontochem:hasInitialization ?in . " 
				+ "?in gc:hasMoleculeProperty ?mp . "
				+ "?mp gc:hasName ?name ." 
				+ "?mp gc:hasMolecule ?molecule ." 
				+ "?molecule gc:hasAtom ?atom. "
				+ "?atom gc:isElement <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#" + atomName + ">."
				+ "?molecule gc:hasNumberOfAtoms ?n ." 
				+ "FILTER(str(?n)='" + atomNumber + "') ." + "}";

		return query;
	}

	/**
	 * @author nk510
	 * @param atomName
	 *            name of literal that represents atom name from periodic table
	 * @param atomNumber
	 *            number of atoms which appear in a molecule for give atom name
	 * @return Query as a String. Result of that query should be list of  all molecule names
	 *         which do not contain selected atom name and selected number of atoms.
	 */
	public static String getAllTriplesForNegativeLiteral(String atomName, int atomNumber) {

		String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>" 
		        + "PREFIX gc: <http://purl.org/gc/>"
				+ "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>" 
		        + "SELECT ?name "
				+ "WHERE { " 
		        + "?s ontochem:hasInitialization ?in . " 
				+ "?in gc:hasMoleculeProperty ?mp . "
				+ "?mp gc:hasName ?name ." 
				+ "FILTER NOT EXISTS { " 
				+ "?mp gc:hasMolecule ?molecule ."
				+ "?molecule gc:hasAtom ?atom. " 
				+ "?molecule gc:hasNumberOfAtoms ?n ."
				+ "?atom gc:isElement <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#" + atomName + ">."
				+ "FILTER(str(?n)='" + atomNumber + "') ." 
				+ "}" 
				+ "}";

		return query;
	}
	
/**
 * @param moleculeName  a name of molecule (for example: Cl 2 O 6).
 * @return Query as a string. Result of that query should be a list of molecule properties to be used in thermodynamic calculations. 
 */
public static String getAllTriplesMoleculeProperty(String moleculeName) {
		
		String query=
				    "PREFIX ontochem: <http://ontochem.theworldavatar.com/kb/OntoChem.owl#>"
					+"PREFIX gc: <http://purl.org/gc/>"
					+"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
					+"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
					+"SELECT (strafter(str(?g09), 'compchem/') AS ?uuid) ?levelOfTheory ?basisSetValue ?frequenciesSize ?frequenciesValue ?frequenciesUnit ?spinMultiplicityValue  ?coordinateX ?coordinateY ?coordinateZ ?massValue ?massUnit ?geometryTypeValue ?rotationalConstantsSize ?rotationalConstantsUnit ?rotationalConstantsValue ?rotationalSymmetryNumber ?programName ?programVersion ?runDate" 
					+"WHERE {" 
					+"{"
					+"?g09 ontochem:hasInitialization ?mn0 ." 
					+"?mn0 gc:hasMoleculeProperty ?mp0 ." 
					+"?mp0 gc:hasName "+ moleculeName +"." 
					+"}"
					+"UNION"
					+"{"
					+"?g09 ontochem:hasInitialization ?mn1 ." 
					+"?mn1 gc:hasMoleculeProperty ?mp1 ."  
					+"?mp1 gc:hasMolecule ?mol0 ."
					+"?mol0 gc:hasNumberOfAtoms ?atomNumber."
					+"?mol0 gc:hasAtom ?at0 ." 
					+"?at0 gc:isElement ?atomName ." 
					+"}"
					+"UNION"
					+"{"
					+"?g09 ontochem:hasInitialization ?mn2 ."
					+"?mn2 gc:hasParameter ?p1 ."
					+"?p1 rdf:type <http://ontochem.theworldavatar.com/kb/OntoChem.owl#LevelOfTheory> ."
					+"?p1 ontochem:hasLevelOfTheoryValue ?levelOfTheory ."
					+"?mn2 gc:hasParameter ?p2 ."
					+"?p2 rdf:type <http://purl.org/gc/BasisSet> ."
					+"?p2 gc:hasBasisSet ?basisSetValue ."
					+"}"
					+"UNION"
					+"{"
					+"?g09 gc:isCalculationOn ?go." 
					+"?go rdf:type gc:GeometryOptimization ."
					+"?go gc:hasMolecule ?mol1."
					+"?mol1 gc:hasAtom ?at1 ."
					+"?at1 gc:isElement ?atomName ." 
					+"?at1 gc:hasAtomCoordinateX ?x . ?x gc:hasValue ?coordinateX."
					+"?at1 gc:hasAtomCoordinateY ?y . ?y gc:hasValue ?coordinateY."
					+"?at1 gc:hasAtomCoordinateZ ?z . ?z gc:hasValue ?coordinateZ."
					+"?at1 gc:hasMass ?mass . ?mass gc:hasValue ?massValue ." 
					+"?mass gc:hasUnit ?massUnit ."
					+"}"
					+"UNION"
					+"{"
					+"?g09 gc:isCalculationOn ?go1." 
					+"?go1 rdf:type gc:GeometryOptimization ."
					+"?go1 gc:hasMolecule ?mol2."
					+"?mol2 ontochem:hasSpinMultiplicityValue ?spinMultiplicityValue ."
					+"}"
					+"UNION"
					+"{"
					+"?g09 gc:isCalculationOn ?go2." 
					+"?go2 rdf:type gc:VibrationalAnalysis." 
					+"?go2 gc:hasResult ?r ." 
					+"?r rdf:type gc:Frequency ."
					+"?r gc:hasVibrationCount ?frequenciesSize ."
					+"?r ontochem:hasFrequenciesValue ?frequenciesValue ."
					+"?r gc:hasUnit ?frequenciesUnit." 
					+"}"
					+"UNION"
					+"{" 
					+"?g09 gc:isCalculationOn ?go3." 
					+"?go3 a ontochem:GeometryType ."
					+"?go3 ontochem:hasGeometryTypeValue ?geometryTypeValue."
					+"}"
					+"UNION"
					+"{"
					+"?g09 gc:isCalculationOn ?go4."
					+"?go4 rdf:type ontochem:RotationalConstants ."
					+"?go4 ontochem:hasRotationalConstantsValue ?rotationalConstantsValue ."
					+"?go4 gc:hasUnit ?rotationalConstantsUnit ." 
					+"?go ontochem:hasRotationalConstantsCount ?rotationalConstantsSize ."
					+"}"
					+"UNION"
					+"{"
					+"?g09 gc:isCalculationOn ?go5."
					+"?go5 rdf:type ontochem:RotationalSymmetry ."
					+"?go5 ontochem:hasRotationalSymmetryNumber ?rotationalSymmetryNumber ."
					+"}"
					+"UNION"
					+"{"
					+"?g09 ontochem:hasEnvironment ?env ."
					+"?env ontochem:hasProgram ?programName ." 
					+"?env ontochem:hasProgramVersion ?programVersion ."
					+"?env ontochem:hasRunDate ?runDate ."
					+"}"
					+"}";
				
				return query;
	}
}
