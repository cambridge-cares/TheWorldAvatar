package uk.ac.cam.cares.ebr.query;
/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 *
 */
public class QueryTemplate {

	/**
	 * 
	 * @param webLinkIri
	 * @return the species IRIs that have given web link . This is a test federated query and it is not use in generating csv file.
	 */
	public static String getSpeciesIriWtihGivenWebLink(String webLinkIri) {
		
		String query = 
				"SELECT distinct ?s ?speciesIri "
				+ "WHERE { "  
				+ "?s <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#hasUniqueSpeciesIRI> ?speciesIri . "
				+ "?speciesIri <"+ webLinkIri+"> . "  
				+ "}";
		
		return query ;
	}
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the species iri, species registry id, atomic bond, geometry, scf energy, zero-point energy.
	 * 
	 */
	public static String getSpeciesRegistryIDAtomicBondAndGeometryScfZeroEnergy() {
		
		String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
				+ "PREFIX gc: <http://purl.org/gc/> "
				+ "SELECT DISTINCT ?species ?compchemspecies ?crid ?atomicBond ?geometry ?enthalpyOfFormationValue ?scfEnergyValue ?zeroEnergyValue "
				+ "WHERE { "
				+ "?species OntoSpecies:casRegistryID ?crid . "
				+ "?species OntoSpecies:hasAtomicBond ?atomicBond . "
				+ "?species OntoSpecies:hasGeometry ?geometry . "
				+ "?species OntoSpecies:hasStandardEnthalpyOfFormation ?enthalpy . "
				+ "?enthalpy OntoSpecies:value ?enthalpyOfFormationValue ."
				+ "?compchemspecies ontocompchem:hasUniqueSpecies ?species . "
				+ "?compchemspecies gc:isCalculationOn ?scfEnergy . "
				+ "?scfEnergy a ontocompchem:ScfEnergy . "
				+ "?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy . "
				+ "?scfElectronicEnergy gc:hasValue ?scfEnergyValue . "
				+ "?compchemspecies gc:isCalculationOn ?zeroEnergy . "
				+ "?zeroEnergy a ontocompchem:ZeroPointEnergy . "
				+ "?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy . "
				+ "?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue . "
				+ "}";
		
		return query;
	}
}