package uk.ac.cam.cares.query;
/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 *
 */
public class QueryTemplate {

	/**
	 * 
	 * @param webLinIri
	 * @return the species IRIs that have given web link .
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
	 * @return the species iri, species registry id, atomic bond, and geometry
	 * 
	 */
	public static String getSpeciesRegistryIDAtomicBondAndGeometry() {
		
		String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>"
				+ "SELECT ?species ?crid ?atomicBond ?geometry "
				+ "WHERE { "
				+ "?species OntoSpecies:casRegistryID ?crid . "
				+ "?species OntoSpecies:hasAtomicBond ?atomicBond . "
				+ "?species OntoSpecies:hasGeometry ?geometry . "
				+ "} LIMIT 1";
		
		return query;
	}
	
}
