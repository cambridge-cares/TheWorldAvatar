package uk.ac.cam.cares.jps.base.query.fed;

import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

/**
 * A common interface for different engines / implementations for executing federated queries. 
 *
 */
public interface FederatedQueryInterface {
	
	/**
	 * Executes the SPARQL query as federated query and returns the result set as JSON array string. 
	 * Each element of the JSON array represents a row in the result set and consists of key-value pairs. 
	 * Example of a JSON element:
	 * <pre>
	 * {"generation":"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration",<br>
	 * "emission":"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan",<br>
	 * "emissionvalue":"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#v_CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan",<br>
	 * "emissionvaluenum":"15.75"}
	 * </pre>
	 * The above format is a simplified version of the official W3C JSON format 
	 * as described in {@link https://www.w3.org/TR/rdf-sparql-json-res/}. 
	 * Concerning the above example in the official W3C JSON format see 
	 * {@link JenaResultSetFormatter#convertToJSONW3CStandard(org.apache.jena.query.ResultSet)}.
	 * 
	 * @param sparql 
	 * @return query result set 
	 * @throws Exception
	 */
	public String executeFederatedQuery(String sparql);
}
