package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.json.JSONArray;
import org.json.JSONObject;

public class JenaResultSetFormatter {

	/**
	 * returns CSV file where the first row is the header. The header contains the comma-separated SPARQL parameter names (without ?) as header names
	 * @param resultSet
	 * @return
	 */
	public static String convertToCSV(ResultSet resultSet) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ResultSetFormatter.outputAsCSV(os, resultSet);
		return new String(os.toByteArray());
	}
	
	/**
	 * returns official W3C JSON format as described in https://www.w3.org/TR/rdf-sparql-json-res/ <br>
	 * <br>
	 * example with one row in result set (one element in results-bindings JSON array):<br>
	 * { <br>
	 * "head": { "vars": [ "generation" , "emission" , "emissionvalue" , "emissionvaluenum" ] } ,<br>
     * "results": { "bindings": [<br>
     * {<br>
     *   "generation": { "type": "uri" , "value": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration" } ,<br>
     *   "emission": { "type": "uri" , "value": "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan" } ,<br>
     *   "emissionvalue": { "type": "uri" , "value": "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#v_CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan" } , <br>
     *   "emissionvaluenum": { "type": "literal" , "value": "15.75" } <br>
     * } <br>
     * ] } }<br>
	 * 
	 * @param resultSet
	 * @return
	 */
	public static String convertToJSONW3CStandard(ResultSet resultSet) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ResultSetFormatter.outputAsJSON(os, resultSet);
		return new String(os.toByteArray());
	}
	
	/**
	 * returns a list of simplified JSONObjects where each element represents a row in the resultSet and contains of key-value pairs <br>
	 * <br>
	 * example with one row (list element): <br>
	 * { "results": [<br>
	 * { <br>
	 * "generation":"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration",<br>
	 * "emission":"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan",<br>
	 * "emissionvalue":"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#v_CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan",<br>
	 * "emissionvaluenum":"15.75"<br>
	 * }<br>
	 * ]}
	 * 
	 * @param resultSet
	 * @return
	 */
	public static JSONObject convertToSimplifiedList(ResultSet resultSet) {
		String json = convertToJSONW3CStandard(resultSet);
		return convertToSimplifiedList(json);
	}
	
	/**
	 * see examples from {@link #convertToJSONW3CStandard(ResultSet)} for input and {@link #convertToSimplifiedList(ResultSet)} for output
	 * 
	 * @param resultJSONW3CStandard a query result string according the official W3C JSON format as described in https://www.w3.org/TR/rdf-sparql-json-res/
	 * @return
	 */
	public static JSONObject convertToSimplifiedList(String resultJSONW3CStandard) {
		JSONObject result = new JSONObject();
		
		JSONArray joarray = new JSONArray();
		result.put("results", joarray);
		
		JSONObject jo = new JSONObject(resultJSONW3CStandard);
		JSONArray array = jo.getJSONObject("results").getJSONArray("bindings");
		for (int i=0; i<array.length(); i++) {
			
			JSONObject row = array.getJSONObject(i);
			JSONObject simplifiedRow = new JSONObject();
			for (String current : row.keySet()) {
				String value =  row.getJSONObject(current).getString("value");
				simplifiedRow.put(current, value);
			}
			
			joarray.put(simplifiedRow);
		}
	
		return result;
	}
	
	/**
	 * Each String array in the result list represents the values of a single result row. The values in an array are ordered according
	 * to the input parameter keys. If a key is not found then the value <code>null</code> is set in the array.
	 * 
	 * @param resultJSONW3CStandard
	 * @param keys
	 * @return
	 */
	public static List<String[]> convertToListofStringArrays(String resultJSONW3CStandard, String... keys) {
		
		List<String[]> result = new ArrayList<String[]>();
		
		JSONObject jo = JenaResultSetFormatter.convertToSimplifiedList(resultJSONW3CStandard);
		JSONArray ja = jo.getJSONArray("results");
	
		for (int i=0; i<ja.length(); i++) {
			String[] array = new String[keys.length];
			JSONObject row = ja.getJSONObject(i);
			for (int j=0; j<keys.length; j++) {
				String value = row.optString(keys[j], null);
				array[j] = value;
			}
			result.add(array);
		}
		
		return result;
	}
	
	public static String[] getKeys(String resultJSONW3CStandard) {
		
		// "head": { "vars": [ "generation" , "emission" , "emissionvalue" , "emissionvaluenum" ] }
		JSONObject jo = new JSONObject(resultJSONW3CStandard);
		JSONArray ja = jo.getJSONObject("head").getJSONArray("vars");
		String[] result = new String[ja.length()];
		for (int i=0; i<ja.length(); i++) {
			result[i] = ja.getString(i);
		}
		
		return result;
	}
}
