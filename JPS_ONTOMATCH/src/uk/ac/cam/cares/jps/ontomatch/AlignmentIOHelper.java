package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;

/***
 * Agent component that contains:
 * Input and output methods to render alignment(List<Map>) to knowledge graph and back 
 * 
 */
public class AlignmentIOHelper {
	/**
	 * Alignment knowledge graph(RDF) to JSONArray
	 * @param iriOfAlignmentFile
	 * @param threshold
	 * @return JSONArray
	 * @throws Exception
	 */
	public static JSONArray readAlignmentFileAsJSONArray(String iriOfAlignmentFile,Double threshold) throws Exception {
		String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "SELECT ?entity1 ?entity2 ?measure " 
				//+ "WHERE {?a ?p ?o."

				+ "WHERE {?cell alignment:entity1 ?entity1."
				+ "?cell  alignment:entity2 ?entity2 ."
				+"?cell alignment:measure ?measure."

				+ "FILTER (?measure >= "+threshold +" ) " //filtering gen 001 as it is slackbus
				+ "}";
		System.out.println(queryStr);
		JSONArray resArr = new JSONArray();
		List<String[]> resultListfromquery = null;
	
			OntModel model = JenaHelper.createModel(iriOfAlignmentFile);
			ResultSet resultSet = JenaHelper.query(model, queryStr);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	        System.out.println("reading alignment:");
			System.out.println(resultListfromquery.toString());
			for(String[] paras:resultListfromquery) {
				JSONObject resObj = new JSONObject();
				for(int idx = 0; idx<keys.length; idx++) {
					resObj.put(keys[idx], paras[idx]);
				}
				resArr.put(resObj);
				System.out.println(resObj);
			}

		return resArr;
	}
	

	/***
	 * alignment(List<Map>) to JsonArray
	 * @param mlist(alignment as java List)
	 * @return JSONArray
	 */
	public static JSONArray scoreList2Json(List<Map> mlist){
		JSONArray jlist = new JSONArray();
		for(int i = 0; i<mlist.size();i++) {
            Map mcell = mlist.get(i);            
            JSONArray ja = new JSONArray();
            ja.put(mcell.get("entity1"));
            ja.put(mcell.get("entity2"));
            ja.put(mcell.get("measure"));
		    jlist.put(ja);
		}
	return jlist;
	}
	
	/**
	 * JSONArray to alignment(List<Map>) 
	 * @param ja, JSONArray
	 * @return alignment(List<Map>)
	 */
	public static List<Map> Json2ScoreList(JSONArray ja) {
		List<Map> mlist = new ArrayList<Map>();
		for(int idx = 0; idx<ja.length();idx++) {
			JSONObject  jo = ja.getJSONObject(idx);
			Map<String, Object> mcell = new HashMap();
			mcell.put("entity1", jo.getString("entity1"));
			mcell.put("entity2", jo.getString("entity1"));
			mcell.put("measure", jo.getInt("measure"));
			mlist.add(mcell);
		}
		return mlist;
	}
	
	/***
	 * Alignment(List<Map>) to knowledge graph(RDF)
	 * @param alignment
	 * @param onto1
	 * @param onto2
	 * @param addr(local address)
	 * @throws IOException
	 */
	//TODO: json doesnt work,rewrite this in java then...or make into a file
	public static void writeAlignment2File(List<Map> alignment, String onto1, String onto2, String addr) throws IOException {
		//TODO: if use py, needs to convert Java list to JSON and JSON to py list
		String aIRI = ResourcePathConverter.convert(addr);
		String ja = scoreList2Json(alignment).toString();
		String[] paras = {aIRI, onto1, onto2, ja};
		String[] results = AsyncPythonHelper.callPython("alignment.py",paras,AlignmentIOHelper.class);
	    if(!results[0].contains("success")) {
	    	throw new IOException("Python execution failed, print error stack:  "+results[1]);
	    }
	}
}
