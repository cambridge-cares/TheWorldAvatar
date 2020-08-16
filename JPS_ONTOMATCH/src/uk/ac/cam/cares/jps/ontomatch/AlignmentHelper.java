package uk.ac.cam.cares.jps.ontomatch;

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

//TODO: probably better make it into a Alignment class with the List<hashmap>
public class AlignmentHelper {
	public static JSONArray readAlignmentFileAsJSONArray(String iriOfAlignmentFile,String threshold) throws Exception {
		String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "SELECT ?entity1 ?entity2 " 
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
	
	public static void writeAlignment2RDF(List<Map> alignment) {
		//TODO: use existing python module
	}
	
	public static JSONArray scoreList2Json(List<Map> mlist){
		JSONArray jlist = new JSONArray();
		for(int i = 0; i<mlist.size();i++) {
            Map mcell = mlist.get(i);            
            JSONArray jo = new JSONArray();
            jo.put(mcell.get("entity1"));
            jo.put(mcell.get("entity2"));
            jo.put(mcell.get("measure"));
		    jlist.put(jo);
		}
	return jlist;
	}
	
	
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
}
