package uk.ac.cam.cares.jps.ontomatch;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;

/***
 * 
Agent that performs element level matching, four types can be chosen:
string, word, domain, value
Functionally it is a servlet wrapper around the python programs that actually does the job 
Input: IRI of ontology
Output: IRI to alignmentFile
 * @author shaocong
 */
@WebServlet(urlPatterns = { "/elementMatcher" })
public class ElementMatcher extends JPSHttpServlet{
	public enum MATCHERTYPE {
	    DOMAIN, STRING,WORD, VALUE
	}
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Topic model agent");
		JSONObject jo = requestParams;
		String savePath = null, matchMethod = null, targetOnto = null, sourceOnto = null,modelPath = null, dictPath = null;
		MATCHERTYPE type = MATCHERTYPE.STRING;
		String stubSavePath = "";
		String stubTgt = "";
		String stubSrc = "";
		String stubMM = "matchSerial";
		String stubModelPath = null;
		String stubDictPath = null;
		try {
			//savePath = jo.getString("alignmentFileAddress");
			//matchMethod = jo.getString("matchMethod");
			//targetOnto = jo.getString("targetOntoIRI");
			//sourceOnto = jo.getString("sourceOntoIRI");
			//type = MATCHERTYPE.valueOf(jo.getString("matcherType"));
			if(type == MATCHERTYPE.DOMAIN) {
				modelPath = jo.getString("modelAddress");
				dictPath = jo.getString("dictAddress");

			}
			//TODO:get parameter to get location from the target and source pkl 
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		//TODO: call python, then save file
		try {
			String[] results = null;
			String matcherLocation = getMatcherScriptName(type);
			if(type == MATCHERTYPE.DOMAIN) {
			String[] paras = {stubSavePath, stubTgt, stubSrc, stubMM, modelPath, dictPath};
			results = AsyncPythonHelper.callPython(matcherLocation,paras,OntologyProcessor.class);
			}
			String[] paras = {stubSavePath, stubTgt, stubSrc, stubMM};
			results = AsyncPythonHelper.callPython(matcherLocation,paras,OntologyProcessor.class);
			System.out.println("result: ");
			System.out.println(results[0].toString());
			System.out.println("err: ");
			System.out.println(results[1].toString());
			resultObj.put("success", true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}
	
	private String getMatcherScriptName(MATCHERTYPE matcherType) throws Exception {
		switch(matcherType) {
		case STRING:
			return "matcher/StringMatcher.py";
		case WORD:
			return "matcher/WordMatcher.py";
		case DOMAIN:
			return "matcher/DomainMatcher.py";	
		default:
			throw new Exception("Matcher Type not Supported");
		} 
		
		
	}
}
