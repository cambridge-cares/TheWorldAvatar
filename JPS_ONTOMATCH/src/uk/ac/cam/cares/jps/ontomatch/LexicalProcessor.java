package uk.ac.cam.cares.jps.ontomatch;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/***
 * 
Agent that lexically process an ontology and save it in an pkl file
A necessary shared function used by several ElementMatcher
Functionally it is a servlet wrapper around the python program that actually does the job 
Input: IRI of ontology
Output: metadata annotation(pointing to filename.pkl)
 * @author shaocong
 *
 */

@WebServlet(urlPatterns = { "/ontologyProcessor" })

public class LexicalProcessor extends JPSHttpServlet{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5914984180067956570L;

	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Ontology processor agent");
		JSONObject jo = requestParams;
		String afileIRI = null, saveAddress = null;

		//TODO: Used for simpletesting, comment out and move to JUNIT later
		String stubIRI = "D://workwork//testFiles//ontologies/PowerPlant.owl";
		String stubAddress = "D://workwork/ontoMatchFiles/targetOntology.pkl";
		try {
			afileIRI = jo.getString("ontologyIRI");
			saveAddress = jo.getString("saveAddress");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		//TODO: call python, then save file
		try {
			String[] paras = {afileIRI, saveAddress};
			String[] results = AsyncPythonHelper.callPython("ontologyWrapper.py",paras,LexicalProcessor.class);
			String agent  = "http://www.theworldavatar.com/"+request.getServletPath();
			System.out.println(agent);
			List<String> topics = new ArrayList<String>();
			topics.add(afileIRI);
	    	MetaDataAnnotator.annotate(saveAddress, null, agent, true, topics);
			System.out.println("result: ");
			System.out.println(results[0].toString());
			System.out.println("err: ");
			System.out.println(results[1].toString());
			if(results[0].toString().contains("success")) {
			resultObj.put("success", true);
			} else {
				resultObj.put("error", results[1].toString());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}
}
