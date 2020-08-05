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
Agent that lexical process an ontology and save it in an pkl file
Functionally it is a servlet wrapper around the python program that actually does the job 
Input: IRI of ontology
Output: metadata annotation
 * @author shaocong
 *
 */

@WebServlet(urlPatterns = { "/ontologyProcessor" })

public class OntologyProcessor extends JPSHttpServlet{
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Ontology processor agent");
		JSONObject jo = requestParams;
        //TODO:check if agent locator can find this script
		String afileIRI = null;
		String stubIRI = "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl";
		String stubAddress = "D:\\workwork\\ontoMatchFiles\\targetOntology.pkl";//output
		try {
			//afileIRI = jo.getString("ontologyIRI");
			//String saveAddress =  KeyValueMap.getInstance().get("targetOntology.pklfile");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		//TODO: call python, then save file
		try {
			String[] paras = {stubIRI, stubAddress};
			String[] results = AsyncPythonHelper.callPython("ontologyWrapper.py",paras,OntologyProcessor.class);
			String agent  = "http://www.theworldavatar.com/"+request.getServletPath();
			System.out.println(agent);
			List<String> topics = new ArrayList<String>();
			topics.add(stubIRI);
	    	MetaDataAnnotator.annotate(stubAddress, null, agent, true, topics);
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
}
