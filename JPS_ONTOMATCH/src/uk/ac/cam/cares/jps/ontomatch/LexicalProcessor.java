package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/***
 * 
 * Agent that lexically process an ontology and save it in an pkl file A
 * necessary shared function used by several ElementMatcher Functionally it is a
 * servlet wrapper around the python program that actually does the job Input
 * from KG: IRIs of portions of KG to be matched Output to KG: preprocessed file
 * address to metadata store
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */

@WebServlet(urlPatterns = { "/ontologyProcessor" })

public class LexicalProcessor extends JPSHttpServlet {

	private static final long serialVersionUID = -5914984180067956570L;

	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject jo = requestParams;
		String afileIRI = null, saveAddress = null;

		try {
			afileIRI = jo.getString("ontologyIRI");
			saveAddress = jo.getString("saveAddress");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		JSONObject resultObj = new JSONObject();
		try {
			logger.info("Ontology processor agent: pre-process: " + afileIRI);
			/** call python ***/
			String[] paras = { afileIRI, saveAddress };
			String pyname = OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_LEXICALPROCESSOR);
			String[] results = AsyncPythonHelper.callPython(pyname, paras, LexicalProcessor.class);

			/** write to metadata store **/
			String serverUrl = OntomatchProperties.getInstance().getProperty(OntomatchProperties.KB_URL);

			String agent = serverUrl+"/" + request.getServletPath();
			List<String> topics = new ArrayList<String>();
			topics.add(afileIRI);
			MetaDataAnnotator.annotate(saveAddress, null, agent, true, topics);
			logger.info("Ontology processor agent: save to metadatastore about: " + saveAddress);

			/** write response: success/err **/
			if (results[0].toString().contains("success")) {
				resultObj.put("success", true);
			} else {
				resultObj.put("error", results[1].toString());
				//System.out.println(results[1].toString());
			    throw new Exception(results[1].toString());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return resultObj;
	}

	protected void testGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}
}
