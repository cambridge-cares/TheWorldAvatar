package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/***
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
public class LexicalProcessor extends JPSAgent {

	private static final long serialVersionUID = -5914984180067956570L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject resultObj = new JSONObject();
		JSONObject jo = requestParams;
		String afileIRI = null, saveAddress = null;
		if (validateInput(requestParams)) {
		try {
			afileIRI = jo.getString("ontologyIRI");
			saveAddress = jo.getString("saveAddress");
		} catch (Exception e1) {
			e1.printStackTrace();
			throw new JPSRuntimeException(e1);
		}
		try {
			/** call python ***/
			String readAddress = ResourcePathConverter.convertToLocalPath(afileIRI);
			String[] paras = { readAddress, saveAddress };
			String pyname = OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_LEXICALPROCESSOR);
			String successFlag = OntomatchProperties.getInstance().getProperty(OntomatchProperties.SUCCESS_FLAG);
			JSONObject pyresult = AsyncPythonHelper.callPython(successFlag, pyname, paras, LexicalProcessor.class);
            System.out.println(pyresult.toString());
			/** write to metadata store **/
			String serverUrl = OntomatchProperties.getInstance().getProperty(OntomatchProperties.SERVER_URL);
			String callingAgent = serverUrl + "/" + request.getServletPath();
			List<String> topics = new ArrayList<String>();
			topics.add(afileIRI);
			MetaDataAnnotator.annotate(saveAddress, null, callingAgent, true, topics);
			// logger.info("Ontology processor agent: save to metadatastore about: " +
			// saveAddress);

			/** write response **/
			return pyresult;
		} catch (Exception e) {
			e.printStackTrace();
			throw new JPSRuntimeException(e);
		}
		}
		return resultObj;
	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty() || !requestParams.has("ontologyIRI") || !requestParams.has("saveAddress")) {
			throw new BadRequestException();
		}
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
		paramTypes.put("ontologyIRI", CUSTOMVALUETYPE.URL);
		paramTypes.put("saveAddress", CUSTOMVALUETYPE.PATH);
		if (!ParamsValidateHelper.validateALLParams(requestParams, paramTypes)) {
			throw new BadRequestException();
		}
		return true;
	}

}
