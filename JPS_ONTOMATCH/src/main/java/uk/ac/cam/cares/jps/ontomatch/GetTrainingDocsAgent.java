package uk.ac.cam.cares.jps.ontomatch;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/***
 * 
 * Agents that scrap training documents later used by topic model agent Output
 * to KG: metadata annotation of docs location
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class GetTrainingDocsAgent extends JPSAgent {

	private static final long serialVersionUID = 1L;

	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		logger.info("GetTraingDocs agent");
		JSONObject jo = requestParams;
		JSONObject result = new JSONObject();
		String savePath = "";
		// read parameters
		if (validateInput(requestParams)) {

		try {
			savePath = jo.getString("savePath");
		} catch (JSONException e1) {
			throw new JPSRuntimeException(e1);

		}

		try {
			/**** call scrapping scirpt ******/
			String scrappingScriptDir = "";
			String[] cmds = { savePath };
			AsyncPythonHelper.callCommandFrom(cmds, scrappingScriptDir);
			/*** write to metadata score ******/
			String serverPath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.SERVER_URL);
			String agent = serverPath + request.getServletPath();
			String IRI = serverPath + "/topicModelAgent";
			List<String> topics = new ArrayList<String>();
			topics.add(IRI);
			MetaDataAnnotator.annotate(savePath, null, agent, true, topics);
		} catch (Exception e) {
			throw new JPSRuntimeException(e);

		}
		}
		return result;
	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty() || !requestParams.has("savePath")) {
			throw new BadRequestException();
		}
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
		paramTypes.put("savePath", CUSTOMVALUETYPE.PATH);
		if (!ParamsValidateHelper.validateALLParams(requestParams, paramTypes)) {
			throw new BadRequestException();
		}
		return true;
	}

}
