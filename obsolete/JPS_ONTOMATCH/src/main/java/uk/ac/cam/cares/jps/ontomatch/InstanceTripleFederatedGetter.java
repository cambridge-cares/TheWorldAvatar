package uk.ac.cam.cares.jps.ontomatch;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/**
 * Agent gets related triples to an entity across JPS and DBP. For
 * Visualization/View only. Input from KG: related triples to an entity
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */

@WebServlet(urlPatterns = { "/federatedAttrs" })
public class InstanceTripleFederatedGetter extends JPSAgent {

	private static final long serialVersionUID = 7607478466081757161L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject result = new JSONObject();
		JSONObject jo = requestParams;
		String entityIRI = null;
		if (validateInput(requestParams)) {
		try {
			entityIRI = jo.getString("entityIRI");
			logger.info("InstanceTripleFederatedGetter:QUERY FOR ENTITY: " + entityIRI);
		} catch (JSONException e1) {
			throw new JPSRuntimeException(e1);
		}
		// String stubIRI =
		// "http://www.theworldavatar.com/kb/powerplants/Altbach_Coal_Power_Plant_Germany.owl#Altbach_Coal_Power_Plant_Germany";
		JSONArray resArr = performFederatedQuery(entityIRI);
		int tripleNumber = resArr.length();
		try {
			result.put("content", resArr);
			result.put("number", tripleNumber);
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			throw new JPSRuntimeException(e);
		}
		}
		return result;

	}

	/***
	 * query KG for related triples of entity then return as JSONArray
	 * 
	 * @param iri of entity
	 * @return
	 */
	public JSONArray performFederatedQuery(String iri) {
		String queryStrRemote = "PREFIX owl: <http://www.w3.org/2002/07/owl#> " + "SELECT distinct ?p ?o " + "WHERE {<"
				+ iri + "> owl:sameAs ?dbiri." + " SERVICE <http://dbpedia.org/sparql> " + " {?dbiri ?p ?o.}" + "}";
		System.out.println(queryStrRemote);

		String queryStrLocal = "SELECT distinct ?p ?o " + "WHERE {<" + iri + "> ?p ?o." + "}";

		JSONArray resArr = new JSONArray();
		try {
			OntModel model = JenaHelper.createModel(convertIRI(iri, false));
			ResultSet resultSetRemote = JenaHelper.query(model, queryStrRemote);
			ResultSet resultSetLocal = JenaHelper.query(model, queryStrLocal);

			String resultR = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetRemote);
			String resultL = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetLocal);
			String[] keys = JenaResultSetFormatter.getKeys(resultR);
			List<String[]> localListfromquery = JenaResultSetFormatter.convertToListofStringArrays(resultL, keys);
			List<String[]> remoteListfromquery = JenaResultSetFormatter.convertToListofStringArrays(resultR, keys);
			List<String[]> combined = new ArrayList<String[]>(localListfromquery);
			combined.addAll(remoteListfromquery);
			for (String[] paras : combined) {
				JSONObject resObj = new JSONObject();
				for (int idx = 0; idx < keys.length; idx++) {
					resObj.put(keys[idx], paras[idx]);
				}
				resArr.put(resObj);
			}
		} catch (Exception e) {
			StringWriter sw = new StringWriter();
			e.printStackTrace(new PrintWriter(sw));
			String exceptionAsString = sw.toString();
			logger.error(exceptionAsString);
		}
		return resArr;

	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty() || !requestParams.has("entityIRI")) {
			throw new BadRequestException();
		}
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
		paramTypes.put("entityIRI", CUSTOMVALUETYPE.URL);
		if (!ParamsValidateHelper.validateALLParams(requestParams, paramTypes)) {
			throw new BadRequestException();
		}
		return true;	
		}

	/** only neeeded for local testing **/
	public String convertIRI(String iri, boolean local2IRI) {
		if (local2IRI == true) {
			return iri.replace("localhost:3000", "www.theworldavatar.com");
		} else {
			return iri.replace("www.theworldavatar.com", "localhost:3000");
		}
	}

}
