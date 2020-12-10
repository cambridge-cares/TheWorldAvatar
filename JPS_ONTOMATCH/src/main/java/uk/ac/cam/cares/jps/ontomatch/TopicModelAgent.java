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

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/***
 * 
 * Agents that generates a topic model from a list of training documents
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-11-25
 */
@WebServlet(urlPatterns = { "/topicModelAgent" })
public class TopicModelAgent extends JPSAgent{

	private static final long serialVersionUID = -4621180516256485859L;
	private String venvname = OntomatchProperties.getInstance().getProperty(OntomatchProperties.VENV_NAME);
	private AsyncPythonHelper pyHelper =  AsyncPythonHelper.getInstance(venvname);
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        JSONObject result = new JSONObject();
		if (validateInput(requestParams)) {
		logger.info("Topic model agent");
		JSONObject jo = requestParams;
		String docsPath = "";
		String modelLocation = "";
		String dictionaryLocation = "";
		String corpusLocation = "";
		//read parameters
		try {
			modelLocation = jo.getString("modelLocation");
			dictionaryLocation = jo.getString("dictionaryLocation");
			corpusLocation = jo.getString("corpusLocation");
		} catch (JSONException e1) {
			throw new JPSRuntimeException(e1);
		}
		try {//get document save address by querying metadata annotator
			String serverPath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.SERVER_URL);
		    String docPath = queryDocsAddressfromMetaData(serverPath+request.getServletPath());
		}catch(Exception e) {
			throw new JPSRuntimeException(e);
		}
        
		try {
			String[] params = {corpusLocation, dictionaryLocation, docsPath, modelLocation}; 
			String[] prints = pyHelper.callPython("modelTopic.py",params, TopicModelAgent.class);
			List<String> topics = new ArrayList<String>();
			String serverPath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.SERVER_URL);
			String afileIRI = serverPath+"topicmodel";
			topics.add(corpusLocation);
			topics.add(dictionaryLocation);
			topics.add(modelLocation);
			MetaDataAnnotator.annotate(afileIRI, null, serverPath+request.getServletPath(), true, topics);

			
		result.put("success", 1);
		} catch (Exception e1) {
			throw new JPSRuntimeException(e1);
		}
		}
		return result;

	}
	
	/**
	 * get subject of IRI from querying metadata store
	 * @param IRI
	 * @return queried result
	 */
	private String queryDocsAddressfromMetaData(String IRI) {
		String addr = null;
		String queryStr = "PREFIX dcterms: <http://purl.org/dc/terms/> " + "SELECT ?addr  " + "WHERE {"
				+ "?addr  dcterms:subject <" + IRI + ">." + "}";
		String resultStr = KnowledgeBaseClient.query(MetaDataAnnotator.getMetadataSetUrl(), null, queryStr);
		try {
			JSONObject jo = new JSONObject(resultStr);
			addr = jo.getJSONObject("results").getJSONArray("bindings").getJSONObject(0).getJSONObject("addr")
					.getString("value");
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
		return addr;
	}
	
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {

        if (requestParams.isEmpty()||!requestParams.has("modelLocation")||!requestParams.has("dictionaryLocation")||!requestParams.has("corpusLocation")) {
            throw new BadRequestException();
        }
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
	     paramTypes.put("modelLocation",CUSTOMVALUETYPE.PATH);
	     paramTypes.put("dictionaryLocation",CUSTOMVALUETYPE.PATH);
	     paramTypes.put("corpusLocation",CUSTOMVALUETYPE.PATH);

			if (!ParamsValidateHelper.validateALLParams(requestParams, paramTypes)) {
				throw new BadRequestException();
			}
			return true;    }	
	
}

