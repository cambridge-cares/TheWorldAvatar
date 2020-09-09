package uk.ac.cam.cares.jps.ontomatch;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/***
 * 
 * Agents that generates a topic model from a list of training documents
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
@WebServlet(urlPatterns = { "/topicModelAgent" })
public class TopicModelAgent extends JPSHttpServlet{

	private static final long serialVersionUID = -4621180516256485859L;

	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
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
			e1.printStackTrace();
		}
		try {//get document save address by querying metadata annotator
			String serverPath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.KB_URL);
		    String docPath = queryDocsAddressfromMetaData(serverPath+request.getServletPath());
		}catch(Exception e) {
			e.printStackTrace();
		}
        //String corpusLocation = OntomatchProperties.getInstance().getProperty(OntomatchProperties.TOPICMODEL_CORPUS_PATH);
        //String dictionaryLocation = OntomatchProperties.getInstance().getProperty(OntomatchProperties.TOPICMODEL_DICTIONARY_PATH);
        //String modelLocation = OntomatchProperties.getInstance().getProperty(OntomatchProperties.TOPICMODEL_MODEL_PATH);
        
        JSONObject result = new JSONObject();
		try {
			String[] params = {corpusLocation, dictionaryLocation, docsPath, modelLocation}; 
			String[] prints = AsyncPythonHelper.callPython("modelTopic.py",params, TopicModelAgent.class);
			List<String> topics = new ArrayList<String>();
			String serverPath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.KB_URL);
			String afileIRI = serverPath+"topicmodel";
			topics.add(corpusLocation);
			topics.add(dictionaryLocation);
			topics.add(modelLocation);
			MetaDataAnnotator.annotate(afileIRI, null, serverPath+request.getServletPath(), true, topics);

			
		result.put("success", 1);
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		return result;

	}
	
	//TODO: duplicate, merge into a helper function
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
			logger.error(e.toString());
		}
		return addr;
	}
	
}

