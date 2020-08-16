package uk.ac.cam.cares.jps.ontomatch;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

@WebServlet(urlPatterns = { "/topicModelAgent" })
public class TopicModelAgent extends JPSHttpServlet{
	private String TOPICMODEL_TRAININGDOCUMENTS_PATH = "topicmodel.trainingdocuments.path";
	private String TOPICMODEL_DICTIONARY_PATH = "topicmodel.dictionary.path";
	private String TOPICMODEL_CORPUS_PATH = "topicmodel.corpus.path";
    private String TOPICMODEL_MODEL_PATH = "topicmodel.model.path";
	//TODO:input: parameters, training documents location, both should be put in settings?
//TODO: describe parameters and document location in owl?
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Topic model agent");
		JSONObject jo = requestParams;

		//feed paramter to python
        String documentsLocation = KeyValueMap.getInstance().get(TOPICMODEL_TRAININGDOCUMENTS_PATH);
        String corpusLocation = KeyValueMap.getInstance().get(TOPICMODEL_CORPUS_PATH);
        String dictionaryLocation = KeyValueMap.getInstance().get(TOPICMODEL_CORPUS_PATH);
        String modelLocation = KeyValueMap.getInstance().get(TOPICMODEL_CORPUS_PATH);
        
        //TODO:check if agent locator can find this script
		try {
			String[] params = {corpusLocation, dictionaryLocation, documentsLocation, modelLocation}; 
			String[] result = AsyncPythonHelper.callPython("modelTopic.py",params, TopicModelAgent.class);
		System.out.println(result);
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        JSONObject result = new JSONObject();
		JSONArray resArr = new JSONArray();
		try {
			result.put("success", 1);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;

	}


	
		

	
}

