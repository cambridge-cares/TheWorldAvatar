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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


@WebServlet(urlPatterns = { "/alignment" })
public class AlignmentReader extends JPSHttpServlet {
    
	private static final long serialVersionUID = -2354646810093235777L;
    
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(DataLinker.class);
    }
	
	Logger logger = LoggerFactory.getLogger(DataLinker.class);
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Datalinker agent");

		JSONObject jo = requestParams;
		// read alignment
		String afileIRI = "";
		try {
			//Use this for stageB
			//afileIRI = jo.getString("alignmentIRI");
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		//TODO: add this to config
	    String threshold = "0.6";
	    String stubIRI = "http://localhost:3000/a.owl";
		System.out.println("reading alignment from  " + afileIRI);
		//get a list of matched IRIs where 
		//add to destination
		JSONArray instances2Equal;

		JSONObject result = new JSONObject();
		try {
			instances2Equal = AlignmentHelper.readAlignmentFileAsJSONArray(stubIRI, threshold);
			result.put("alignmentlist", instances2Equal);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
	}

		


}

