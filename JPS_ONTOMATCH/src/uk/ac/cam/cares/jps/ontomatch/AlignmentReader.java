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

/***
 * Agent that reads content from alignment kg(rdf format), for visualization purpose
 *
 */
@WebServlet(urlPatterns = { "/alignment" })
public class AlignmentReader extends JPSHttpServlet {
    
    /**
     * 
     */
	private static final long serialVersionUID = -4365515995166685342L;


	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(AlignmentReader.class);
    }
	
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("AlignmentReader agent");
		JSONObject jo = requestParams;
		String afileIRI = "";
		Double threshold = 0.0;
		//read parameters
		try {
			//Use this for stageB
			afileIRI = jo.getString("alignmentIRI");
			threshold = jo.getDouble("threshold");
		} catch (JSONException e1) {
			e1.printStackTrace();
		}
	    //String threshold = "0.6";
	    //String stubIRI = "http://localhost:3000/a.owl";
		System.out.println("reading alignment from  " + afileIRI);
	 	JSONArray instances2Equal;

		JSONObject result = new JSONObject();
		try {
			//get a list of matched IRIs where
			instances2Equal = AlignmentIOHelper.readAlignmentFileAsJSONArray(afileIRI, threshold);
			result.put("alignmentlist", instances2Equal);
			System.out.print("read parameter result: ");
			System.out.println(result.toString());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

		


}

