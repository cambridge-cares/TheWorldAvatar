package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHING_TYPE;

/**
 * Agent performs Terminology matching
 *
 */
@WebServlet(urlPatterns = { "/coordinate" })
public class CoordinationAgent extends JPSHttpServlet {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1129478305476925061L;
	private String MATCH_METHOD = "matchSerial";
	private String kb_path = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class);
	
    
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(AlignmentReader.class);
    }
	/***
	 * Types:a.terminology matching b.individual matching 
	 * Steps:
	 * 1. call list of matchers 
	 * 2. call aggregator
	 *  input: 
	 *  output: new ontology alignment file in kb
	 */
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("Coordination agent");
		System.out.println("kb path: "+kb_path);
		JSONObject jo = requestParams;
		String alignmentIRI = null, sIRI = null, tIRI = null, type = null;
        double threshold = 0;
        Double[] weights = null;
        String[] choices = null;
        String[] matchers = new String[3];

		try {//get caller parameters
			alignmentIRI = jo.getString("aIRI");
			 sIRI = jo.getString("sourceIRI");
			 tIRI = jo.getString("targetIRI");
			 type = jo.getString("matchingType");
			 threshold = jo.getDouble("threshold");
			 /**get weights**/
			 JSONArray jweight = jo.getJSONArray("weights");
			List<Double> lweight = new ArrayList<Double>();
			for(int i=0; i<jweight.length();i++) {
				lweight.add(jweight.getDouble(i));
			}
			weights = new Double[lweight.size()];
			lweight.toArray(weights);
			/**get choices if any**/
			JSONArray jcho = jo.getJSONArray("choices");
			List<String> lcho = new ArrayList<String>();
			for(int i=0; i<jweight.length();i++) {
				lcho.add(jcho.getString(i));
			}
			choices = new String[lcho.size()];
			lcho.toArray(choices);
			
		} catch (JSONException e1) {
			e1.printStackTrace();
		}
		//set matchers - matching type specific
         if(MATCHING_TYPE.valueOf(type)==MATCHING_TYPE.TERM) {
        	 matchers[0]="STRING";
        	 matchers[1] = "WORD";
        	 matchers[2] ="DOMAIN";
         } else {
        	 matchers[0]="I_STRING";
        	 matchers[1] = "I_WORD";
        	 matchers[2] ="VALUE";  
        	 String[] targetClassIRIs = {"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant"};
        	  //get tmp file IRI that contains potential triples
        	 URI uri;
			try {
				uri = new URI(tIRI);
	        	 String[] segments = uri.getPath().split("/");
	        	 String name = segments[segments.length-1]+"pt.owl";
	        	 String nameWpath = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class) +"/"+name;
	        	 System.out.println(nameWpath);
	        	 tIRI = queryPotentialInstanceAndSave(targetClassIRIs,tIRI,nameWpath);
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

         }
         
		JSONObject result = new JSONObject();
		try {
			preProcessOnto(sIRI);
			preProcessOnto(tIRI);
            String[] tmpAlignments = callMatchingAgents(sIRI, tIRI,matchers);
            callAggregationAgents(weights, tmpAlignments,choices,threshold,sIRI,tIRI,alignmentIRI);
			result.put("success", true);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
	}

	protected void preProcessOnto(String IRI) {
		JSONObject requestParams = new JSONObject();
		String[] splitIRIs = IRI.split("/");
		String name = splitIRIs[splitIRIs.length-1];
		String saveAddress = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class)+"/JPS_ONTOMATCH/tmp/"+name; 
        requestParams.put("saveAddress", saveAddress);
        requestParams.put("ontologyIRI", IRI);
        System.out.println("Call lexical processor with saveAddress:"+saveAddress+" ontologyIRI: "+IRI);
		AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/ontologyProcessor", requestParams.toString());

	}
	protected String[] callMatchingAgents(String sOnto, String tOnto,String[] matchers) {
		//call each matcher
		for (int i = 0; i< matchers.length; i++) {
			JSONObject requestParams = new JSONObject();
			//TODO: make up names for each
			//requestParams.put("alignmentFileAddress", );
			//handle special case: domain
                requestParams.put("matcherType", matchers[i]);
                requestParams.put("targetOntoIRI", tOnto);
                requestParams.put("sourceOntoIRI", sOnto);
                requestParams.put("matchMethod", MATCH_METHOD);
        		AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/elementMatcher", requestParams.toString());
		//TODO:return names for each
		}
		return null;
	}
	
	protected void callAggregationAgents(Double[] weights, String[] alignmentIRIs2Aggr, String[] choices, double threshold, String srcOnto, String tgtOnto, String addr ) {
		JSONObject requestParams = new JSONObject();
        requestParams.put("weights", weights);
        requestParams.put("alignments", alignmentIRIs2Aggr);
        requestParams.put("srcOnto", srcOnto);
        requestParams.put("tgtOnto", tgtOnto);
        requestParams.put("addr", addr);
        if(choices!=null) {
            requestParams.put("choices", choices);
        }

        AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/matchAggregator", requestParams.toString());
	}
	

	
	protected String queryPotentialInstanceAndSave(String[] targetClassIRIs, String remoteEP, String savePath) {
		JSONObject requestParams = new JSONObject();
       //use targetClassIRI to execute remote query to remoteEP
		//TODO:save retrieved triples into a file
		return null;
	}
	
	//TODO
	protected void queryPotentialInstanceAndSave(String termAlignmentIRI, String remoteEP, double threshold, String savePath) {
		JSONObject requestParams = new JSONObject();
        requestParams.put("alignmentIRI", termAlignmentIRI);
        requestParams.put("threshold", threshold );
		String termAlignment = AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/AlignmentReader", requestParams.toString());
		//TODO:get list, then call functionBelow		
	}
	protected void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
}

