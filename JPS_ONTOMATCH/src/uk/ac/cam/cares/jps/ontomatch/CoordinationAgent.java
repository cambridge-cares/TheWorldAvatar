package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
 * Coordination agent that runs each component agent to run a process of ontology matching, two types provided: terminology and individual
 * Outout to KG: alignment ontology of this matching process
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
@WebServlet(urlPatterns = { "/coordinate" })
public class CoordinationAgent extends JPSHttpServlet {

	private static final long serialVersionUID = 1129478305476925061L;
	private String kb_path = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class);
	
    


	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		logger.info("Coordination agent");
		JSONObject jo = requestParams;
		String alignmentIRI = null, sIRI = null, tIRI = null, type = null;
		String modelPath = null, dictPath = null;
        double threshold = 0;
        Double[] weights = {0.1,0.2,0.3};
        String[] choices = null;
        String[] matchers = new String[3];
		/**get caller parameters and set component agent parameter*******/
		try {
			alignmentIRI = jo.getString("aIRI");
			 sIRI = jo.getString("sourceIRI");
			 tIRI = jo.getString("targetIRI");
			 type = jo.getString("matchingType");
			 threshold = jo.getDouble("threshold");
			 /**get weights**/
			 JSONArray jweight = jo.getJSONArray("weights");
			List<Double> lweight = new ArrayList<Double>();
			System.out.println(jweight);
			for(int i=0; i<jweight.length();i++) {
				lweight.add(jweight.getDouble(i));
			}
			weights = new Double[lweight.size()];
			lweight.toArray(weights);
			/**get choices if any**/
			JSONArray jcho = jo.getJSONArray("choices");
			List<String> lcho = new ArrayList<String>();
			for(int i=0; i<weights.length;i++) {
				lcho.add(jcho.getString(i));
			}
			choices = new String[lcho.size()];
			lcho.toArray(choices);
			
		} catch (JSONException e1) {
			e1.printStackTrace();
		}
		
		try {
			modelPath = jo.getString("modelAddress");
			dictPath = jo.getString("dictAddress");
		}catch(Exception e) {
			
		}
		/**set matchers to call - matching type specific***/
         if(MATCHING_TYPE.valueOf(type)==MATCHING_TYPE.TERM) {//is term matching
        	 matchers[0]="STRING";
        	 matchers[1] = "WORD";
        	 matchers[2] ="DOMAIN";
         } else {//is individual matching
        	 matchers[0]="I_STRING";
        	 matchers[1] = "I_WORD";
        	 matchers[2] ="VALUE";  
        	 
       	  /**get tmp file IRI that contains potential triples****/
        	 String[] targetClassIRIs = {"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant"};
        	 URI uri;
			try {
				uri = new URI(tIRI);
	        	 String[] segments = uri.getPath().split("/");
	        	 String name = segments[segments.length-1]+"pt.owl";//TODO: proper format this name
	        	 String nameWpath = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class) +"/"+name;
	        	 logger.info(nameWpath);
	        	 tIRI = queryPotentialInstanceAndSave(targetClassIRIs,tIRI,nameWpath);
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}

         }
         
		JSONObject result = new JSONObject();
		try {
			/**call lexical processor***/
			callLexicalProcessor(sIRI);
			callLexicalProcessor(tIRI);
			/**call matching agents*/
            String[] tmpAlignments = callMatchingAgents(sIRI, tIRI,type, matchers);
            /**call aggregation agent, which saves the alignment result to KG**/
            callAggregationAgents(weights, tmpAlignments,choices,threshold,sIRI,tIRI,alignmentIRI);
			result.put("success", true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	/***
	 * calls the lexical processor with required params
	 * @param IRI of the ontology to process
	 */
	protected void callLexicalProcessor(String IRI) {
		JSONObject requestParams = new JSONObject();
		String[] splitIRIs = IRI.split("/");
		String name = splitIRIs[splitIRIs.length-1];
		String saveAddress = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class)+"/tmp/"+name; 
		saveAddress = saveAddress.replaceAll("\\\\", "/");

		requestParams.put("saveAddress", saveAddress);
        requestParams.put("ontologyIRI", IRI);
        logger.info("Call lexical processor with saveAddress:"+saveAddress+" ontologyIRI: "+IRI);
		AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/ontologyProcessor", requestParams.toString());

	}
	
	/***
	 * 
	 * @param sOnto source ontology IRI to match
	 * @param tOnto target ontology IRI to match
	 * @param matchingType TERM or INDIVIDUAL
	 * @param matchers String[] types of the matchers to call
	 * @return String[] names of the generated alignment ontology in KG
	 * @throws JSONException
	 * @throws ParseException
	 */
	protected String[] callMatchingAgents(String sOnto, String tOnto,String matchingType,String[] matchers) throws JSONException, ParseException {
		//call each matcher
		String[] names = new String[matchers.length];
		for (int i = 0; i< matchers.length; i++) {
			JSONObject requestParams = new JSONObject();
			//create names for each 2bgenerate alignment file
			String name = getAlignmentFileName(tOnto, sOnto, matchers[i]);
			names[i] = name;
			 requestParams.put("alignmentFileAddress", name );
			//handle special case: domain
             requestParams.put("matcherType", matchers[i]);
                requestParams.put("matchingType", matchingType);
                requestParams.put("targetOntoIRI", tOnto);
                requestParams.put("sourceOntoIRI", sOnto);
                if(matchers[i].equals("DOMAIN")) {
                    requestParams.put("modelAddress", tOnto);
                    requestParams.put("dictAddress", sOnto);
                }
        		AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/elementMatcher", requestParams.toString());
		}
		return names;
	}
	
	/**
	 * auto construct alignment ontology name according to the matching info
	 * @param tgtOnto target ontology IRI
	 * @param srcOnto source ontology IRI
	 * @param matcherType matcherType or matchingType of the agent that will generate this alignment
	 * @return String name of alignment ontology to generate
	 * @throws ParseException
	 */
	protected String getAlignmentFileName(String tgtOnto, String srcOnto, String matcherType) throws ParseException {
		final DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/ddhh:mm:ssaaa");
		Date date = new Date();
		String dateFormatted = dateFormat.format(date);
		return getName(tgtOnto)+getName(srcOnto)+matcherType;
		}
	
	protected String getName(String IRI) {
		String secs[]= IRI.split("/");
         String fileName =  secs[secs.length-1];
         String names[] = fileName.split(".");
         return names[0];
	}
	/**
	 * Calls the aggregation agent with correct params
	 * @param weights   weights to weight sum each alignment measure
	 * @param alignmentIRIs2Aggr
	 * @param choices   choices of optional steps in aggregator
	 * @param threshold threshold of measure saving to the final alignment
	 * @param srcOnto
	 * @param tgtOnto
	 * @param addr
	 */
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
	

    /***
     * 	Get potential Instances to match according to class, save as a tmp ontology file
     * @param targetClassIRIs
     * @param remoteEP
     * @param savePath
     * @return IRI of the tmp ontology file
     */
	//TODO
	protected String queryPotentialInstanceAndSave(String[] targetClassIRIs, String remoteEP, String savePath) {
		JSONObject requestParams = new JSONObject();
       //use targetClassIRI to execute remote query to remoteEP
		//TODO:save retrieved triples into a file
		return null;
	}
	

	protected void testGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
}

