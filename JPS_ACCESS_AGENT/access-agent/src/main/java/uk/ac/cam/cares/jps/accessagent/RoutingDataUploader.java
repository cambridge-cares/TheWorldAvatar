package uk.ac.cam.cares.jps.accessagent;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * The purpose of this class is to upload routing information to ontokgrouter
 * @author csl37
 *
 */
@WebServlet(urlPatterns = {RoutingDataUploader.UPLOAD_URL})
public class RoutingDataUploader  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	public static final String UPLOAD_URL = "/upload";
	
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(RoutingDataUploader.class);
	    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

    	validateInput(requestParams);
        
    	String routerEndpoint = MiscUtil.optNullKey(requestParams,"routerEndpoint");
    	if(routerEndpoint == null) {
    		routerEndpoint = StoreRouter.STOREROUTER_ENDPOINT;    		
    	}
    	
    	UpdateRequest sparqlUpdate = new UpdateRequest();
    	
    	JSONArray array = requestParams.getJSONArray(JPSConstants.CONTENT);    	    	
    	
    	for (int i = 0, size = array.length(); i < size; i++) {
    	
    		JSONObject obj = array.getJSONObject(i);
    		
    		//Parse
        	String label = obj.getString("label"); 
        	String queryEndpoint = obj.getString("queryEndpoint");
        	String updateEndpoint = obj.getString("updateEndpoint");
        	
        	createTriples(label, queryEndpoint, updateEndpoint).appendTo(sparqlUpdate);	
    	}
    	    
    	//Upload triples
    	upload(routerEndpoint, sparqlUpdate);
        
    	//TODO return number of triples uploaded
        return new JSONObject().put("Result", "Done");
    }
    
    @Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {	    
		
	    if (requestParams.isEmpty()) {
	        throw new BadRequestException();
	    }
	    try {
	    	
	    	//POST only
	    	String method = MiscUtil.optNullKey(requestParams,JPSConstants.METHOD);
	        if (method != HttpPost.METHOD_NAME) {
	        	return false;
	        }
	    	
	        //JSONObject
	        
			return true;
	        
	    }catch (JSONException ex) {
	    	return false;
	    }
	}
    
	//TODO accessagent.properties file with data structure
	//or load from ontology
	// This should be a servlet and use RemoteStoreClient to send a sparql update
	
    private static final String ONTOKGROUTER = "http://www.theworldavatar.com/kb/ontokgrouter/";
    private static final String RDF_TYPE = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
    
	public UpdateBuilder createTriples(String label, String queryEndpoint, String updateEndpoint) {
		
		String s = "<"+ONTOKGROUTER+label+">";
		
		UpdateBuilder builder = new UpdateBuilder();
		builder.addInsert(s,
				"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>",
				"\""+queryEndpoint+"\"");
		builder.addInsert(s,
				"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint>",
				"\""+updateEndpoint+"\"");
		builder.addInsert(s,
				RDF_TYPE,
				"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>");
		builder.addInsert(s,
				RDF_TYPE,
				"<http://www.w3.org/2002/07/owl#NamedIndividual>");
		builder.addInsert(s,
				"<http://www.w3.org/2000/01/rdf-schema#label>",
				"\""+label+"\"");
				
		return builder;
	}
	
	public void upload(String routerEndpoint, UpdateRequest sparqlUpdate) {
			
		RemoteStoreClient storeRouter = new RemoteStoreClient();
		storeRouter.setUpdateEndpoint(routerEndpoint);
		storeRouter.executeUpdate(sparqlUpdate);		
	}
	
	
}
