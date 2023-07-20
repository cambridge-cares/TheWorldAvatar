package uk.ac.cam.cares.jps.accessagent.utils;

import org.apache.jena.arq.querybuilder.AskBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

/**
 * This class is a tool to upload store routing data 
 * (used by the AccessAgent and StoreRouter) to a triple store.
 * See the method 
 * {@link uk.ac.cam.cares.jps.accessagent.utils.RouterUploadTool#uploadRoutingData uploadRoutingData}
 * for details. <br>
 * Note: If routing data already exists for a given label, it is not overridden.
 * 
 * @author csl37
 *
 */
public class RouterUploadTool {

	private static final Logger LOGGER = LogManager.getLogger(RouterUploadTool.class);
	
	public static final String ONTOKGROUTER = "http://www.theworldavatar.com/kb/ontokgrouter/";
	public static final String RDF_TYPE = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
    
    public static final String LABEL = "label";
	public static final String QUERY_ENDPOINT = "queryEndpoint";
	public static final String UPDATE_ENDPOINT = "updateEndpoint";
    
	/**
	 * This method is used to upload routing information to the store router 
	 * The routing information (<b>label, queryEndpoint, updateEndpoint</b>)
	 * should be provided in the form of a JSONArray e.g. <br>
	 *  [<br>{  "label": "testLabel1", <br>
			"queryEndpoint": "http://www.example.com/blazegraph/namespace/testLabel1/sparql", <br>
			"updateEndpoint": "http://www.example.com/blazegraph/namespace/testLabel1/sparql" <br>
		}, <br>
		{	"label": "testLabel2", <br>
			"queryEndpoint": "http://www.example.com/blazegraph/namespace/testLabel2/sparql", <br>
			"updateEndpoint": "http://www.example.com/blazegraph/namespace/testLabel2/sparql" <br>
		}<br>] <br>
	 *	
	 * This will converted into triple of the form: <br>
	 * 	{@literal <}http://www.theworldavatar.com/kb/ontokgrouter/testLabel1{@literal >}	{@literal <}http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint{@literal >}	"http://www.example.com/blazegraph/namespace/testLabel1/sparql".<br>
	 *	{@literal <}http://www.theworldavatar.com/kb/ontokgrouter/testLabel1{@literal >}	{@literal <}http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint{@literal >}  "http://www.example.com/blazegraph/namespace/testLabel1/sparql".<br>
	 *	{@literal <}http://www.theworldavatar.com/kb/ontokgrouter/testLabel1{@literal >}	{@literal <}http://www.w3.org/1999/02/22-rdf-syntax-ns#type{@literal >} 										{@literal <}http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource{@literal >}.<br>
	 *	{@literal <}http://www.theworldavatar.com/kb/ontokgrouter/testLabel1{@literal >}	{@literal <}http://www.w3.org/1999/02/22-rdf-syntax-ns#type{@literal >} 										{@literal <}http://www.w3.org/2002/07/owl#NamedIndividual{@literal >}.<br>
	 *	{@literal <}http://www.theworldavatar.com/kb/ontokgrouter/testLabel1{@literal >}	{@literal <}http://www.w3.org/2000/01/rdf-schema#label{@literal >} 												"testLabel1".<br>
	 * 
	 * @param routingData routing information as an array of JSONObjects
	 * @param storeClient store router
	 * @return
	 */
    public int uploadRoutingData(JSONArray routingData, TripleStoreClientInterface storeClient) {
    	
    	UpdateRequest sparqlUpdate = new UpdateRequest();  	
    	int nUpload = 0;
    	int size = routingData.length();
    			
    	for (int i = 0; i < size; i++) {
        	
    		JSONObject obj = routingData.getJSONObject(i);
    		
    		//Parse
        	String label = MiscUtil.optNullKey(obj,LABEL); 
        	String queryEndpoint = MiscUtil.optNullKey(obj,QUERY_ENDPOINT);
        	String updateEndpoint = MiscUtil.optNullKey(obj,UPDATE_ENDPOINT);
        	
        	//check if routing information already exists for the label
        	//routing info is only uploaded if the label does not exist
        	boolean exists = checkLabel(storeClient, createCheckLabelQuery(label));
        	    
        	if(!exists) {
        		createInsertTriplesUpdate(label, queryEndpoint, updateEndpoint).appendTo(sparqlUpdate);
        		nUpload++;
        		LOGGER.info("Upload: label="+label+", queryEndpoint="+queryEndpoint+", updateEndpoint="+updateEndpoint);
        	}else {
        		String s = "<"+ONTOKGROUTER+label+">";
        		LOGGER.info("Routing data exists for subject: "+s);
        	}
    	}
    	    
    	//Upload triples
    	if (nUpload>0) {
    		uploadTriples(storeClient, sparqlUpdate);
    	}
    	
    	LOGGER.info("Uploaded "+Integer.toString(nUpload)+" of "+Integer.toString(size)+" entries");
    	
    	return nUpload;
    }
    
    /**
     * Create SPARQL ASK query to check if the "label" exists in the store.
     * The subject "<"+ONTOKGROUTER+label+">" is checked
     * @param label
     * @return
     */
    public String createCheckLabelQuery(String label) {
    	
    	String s = "<"+ONTOKGROUTER+label+">";
    	
    	AskBuilder builder = new AskBuilder();
		builder.addWhere(s, "?p", "?o");
 
    	return builder.build().toString();
    }
    
	/**
     * Execute SPARQL ASK query on storeClient to check if label exists
     * @param routerEndpoint
     * @param query
     * @return
     */
    public boolean checkLabel(StoreClientInterface storeClient, String query) {
    	
		String result = storeClient.execute(query);
		JSONObject obj =  new JSONArray(result).getJSONObject(0);
		return (boolean) obj.get("ASK");
    }
        
    /**
     * Create routing triples
     * @param label
     * @param queryEndpoint
     * @param updateEndpoint
     * @return
     */
	public UpdateBuilder createInsertTriplesUpdate(String label, String queryEndpoint, String updateEndpoint) {
		
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
	
	/**
	 * Upload triples to store router using a SPARQL update
	 * @param storeClient
	 * @param sparqlUpdate
	 */
	public void uploadTriples(TripleStoreClientInterface storeClient, UpdateRequest sparqlUpdate) {
		storeClient.executeUpdate(sparqlUpdate);		
	}
}
