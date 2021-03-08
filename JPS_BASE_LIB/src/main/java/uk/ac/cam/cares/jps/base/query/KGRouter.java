package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is developed to work as an instance factory for KnowledgeBaseClient.<br>
 * It reduces the burden of users to modify the SPARQL Endpoints for different<br>
 * knowledge bases when the web server changes. The users will always refer to<br>
 * the knowledge bases using the same IRI. For example, for ontokin the IRI will be<be>
 * http://kb/ontokin.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class KGRouter{
	private static Logger logger = LoggerFactory.getLogger(KGRouter.class);
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	public static final String KB="kb";
	public static final String BACKSLASH="kb";
	public static final String HTTP_KB_PREFIX = HTTP.concat(KB).concat(BACKSLASH);
	public static final String EMPTY = "";
	private static final String KGROUTER_ENDPOINT = "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql";
	public static final String RDFS_PREFIX = "rdfs";
	public static final String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF_PREFIX = "rdf";
	public static final String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public static final String RDF_TYPE = "type";
	public static final String ONTOKGROUTER_PREFIX = "ontokgrouter";
	public static final String ONTOKGROUTER = "http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#";
	public static final String RESOURCE = "resource";
	public static final String LABEL = "label";
	public static final String QUERY_ENDPOINT = "queryEndpoint";
	public static final String HAS_QUERY_ENDPOINT = "hasQueryEndpoint";
	public static final String UPDATE_ENDPOINT = "updateEndpoint";
	public static final String HAS_UPDATE_ENDPOINT = "hasUpdateEndpoint";
	public static final String COLON = ":";
	public static final String QUESTION_MARK = "?";
	public static final String TARGET_RESOURCE = "TargetResource";
	
	static KGRouter kgRouter = null;
	
	/**
	 * Based on a target resource IRI or path provided as the input, it returns the<br>
	 * corresponding KnowledgeBaseClient. For query and/or update operations, it<br>
	 * supports two types of resources: a) a repository/namespace and b) an ontology<br>
	 * file. Some examples of these resources are provided below:<br>
	 * a) Example repositories/namespaces are:<br>
	 *    - http://kb/ontokin
	 *    - http://kb/ontospecies
	 *    - http://kb/ontocompchem
	 * b) Example ontology files are:<br>
	 *    - C://path/to/an/abox.owl (On Windows)
	 *    - /home/path/to/an/abox.owl (On Linux)
	 * 
	 * @param targetResourceIRIOrPath the IRI of an RDF/OWL repository/namespace<br>
	 *  or the path to an RDF/OWL file. 
	 * @param isQueryOperation true/false
	 * @param isUpdateOperation true/false. Note: both query and update operations<br>
	 *  can be true at the same time.
	 * @return
	 */
	public static KnowledgeBaseClient getKnowledgeBaseClient(String targetResourceIRIOrPath, boolean isQueryOperation, boolean isUpdateOperation) {
		String queryIRI = null;
		String updateIRI = null;
		KnowledgeBaseClient kbClient = null;
		if (targetResourceIRIOrPath != null && !targetResourceIRIOrPath.isEmpty()) {
			if (targetResourceIRIOrPath.trim().startsWith(HTTP_KB_PREFIX)) {
				if (kgRouter == null) {
					kgRouter = new KGRouter();
				}
				if (isQueryOperation) {
					queryIRI = kgRouter.getQueryIRI(KGROUTER_ENDPOINT, targetResourceIRIOrPath.replace(HTTP_KB_PREFIX, EMPTY));
				}
				if (isUpdateOperation) {
					updateIRI = kgRouter.getUpdateIRI(KGROUTER_ENDPOINT, targetResourceIRIOrPath.replace(HTTP_KB_PREFIX, EMPTY));
				}
				if (queryIRI != null && !queryIRI.isEmpty()) {
					kbClient = new RemoteKnowledgeBaseClient(queryIRI);
				}
				if (updateIRI != null && !updateIRI.isEmpty()) {
					if (kbClient == null) {
						kbClient = new RemoteKnowledgeBaseClient();
					}
					kbClient.setUpdateEndpoint(updateIRI);
				}
				if(queryIRI==null && updateIRI==null){
					logger.error("Endpoint could not be retrieved for the following resource IRI:"+targetResourceIRIOrPath);
				}
				if(isQueryOperation == false && isUpdateOperation == false){
					logger.error("null will be returned as both the isQueryOperation and isUpdateOperation parameters are set to false.");
				}
			}else{
				File file = new File(targetResourceIRIOrPath);
				if(file.exists()){
					kbClient = new FileBasedKnowledgeBaseClient(targetResourceIRIOrPath);
				}
			}
		}
		return kbClient;
	}
	
	/**
	 * Retrieves the query IRI of the target repository/namespace. 
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @return
	 * @throws Exception
	 */
	private String getQueryIRI(String kgrouterEndpoint, String targetResourceName){
		SelectBuilder builder = new SelectBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
				.addVar( QUESTION_MARK.concat(RESOURCE))
				.addVar( QUESTION_MARK.concat(LABEL) )
				.addVar( QUESTION_MARK.concat(QUERY_ENDPOINT) )
				.addWhere( getCommonKGRouterWhereBuilder() )
			    .addWhere( QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_QUERY_ENDPOINT), QUESTION_MARK.concat(QUERY_ENDPOINT) );
		RemoteKnowledgeBaseClient rKBClient = new RemoteKnowledgeBaseClient(kgrouterEndpoint);
		System.out.println(builder.toString());
		String json = rKBClient.execute(builder.toString());
		JSONArray jsonArray = new JSONArray(json);
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			if(obj.getString(LABEL).equals(targetResourceName)){
				System.out.println(obj.get(QUERY_ENDPOINT));
				return obj.getString(QUERY_ENDPOINT);
			}
		}
		return null;
	}
	
	/**
	 * Retrieves the update IRI of the target repository/namespace. 
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @return
	 * @throws Exception
	 */
	private String getUpdateIRI(String kgrouterEndpoint, String targetResourceName){
		SelectBuilder builder = new SelectBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
				.addVar( QUESTION_MARK.concat(RESOURCE))
				.addVar( QUESTION_MARK.concat(LABEL) )
				.addVar( QUESTION_MARK.concat(UPDATE_ENDPOINT) )
				.addWhere( getCommonKGRouterWhereBuilder() )
			    .addWhere( QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_UPDATE_ENDPOINT), QUESTION_MARK.concat(UPDATE_ENDPOINT) );
		RemoteKnowledgeBaseClient rKBClient = new RemoteKnowledgeBaseClient(kgrouterEndpoint);
		System.out.println(builder.toString());
		String json = rKBClient.execute(builder.toString());
		JSONArray jsonArray = new JSONArray(json);
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			if(obj.getString(LABEL).equals(targetResourceName)){
				System.out.println(obj.get(UPDATE_ENDPOINT));
				return obj.getString(UPDATE_ENDPOINT);
			}
		}
		return null;
	}

	
	/**
	 * Created to put the generic part of the SPARQL query commands using the Jena Query Builder.
	 * 
	 * @return
	 */
	private WhereBuilder getCommonKGRouterWhereBuilder(){
		return new WhereBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
			    .addWhere( QUESTION_MARK.concat(RESOURCE), RDF_PREFIX.concat(COLON).concat(RDF_TYPE), ONTOKGROUTER_PREFIX.concat(COLON).concat(TARGET_RESOURCE) )
			    .addWhere( QUESTION_MARK.concat(RESOURCE), RDFS_PREFIX.concat(COLON).concat(LABEL), QUESTION_MARK.concat(LABEL) );
		}
}
