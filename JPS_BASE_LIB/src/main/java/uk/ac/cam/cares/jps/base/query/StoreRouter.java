package uk.ac.cam.cares.jps.base.query;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.router.AbstractCachedRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;

/**
 * This class is developed to work as an instance factory for StoreClient.<br>
 * It reduces the burden of users to modify the SPARQL Endpoints for different<br>
 * knowledge bases when the web server changes. The users will always refer to<br>
 * the knowledge bases using the same IRI. For example, for ontokin the IRI will be<be>
 * http://kb/ontokin.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class StoreRouter extends AbstractCachedRouter<String, String>{
	private static Logger LOGGER = LogManager.getLogger(StoreRouter.class);
	public static final String FILE="file://";
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	public static final String KB="kb";
	public static final String SLASH="/";
	public static final String HTTP_KB_PREFIX = HTTP.concat(KB).concat(SLASH);
	public static final String EMPTY = "";
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
	public static final String FILE_PATH = "filePath";
	public static final String HAS_FILE_PATH = "hasFilePath";
	public static final String TOMCAT_ROOT_LABEL = "tomcatrootpath";
	public static final String COLON = ":";
	public static final String QUESTION_MARK = "?";
	public static final String TARGET_RESOURCE = "TargetResource";
	
	// get default ontokgrouter endpoint from jps.properties
	private static String STOREROUTER_ENDPOINT = KeyValueMap.getInstance().get(IKeys.URL_STOREROUTER_ENDPOINT);
	
	/**
	 * List of file extensions for file based resources
	 * ".owl",".rdf",".nt"
	 */
	public static final List<String> fileExtensions = Arrays.asList(".owl",".rdf",".nt"); //File extensions
	
	static StoreRouter storeRouter = null;
		
	/**
	 * Set STOREROUTER_ENDPOINT
	 * @param endpoint
	 */
	public static void setRouterEndpoint(String endpoint) {
		if (storeRouter == null) {
			storeRouter = new StoreRouter();
		}
		STOREROUTER_ENDPOINT = endpoint;
	}
	
	/**
	 * Returns a StoreClientInterface object based on a target resource ID
	 * provided as the input. For query and/or update operations, it
	 * supports two types of resources: <br> (a) a repository/namespace and <br>
	 * (b) an ontology/rdf file. <br> Some examples of these resources are provided below:<br>
	 * a) Example repositories/namespaces are (both IRI and namespace are accepted):<br>
	 *    - "http://theworldavatar.com/kb/ontokin" or "ontokin"<br>
	 *    - "http://theworldavatar.com/kb/ontospecies" or "ontospecies"<br>
	 *    - "http://theworldavatar.com/kb/ontocompchem" or "ontocompchem"<br>
	 * b) The target resource ID for a file based store is expected to end in .owl, .rdf, .nt.
	 * 	  Both a full IRI or just the path component are accepted. The path component must match 
	 *    the relative path of the file from Tomcat ROOT on Claudius. E.g.:<br>
	 *    - "http://theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl"
	 *      or "kb/sgp/singapore/SGTemperatureSensor-001.owl"
	 * 
	 * @param targetResourceID the IRI, namespace or path of an RDF/OWL repository/namespace
	 * @param isQueryOperation true/false
	 * @param isUpdateOperation true/false. 
	 * Note: both query and update operations can be true at the same time.
	 * @return StoreClient
	 */
	public static StoreClientInterface getStoreClient(String targetResourceID, boolean isQueryOperation, boolean isUpdateOperation) {
		
		String queryIRI = null;
		String updateIRI = null;
		StoreClientInterface kbClient = null;
		
		if (targetResourceID != null && !targetResourceID.isEmpty()) {
			
			if (storeRouter == null) {
				storeRouter = new StoreRouter();
			}
		
			if (isFileBasedTargetResourceID(targetResourceID)) {
			  
				String relativePath = getPathComponent(targetResourceID);
				String rootPath = getPathComponent(storeRouter.getLocalFilePath(STOREROUTER_ENDPOINT, TOMCAT_ROOT_LABEL));
				String filePath =  joinPaths(rootPath, relativePath);
				LOGGER.info("File based resource. file path="+filePath);
				
				kbClient = new FileBasedStoreClient(filePath);	
			}else if(isRemoteTargetResourceID(targetResourceID)){
				
				String targetResourceLabel = getLabelFromTargetResourceID(targetResourceID);
				LOGGER.info("Remote store. targetResourceLabel="+targetResourceLabel);
				
				if (isQueryOperation) {
					queryIRI = storeRouter.getQueryIRI(STOREROUTER_ENDPOINT, targetResourceLabel);
				}
				if (isUpdateOperation) {
					updateIRI = storeRouter.getUpdateIRI(STOREROUTER_ENDPOINT, targetResourceLabel);
				}
				if (queryIRI != null && !queryIRI.isEmpty()) {
					kbClient = new RemoteStoreClient(queryIRI);
				}
				if (updateIRI != null && !updateIRI.isEmpty()) {
					if (kbClient == null) {
						kbClient = new RemoteStoreClient();
					}
					kbClient.setUpdateEndpoint(updateIRI);
				}
			
				if(queryIRI==null && updateIRI==null){
					LOGGER.error("Endpoint could not be retrieved for the following resource IRI:"+targetResourceID+", label:"+targetResourceLabel);
				}
				if(isQueryOperation == false && isUpdateOperation == false){
					LOGGER.error("null will be returned as both the isQueryOperation and isUpdateOperation parameters are set to false.");
				}
			}else {
				throw new JPSRuntimeException("Invalid targetResourceID: "+targetResourceID);
			}
		}else {
			LOGGER.error("targetResourceID is null.");
		}
		
		return kbClient;
	}
	
	/**
	 * Check that the targetResourceID is either a valid IRI or namespace label for a remote resource.
	 * A namespace label is valid if it is composed of only alphanumeric characters (A-Z, 0-9) 
	 * or the special characters - and _
	 * @param targetResourceID
	 * @return
	 */
	public static boolean isRemoteTargetResourceID(String targetResourceID) {
		
		if(InputValidator.checkIfValidIRI(targetResourceID)){
			return true;
		}else {
			if(targetResourceID.matches("[A-Za-z0-9\\-\\_]+")) {
				return true;
			}else {
				LOGGER.error("Invalid namespace label:"+targetResourceID+". Not alphanumeric (special characters - and _ are allowed).");
				return false;
			}
		}
	}
	
	/**
	 * Returns true if the resource is a file. 
	 * Checks if the targetResourceID ends with a listed file extension {@link #fileExtensions}
	 * and is a valid file path.
	 * @param targetResourceID
	 * @return
	 */
	public static boolean isFileBasedTargetResourceID(String targetResourceID) {
		//check for valid file extension
		if( fileExtensions.stream().anyMatch(targetResourceID.trim()::endsWith)) {
			
			String path = null;
			try {
				if(InputValidator.checkIfValidIRI(targetResourceID)) {
					//targetResourceID is an IRI, so get the path component first
					path = getPathComponent(targetResourceID);
				}else {
					path = targetResourceID;
				}
				Paths.get(path);
			//JPSRuntimeException can be thrown by getPathComponent
			} catch (InvalidPathException | JPSRuntimeException ex ) {
				LOGGER.error("Invalid file path: "+path);
				return false;
			}
		    return true;
		}else {
			return false;
		}
	}
	
	/**
	 * Get the namespace ("label") from the target resource ID for a remote store, 
	 * this will be matched against the label in ontokgrouter.
	 * @param targetResourceID
	 * @return
	 */
	public static String getLabelFromTargetResourceID(String targetResourceID) {
		return targetResourceID.substring(targetResourceID.lastIndexOf(SLASH)+1).trim();
	}
	
	/**
	 * Get path component from IRI
	 * @param iri: a valid iri
	 * @return
	 */
	public static String getPathComponent(String iri) {
		String path = null;
		try {
			URI uri = new URI(URLDecoder.decode(iri.trim(),"UTF-8"));
			path = uri.getPath();
		} catch (UnsupportedEncodingException | URISyntaxException e) {
			throw new JPSRuntimeException(e);
		}
		return path;
	}
	
	/**
	 * Join two path components 
	 * @param path1
	 * @param path2
	 * @return path1/path2
	 */
	public static String joinPaths(String path1, String path2) {
		//add a slash if path2 does not begin with one
		if(!path2.startsWith(SLASH)) {	
			return path1.trim()+SLASH+path2.trim();
		}else {
			return path1.trim() + path2.trim();		
		}
	}
	
	/**
	 * Retrieve file path of the target resource/owl file.
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @return
	 */
	private String getLocalFilePath(String kgrouterEndpoint, String targetResourceName) {
		SelectBuilder builder = new SelectBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
				.addVar( QUESTION_MARK.concat(RESOURCE))
				.addVar( QUESTION_MARK.concat(LABEL) )
				.addVar( QUESTION_MARK.concat(FILE_PATH) )
				.addWhere( getCommonKGRouterWhereBuilder() )
			    .addWhere( QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_FILE_PATH), QUESTION_MARK.concat(FILE_PATH) );
		//TODO getStoreClient
		//TODO getFromStore
		RemoteStoreClient rKBClient = new RemoteStoreClient(kgrouterEndpoint);
		System.out.println(builder.toString());
		String json = rKBClient.execute(builder.toString());
		JSONArray jsonArray = new JSONArray(json);
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			if(obj.getString(LABEL).equals(targetResourceName)){
				System.out.println(obj.get(FILE_PATH));
				return obj.getString(FILE_PATH);
			}
		}
		return null;
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
		//TODO getStoreClient
		//TODO getFromStore
		RemoteStoreClient rKBClient = new RemoteStoreClient(kgrouterEndpoint);
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
		//TODO getStoreClient
		//TODO getFromStore
		RemoteStoreClient rKBClient = new RemoteStoreClient(kgrouterEndpoint);
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
