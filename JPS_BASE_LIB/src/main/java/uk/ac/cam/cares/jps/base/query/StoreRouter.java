package uk.ac.cam.cares.jps.base.query;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.expr.Expr;
import org.json.JSONArray;
import org.json.JSONObject;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.cache.LRUCache;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.router.AbstractCachedRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

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
public class StoreRouter extends AbstractCachedRouter<String, List<String>>{

	public static final Logger LOGGER = LogManager.getLogger(StoreRouter.class);

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
	
	/**
	 * List of file extensions for file based resources
	 * ".owl",".rdf",".nt"
	 */
	public static final List<String> FILE_EXTENSIONS = Arrays.asList(".owl",".rdf",".nt"); //File extensions

	/*
	 * Static initialisation of the store router endpoint from environment variable
	 * or, if one does not exist, from the jps.properties file.
	 */
	public static final String STOREROUTER_ENDPOINT_NAME = "STOREROUTER_ENDPOINT";
	public static String storeRouterEndpoint;
	static{
		storeRouterEndpoint = System.getenv(STOREROUTER_ENDPOINT_NAME);
		if(storeRouterEndpoint == null) {
			// if endpoint is not set in the system environment  
			// then get the default value from jps.properties
			LOGGER.info("STOREROUTER_ENDPOINT not found in environment variables..."
					+ " Using jps.properties.");
			storeRouterEndpoint = KeyValueMap.getInstance().get(IKeys.URL_STOREROUTER_ENDPOINT);	
		}
		LOGGER.info("STOREROUTER_ENDPOINT set to "+storeRouterEndpoint);		
	}

	/*
	 * LRU Cache configuration:
	 * key=label, value=[queryEndpoint, updateEndpoint]
	 */
	private static final int CACHE_SIZE = Integer.parseInt(KeyValueMap.getInstance().get(IKeys.STOREROUTER_CACHE_SIZE));
	public static final int QUERY_INDEX = 0;
	public static final int UPDATE_INDEX = 1;

	private static StoreRouter storeRouter = null;

	/**
	 * StoreRouter singleton
	 */
	private StoreRouter() {
		super(new LRUCache<String,List<String>>(CACHE_SIZE));
	}
	
	public static synchronized StoreRouter getInstance() {
		if (storeRouter == null) {
			storeRouter = new StoreRouter();
		}
		return storeRouter;
	}
	
	/**
	 * Returns a TripleStoreClientInterface object based on a target resource ID
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
	 * @return TripleStoreClient
	 */
	public static TripleStoreClientInterface getStoreClient(String targetResourceID, boolean isQueryOperation, boolean isUpdateOperation) {
		
		String queryIRI = null;
		String updateIRI = null;
		TripleStoreClientInterface kbClient = null;
		
		if (targetResourceID != null && !targetResourceID.isEmpty()) {
			
			//instantiate singleton if not already done so
			getInstance();

			if (isFileBasedTargetResourceID(targetResourceID)) {
			  
				String relativePath = getPathComponent(targetResourceID);
				String rootPath = getPathComponent(storeRouter.getLocalFilePath(TOMCAT_ROOT_LABEL,storeRouter.getRouterStoreClient()));
				String filePath =  joinPaths(rootPath, relativePath);
				LOGGER.info("File based resource. file path="+filePath);
				
				kbClient = new FileBasedStoreClient(filePath);	
			}else if(isRemoteTargetResourceID(targetResourceID)){

				String targetResourceLabel = getLabelFromTargetResourceID(targetResourceID);
				LOGGER.info("Remote store. targetResourceLabel="+targetResourceLabel);
				
				List<String> endpoints = storeRouter.get(targetResourceLabel);

				if(endpoints != null) {
					if (isQueryOperation) {
						queryIRI = endpoints.get(QUERY_INDEX);
					}
					if (isUpdateOperation) {
						updateIRI = endpoints.get(UPDATE_INDEX);
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
	 * Returns a the query and update endpoints based on a target resource ID
	 * provided as the input.
	 * @param targetResourceID
	 * @return List of endpoints
	 */
	public static List<String> getEndpoints(String targetResourceID){
		
		List<String> endpoints = null;
		
		if (targetResourceID != null && !targetResourceID.isEmpty()) {
			
			//instantiate singleton if not already done so
			getInstance();

			if (isFileBasedTargetResourceID(targetResourceID)) {
				LOGGER.error("Endpoints not supported for file based store.");
			}else if(isRemoteTargetResourceID(targetResourceID)){
				
				String targetResourceLabel = getLabelFromTargetResourceID(targetResourceID);
				LOGGER.info("Remote store. targetResourceLabel="+targetResourceLabel);
				
				endpoints = storeRouter.get(targetResourceLabel);		
				
			}
		}else {
			LOGGER.error("targetResourceID is null.");
		}
		
		return endpoints;
	}
	
	/**
	 * Get store client for ontokgrouter
	 */
	@Override
	public TripleStoreClientInterface getRouterStoreClient() {
		return new RemoteStoreClient(storeRouterEndpoint);
	}

	/**
	 * Get the query endpoint and update endpoint from the ontokgrouter triple store for the targetResourceLabel.
	 *
	 * @param targetResourceLabel
	 * @param storeClient
	 */
	@Override
	public List<String> getFromStore(String targetResourceLabel, TripleStoreClientInterface storeClient){

		ExprFactory exprFactory = new ExprFactory();
		Expr exprRegex = exprFactory.regex(exprFactory.str( QUESTION_MARK.concat(LABEL)), targetResourceLabel, "");

		SelectBuilder builder = new SelectBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
				.addVar( QUESTION_MARK.concat(QUERY_ENDPOINT) )
				.addVar( QUESTION_MARK.concat(UPDATE_ENDPOINT) )
				.addWhere( QUESTION_MARK.concat(RESOURCE), RDF_PREFIX.concat(COLON).concat(RDF_TYPE), ONTOKGROUTER_PREFIX.concat(COLON).concat(TARGET_RESOURCE) )
			    .addOptional( QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_QUERY_ENDPOINT), QUESTION_MARK.concat(QUERY_ENDPOINT) )
				.addOptional(QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_UPDATE_ENDPOINT), QUESTION_MARK.concat(UPDATE_ENDPOINT))
				.addWhere( QUESTION_MARK.concat(RESOURCE), RDFS_PREFIX.concat(COLON).concat(LABEL), QUESTION_MARK.concat(LABEL))
				.addFilter(exprRegex);

		JSONArray results = storeClient.executeQuery(builder.toString());

		if(!results.isEmpty()) {
			//Get first entry
			//Add logic for multiple results?
			JSONObject obj = results.getJSONObject(0);
			String queryEndpoint = MiscUtil.optNullKey(obj, QUERY_ENDPOINT);
			String updateEndpoint = MiscUtil.optNullKey(obj, UPDATE_ENDPOINT);

			List<String> endpoints = new ArrayList<String>();
			endpoints.add(QUERY_INDEX,queryEndpoint);
			endpoints.add(UPDATE_INDEX,updateEndpoint);

			return endpoints;

		}else {
			LOGGER.error("Endpoints not found for resource="+targetResourceLabel);
			return null;
		}
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
		if( FILE_EXTENSIONS.stream().anyMatch(targetResourceID.trim()::endsWith)) {
			
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
	 * @param targetResourceName
	 * @return
	 */
	private String getLocalFilePath(String targetResourceName, TripleStoreClientInterface storeClient) {

		ExprFactory exprFactory = new ExprFactory();
		Expr exprRegex = exprFactory.regex(exprFactory.str( QUESTION_MARK.concat(LABEL)), targetResourceName, "");

		SelectBuilder builder = new SelectBuilder()
				.addPrefix( RDFS_PREFIX,  RDFS )
				.addPrefix( RDF_PREFIX,  RDF )
				.addPrefix( ONTOKGROUTER_PREFIX,  ONTOKGROUTER )
				.addVar( QUESTION_MARK.concat(FILE_PATH) )
				.addWhere( QUESTION_MARK.concat(RESOURCE), RDF_PREFIX.concat(COLON).concat(RDF_TYPE), ONTOKGROUTER_PREFIX.concat(COLON).concat(TARGET_RESOURCE) )
				.addWhere( QUESTION_MARK.concat(RESOURCE), ONTOKGROUTER_PREFIX.concat(COLON).concat(HAS_FILE_PATH), QUESTION_MARK.concat(FILE_PATH) )
				.addWhere( QUESTION_MARK.concat(RESOURCE), RDFS_PREFIX.concat(COLON).concat(LABEL), QUESTION_MARK.concat(LABEL))
				.addFilter(exprRegex);

		JSONArray jsonArray = storeClient.executeQuery(builder.toString());
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			if(obj.getString(LABEL).equals(targetResourceName)){
				System.out.println(obj.get(FILE_PATH));
				return obj.getString(FILE_PATH);
			}
		}
		return null;
	}
}
