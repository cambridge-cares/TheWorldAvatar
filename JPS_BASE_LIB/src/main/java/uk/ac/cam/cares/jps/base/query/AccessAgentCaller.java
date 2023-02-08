package uk.ac.cam.cares.jps.base.query;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

/**
 * The AccessAgentCaller class is used to send HTTP requests to the AccessAgent 
 * to query and update rdf data in triple stores using the methods 
 * {@link uk.ac.cam.cares.jps.base.query.AccessAgentCaller#queryStore queryStore} 
 * and {@link uk.ac.cam.cares.jps.base.query.AccessAgentCaller#updateStore updateStore}, 
 * respectively.
 * <br>
 * These methods can also be accessed in the {@link uk.ac.cam.cares.jps.base.agent.JPSAgent} class.
 * <br>
 * The access agent host:port should be set using the environment variable ACCESSAGENT_HOST.
 * Otherwise, the default host from the jps.properties file is used. Note that if a url is 
 * supplied as the targetResourceID then the host in the url is used.
 *  
 * @author csl37
 *
 */
public class AccessAgentCaller{
		
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessAgentCaller.class);

    public static final String ACCESSAGENT_HOST_NAME = "ACCESSAGENT_HOST";
    public static String accessAgentHost;
    
    //Set the default value for the access agent host
    //TODO this could be done by an "AgentRouter"
    static{
		accessAgentHost = System.getenv(ACCESSAGENT_HOST_NAME);
		if(accessAgentHost == null) {
			// Try get the access agent host from the environment variables
			// if not found, then get from the jps.properties file
			LOGGER.info("ACCESSAGENT_HOST not found in environment variables..."
					+ " Using jps.properties.");
			accessAgentHost = KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST);	
		}
		LOGGER.info("Default ACCESSAGENT_HOST set to "+accessAgentHost);		
	}
    
    /**
	 * Default constructor
	 */
	public AccessAgentCaller() {}	
	
	/**
	 * https://www.w3.org/TR/2013/REC-sparql11-http-rdf-update-20130321/#http-put<br>
	 * The method also allows to put non-RDF resources.
	 * 
	 * @param datasetUrl triple store
	 * @param targetUrl the named resource or graph
	 * @param content
	 * @param contentType
	 * @return
	 */
	public static String put(String datasetUrl, String targetUrl, String content, String contentType) {
		
		LOGGER.info("put for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());
		Object[] a = createRequestUrl(datasetUrl, targetUrl);

		//default to RDF-XML
		if (contentType == null) {
			contentType = MediaType.APPLICATION_RDF_XML.type;
		}
		
        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];
        return Http.execute(Http.put(requestUrl, content, contentType, null, joparams));
	}
	
	/**
	 * cf. https://www.w3.org/TR/2013/REC-sparql11-http-rdf-update-20130321/#http-get<br>
	 * The method also allows to get non-RDF resources. 
	 * 
	 * @param datasetUrl triple store
	 * @param targetUrl the named resource or named graph
	 * @param accept for RDF resources only, available formats see {@link MediaType}, null allowed
	 * @return
	 */
	public static String get(String datasetUrl, String targetUrl, String accept) {
		
        LOGGER.info("get for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

		Object[] a = createRequestUrl(datasetUrl, targetUrl);
		
        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];
        return Http.execute(Http.get(requestUrl, accept, joparams));
    }
	 
	/**
	 * Execute a {@link <a href="https://www.w3.org/TR/sparql11-query/">SPARQL Query</a>} on the target resource.
	 * 
	 * @param targetResourceID 	target namespace or IRI <br>
	 * 							Note: 	If the targetResourceID is a URL/IRI (e.g. "http://localhost:8080/ontokin"), 
	 * 									the request will be sent to the host given in the URL (i.e. localhost:8080).
	 * 									If no host is provided (e.g. targetResourceID = "ontokin"), the request is sent
	 * 									to the host given by the environment variable "ACCESSAGENT_HOST" 
	 * 									or that in jps.properties, if the environment variable is not set.
	 * @param sparqlQuery		SPARQL query string
     * @return the query result in the {@link <a href="https://www.w3.org/TR/sparql11-results-json/">W3C Query result JSON format</a>} 
	 */
	public static JSONArray queryStore(String targetResourceID, String sparqlQuery) {
		//pass the target resource ID directly as the targetUrl
    	//both datasetUrl and targetUrl are not used by the AccessAgent for queries
		//Unpack results into JSONArray
		return new JSONArray(new JSONObject(query(null, targetResourceID, sparqlQuery)).getString(JPSConstants.RESULT_KEY));
	}
	
	/**
	 * @Deprecated Use queryStore instead: results are unpacked into a JSONArray. 
	 */
	@Deprecated
	public static String query(String targetResourceID, String sparqlQuery) {
		//pass the target resource ID directly as the targetUrl
    	//both datasetUrl and targetUrl are not used by the AccessAgent for queries
		return query(null, targetResourceID, sparqlQuery);
	}
	
	/**
	 * cf. https://www.w3.org/TR/sparql11-protocol/#query-via-get<br>
     * differences: parameter key and value are serialized as JSON, the parameter key is
     * "sparqlquery" instead of "query"
	 * 
	 * @param datasetUrl triple store
	 * @param targetUrl the named resource or graph
	 * @param sparqlQuery
     * @return the query result in the W3C Query result JSON format, see
     * https://www.w3.org/TR/sparql11-results-json/
	 */
	public static String query(String datasetUrl, String targetUrl, String sparqlQuery) {

        LOGGER.info("query for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

		Object[] a = createRequestUrl(datasetUrl, targetUrl);
		
        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];
          
     	JSONObject jobody = new JSONObject();
     	jobody.put(JPSConstants.QUERY_SPARQL_QUERY, sparqlQuery);
     	String contentType = MediaType.APPLICATION_JSON.type;
     		
     	return Http.execute(Http.post(requestUrl, jobody.toString(), contentType, null, joparams));		
    }

	/**
     * Execute a {@link <a href="https://www.w3.org/TR/sparql11-update/">SPARQL Update</a>} on the target resource. 
     * @param targetResourceID	the target namespace or IRI <br>
     * 							Note: 	If the targetResourceID is a URL/IRI (e.g. "http://localhost:8080/ontokin"), 
	 * 									the request will be sent to the host given in the URL (i.e. localhost:8080).
	 * 									If no host is provided (e.g. targetResourceID = "ontokin"), the request is sent
	 * 									to the host given by the environment variable "ACCESSAGENT_HOST" 
	 * 									or that in jps.properties, if the environment variable is not set.
     * @param sparqlUpdate		SPARQL update string
     */
	//Duplication of update below for sake of naming consistency with queryStore
	public static void updateStore(String targetResourceID, String sparqlUpdate) {
		//pass the target resource ID directly as the targetUrl
    	//both datasetUrl and targetUrl are not used by the AccessAgent for updates
		update(null, targetResourceID, sparqlUpdate);
    	return;
	}
	
	/**
	 * @deprecated Use updateStore instead. Deprecated to maintain naming consistency with queryStore.
     * Execute a {@link <a href="https://www.w3.org/TR/sparql11-update/">SPARQL Update</a>} on the target resource. 
     * @param targetResourceID	the target namespace or IRI
     * 							e.g. to access the Ontokin triple store
     * 							both "ontokin" and "http://www.theworldavatar.com/kb/ontokin" are accepted.
     * @param sparqlUpdate		SPARQL update string
     */
	@Deprecated
	public static void update(String targetResourceID, String sparqlUpdate) {
		//pass the target resource ID directly as the targetUrl
    	//both datasetUrl and targetUrl are not used by the AccessAgent for updates
    	update(null, targetResourceID, sparqlUpdate);
    	return;
	}
	
	/**
	 * Performs a SPARQL update on the resource identified by its target url (if this possible). 
	 * If a scenario url is given in the JPS context, then the SPARQL update is redirected to the scenario url.
	 * 
	 * @param datasetUrl triple store
	 * @param targetUrl
	 * @param sparqlUpdate
	 */
	public static void update(String datasetUrl, String targetUrl, String sparqlUpdate) {
		
		LOGGER.info("update for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());
		
		Object[] a = createRequestUrl(datasetUrl, targetUrl);
			
		String requestUrl = (String) a[0];
		JSONObject joparams = (JSONObject) a[1];
		
		// According to the W3C standard http://www.w3.org/TR/2013/REC-sparql11-protocol-20130321/
		// there are two ways to send a SPARQL update string. Both ways use an HTTP POST with
		// the SPARQL update string in the message body. They are distinguished by the contentType.
		// However, here we use JSON as content type!
		JSONObject jobody = new JSONObject();
		jobody.put(JPSConstants.QUERY_SPARQL_UPDATE, sparqlUpdate);
		String contentType = MediaType.APPLICATION_JSON.type;
		
		Http.execute(Http.post(requestUrl, jobody.toString(), contentType, null, joparams));	
		return;	
	}
	
	/**
	 * Create the request url and request parameters containing the target resource IRI.
	 * The request is directed to either the ScenarioAccessAgent via the scenarioUrl or
	 * the AccessAgent at "jps/kb". 
	 * @param datasetUrl
	 * @param targetUrl
	 * @return
	 */
	public static Object[] createRequestUrl(String datasetUrl, String targetUrl) {
		
		// The following cases have to be distinguished:
		// 1) no datasetUrl is given, no scenarioUrl in the JPS context
		//	  the targetUrl is requested directly
		// 2) the datasetUrl is given, no scenarioUrl in the JPS context
		//	  the targetUrl may optionally request a graph at the datasetUrl
		// 3) scnearioUrl in the JPS context
		// 	  in combination with corresponding cases from 1) and 2)
		//
		// If no host is provided as part of the datasetUrl (case 2) or targetUrl (case1),
		// a requestUrl will be constructed using the host stored in JPSConstants
		
		String scenarioUrl = JPSContext.getScenarioUrl();			
		String requestUrl = null;
		
		if ((datasetUrl != null) && datasetUrl.isEmpty()) {
			datasetUrl = null;
		}

		JSONObject joparams = null;
		
		//case 3 
		if (scenarioUrl != null)  {			
			// redirect the request to the scenario agent
			// the scenario agent has to be called even for get / query in combination with copy-on-write since in previous calls
			// another agent might have updated the file within the same scenario 
			requestUrl = scenarioUrl;
			
			joparams = new JSONObject();
			String resource = cutHashFragment(targetUrl);
			joparams.put(JPSConstants.SCENARIO_RESOURCE, resource);
			if (datasetUrl != null) {
				if (targetUrl == null) {
					joparams.put(JPSConstants.SCENARIO_RESOURCE, datasetUrl);
				} else {
					joparams.put(JPSConstants.SCENARIO_DATASET, datasetUrl);
				}
			}
			
		//case 2
		}else if(datasetUrl != null) {
			//request targetUrl indirectly as graph in datasetUrl
			requestUrl = getBaseWorldUrl(datasetUrl);
			
			joparams = new JSONObject();
			joparams.put(JPSConstants.TARGETIRI, cutHashFragment(datasetUrl));
			joparams.put(JPSConstants.TARGETGRAPH, cutHashFragment(targetUrl));
			
		//case 1
		}else {
			//request targetUrl directly
			requestUrl = getBaseWorldUrl(targetUrl);
			
			joparams = new JSONObject();
			joparams.put(JPSConstants.TARGETIRI, cutHashFragment(targetUrl));
		}
	
		Object[] a = new Object[] {requestUrl, joparams};			
		return a;
	}	
	
	/**
	 * Apache HTTP client applies percentage encoding to any URL.
	 * Usually, this is not a problem when requesting an OWL file. 
	 * But if requesting http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service 
	 * then percentage encoding results into http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl%23Service
	 * and a consecutive Tomcat error.
	 * To avoid %23 instead of #, we simply skip the #-part by applying this method to the requested Url.
	 * 
	 * @return
	 * @throws URISyntaxException 
	 * @throws UnsupportedEncodingException 
	 */
	public static String cutHashFragment(String url) {

		if (url == null) {
			return null;
		}
		
		URI uri = null;
		try {
			uri = new URI(URLDecoder.decode(url,"UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new JPSRuntimeException(e);
		} catch (URISyntaxException e) {
			throw new JPSRuntimeException(e);
		}
		
		if(uri.getScheme()!=null) {
			return uri.getScheme() + ":" + uri.getSchemeSpecificPart();	
		}else {
			return uri.getSchemeSpecificPart();
		}
	}
	
	/**
	 * Get the base world Access Agent url.
	 * The scheme, host and port of the target or dataset url are preserved, while
	 * the path is changed to the Access Agent Path. 
	 * If no scheme, host or port is provided then url will default to the values 
	 * provided by JPSConstants e.g. "http://www.theworldavatar.com/access-agent/access"
	 * @param url
	 * @return
	 */
	public static String getBaseWorldUrl(String url) {
	
		URI requestUrl = null;
		try {
			URI uri = new URI(URLDecoder.decode(url,"UTF-8"));
			String scheme = uri.getScheme();
			String authority = uri.getAuthority();
			
			//If no scheme is provided then default to HTTP
			if(scheme == null) {
				scheme = "http";
			}
			
			//If no authority is given then get the default host
			if(authority == null) {				
				authority = accessAgentHost;
			}
			
			requestUrl = new URI(scheme,authority,JPSConstants.ACCESS_AGENT_PATH,null,null);
			
		} catch (UnsupportedEncodingException e) {
			throw new JPSRuntimeException(e);
		} catch (URISyntaxException e) {
			throw new JPSRuntimeException(e);
		}
		
		return requestUrl.toString();
	}
}
