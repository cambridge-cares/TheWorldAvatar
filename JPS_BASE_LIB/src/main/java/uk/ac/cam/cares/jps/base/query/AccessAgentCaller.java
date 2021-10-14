package uk.ac.cam.cares.jps.base.query;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

public class AccessAgentCaller{
		
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AccessAgentCaller.class);

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
	 * @param targetResourceID 	target namespace or IRI
     * 							e.g. to access the Ontokin triple store
     * 							both "ontokin" and "http://www.theworldavatar.com/kb/ontokin" are accepted.
	 * @param sparqlQuery		SPARQL query string
     * @return the query result in the {@link <a href="https://www.w3.org/TR/sparql11-results-json/">W3C Query result JSON format</a>} 
	 */
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
		
        System.out.println("a IS NOT NULL!!!");
        String requestUrl = (String) a[0];
        JSONObject joparams = (JSONObject) a[1];
        if (joparams == null) {
            joparams = new JSONObject();
        }
        System.out.println("joparams=" + joparams.toString());
        System.out.println("REQUESTURL=" + requestUrl);
        joparams.put(JPSConstants.QUERY_SPARQL_QUERY, sparqlQuery);
        return Http.execute(Http.get(requestUrl, null, joparams));
    }

	/**
     * Execute a {@link <a href="https://www.w3.org/TR/sparql11-update/">SPARQL Update</a>} on the target resource. 
     * @param targetResourceID	the target namespace or IRI
     * 							e.g. to access the Ontokin triple store
     * 							both "ontokin" and "http://www.theworldavatar.com/kb/ontokin" are accepted.
     * @param sparqlUpdate		SPARQL update string
     */
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
	
		requestUrl = ResourcePathConverter.convert(requestUrl);
	
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
			//If no authority is given then use the default host 
			if(authority == null) {
				authority = JPSConstants.ACCESS_AGENT_HOST;
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
