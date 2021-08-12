package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;


import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class AccessAgentCaller{
		
	/**
	 * Default constructor
	 */
	public AccessAgentCaller() {}	
	
	/**
	 * https://www.w3.org/TR/2013/REC-sparql11-http-rdf-update-20130321/#http-put<br>
	 * The method also allows to put non-RDF resources.
	 * 
	 * @param targetUrl
	 * @param content
	 * @return
	 */
	public static String put(String datasetUrl, String targetUrl, String content, String contentType) {
		
		JPSBaseLogger.info(AccessAgentCaller.class, "put for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());
		Object[] a = createRequestUrl(datasetUrl, targetUrl, true);
		
		if (a != null) {
			String requestUrl = (String) a[0];
			JSONObject joparams = (JSONObject) a[1];
			return Http.execute(Http.put(requestUrl, content, contentType, null, joparams));
		} 
		
		// case 1b
		// this case can only happen if datasetUrl AND targetUrl is null which must not be the case when calling this method
		throw new JPSRuntimeException("No requestUrl was created");
	}	
	
	/**
	 * cf. https://www.w3.org/TR/2013/REC-sparql11-http-rdf-update-20130321/#http-get<br>
	 * The method also allows to get non-RDF resources. 
	 * 
	 * @param datasetUrl 
	 * @param targetUrl the named resource or named graph
	 * @param accept for RDF resources only, available formats see {@link MediaType}, null allowed
	 * @return
	 */
	public static String get(String datasetUrl, String targetUrl, String accept) {
		
		JPSBaseLogger.info(AccessAgentCaller.class, "get for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

		Object[] a = createRequestUrl(datasetUrl, targetUrl, true);
		
		if (a != null) {
			String requestUrl = (String) a[0];
			JSONObject joparams = (JSONObject) a[1];
			return Http.execute(Http.get(requestUrl, accept, joparams));
		} 
		
		// case 1b
		// this case can only happen if datasetUrl AND targetUrl is null which must not be the case when calling this method
		throw new JPSRuntimeException("No requestUrl was created");
	}
	
	/**
	 * cf. https://www.w3.org/TR/sparql11-protocol/#query-via-get<br>
	 * differences: parameter key and value are serialized as JSON,  
	 * the parameter key is "sparqlquery" instead of "query"
	 * 
	 * @param datasetUrl
	 * @param targetUrl
	 * @param sparqlQuery
	 * @return the query result in the W3C Query result JSON format, see https://www.w3.org/TR/sparql11-results-json/
	 */
	public static String query(String datasetUrl, String targetUrl, String sparqlQuery) {
		
		// the following cases have to be distinguished:
		// 1) no datasetUrl is given, no scenarioUrl in the JPS context
		// 1a) HTTP GET on target resource allows to perform SPARQL at the server 
		// 1b) HTTP GET on target resource to download its content but the SPARQL query must be performed at this client
		//	   (this is the case for most of the resources outside JPS control but also for files residing 
		//     in /kb or /data within Tomcats ROOT directory)
		// 2) the datasetUrl is given, no scenarioUrl in the JPS context
		//	  This means that the target resource is only requested indirectly via the datasetUrl 
		// 	  as SPARQL endpoint (such that SPARQL is performed at the endpoint)
		// 3) scnearioUrl in the JPS context
		// 	  in combination with corresponding cases from 1) and 2)
		
		JPSBaseLogger.info(AccessAgentCaller.class, "query for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

		boolean sparqlAbility = hasSparqlAbility(targetUrl);
		Object[] a = createRequestUrl(datasetUrl, targetUrl, sparqlAbility);
		
		if (a != null) {
			System.out.println("a IS NOT NULL!!!");
			String requestUrl = (String) a[0];
			JSONObject joparams = (JSONObject) a[1];
			if (joparams == null) {
				joparams = new JSONObject();
			}
			System.out.println("joparams="+joparams.toString());
			System.out.println("REQUESTURL="+requestUrl);
			joparams.put(JPSConstants.QUERY_SPARQL_QUERY, sparqlQuery);
			return Http.execute(Http.get(requestUrl, null, joparams));
		} 
		
		// case 1b
		JPSBaseLogger.info(AccessAgentCaller.class, "SPARQL query is performed locally for targetUrl=" + targetUrl);
		String localUrl = ScenarioHelper.cutHash(targetUrl);
		localUrl = ResourcePathConverter.convert(localUrl);
		ResultSet resultSet = JenaHelper.queryUrl(localUrl, sparqlQuery);
		return JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	}
	
	public static Object[] createRequestUrl(String datasetUrl, String targetUrl, boolean targetHasSparqlAbility) {
		
		// the same cases as described in method query have to be distinguished
		
		String scenarioUrl = JPSContext.getScenarioUrl();			
		String requestUrl = null;
		
		if ((datasetUrl != null) && datasetUrl.isEmpty()) {
			datasetUrl = null;
		}
		
		// case 3 or case 2 or case 1a
		if ((scenarioUrl != null) || (datasetUrl != null) || targetHasSparqlAbility)  {	
		
			JSONObject joparams = null;
		
			// case 3
			if (scenarioUrl != null)  {				
				// redirect the request to the scenario agent
				// the scenario agent has to be called even for get / query in combination with copy-on-write since in previous calls
				// another agent might have updated the file within the same scenario 
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
				requestUrl = scenarioUrl;
			// case 2
			} else if (datasetUrl != null) {
				joparams = new JSONObject();
				String resource = cutHashFragment(targetUrl);
				joparams.put(JPSConstants.SCENARIO_RESOURCE, resource);
				requestUrl = datasetUrl;
			// case 1a
			} else {
				requestUrl = cutHashFragment(targetUrl);
			}
		
			//requestUrl = ScenarioHelper.cutHash(requestUrl);
			requestUrl = ResourcePathConverter.convert(requestUrl);
		
			Object[] a = new Object[] {requestUrl, joparams};			
			return a;
		} 
		
		// case 1b
		return null;
	}	
	
	
	/**
	 * Performs a SPARQL update on the resource identified by its target url (if this possible). 
	 * If a scenario url is given in the JPS context, then the SPARQL update is redirected to the scenario url.
	 * 
	 * @param targetUrl
	 * @param sparqlUpdate
	 */
	public static void update(String datasetUrl, String targetUrl, String sparqlUpdate) {
		
		JPSBaseLogger.info(AccessAgentCaller.class, "update for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

		boolean sparqlAbility = hasSparqlAbility(targetUrl);
		Object[] a = createRequestUrl(datasetUrl, targetUrl, sparqlAbility);
		
		if (a != null) {
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
		
		// case 1b
		String requestUrl = ScenarioHelper.cutHash(targetUrl);
//		requestUrl = ResourcePathConverter.convertToLocalPath(requestUrl);
		requestUrl = ResourcePathConverter.convert(requestUrl);
		JPSBaseLogger.info(AccessAgentCaller.class, "SPARQL update is performed locally for requestUrl=" + requestUrl);
		UpdateRequest request = UpdateFactory.create(sparqlUpdate);
		OntModel model = JenaHelper.createModel(requestUrl);	
		UpdateAction.execute(request, model);
		JenaHelper.writeAsFile(model, requestUrl);		
	}
	
	private static boolean hasSparqlAbility(String targetUrl) {
		if (targetUrl == null) {
			return false;
		}
		return targetUrl.contains("/" + JPSConstants.KNOWLEDGE_BASE_JPS + "/");
	}
	
	/**
	 * Apache HTTP client applies percentage encoding to any URL.
	 * Usually, this is not a problem when requesting an OWL file. 
	 * But if requesting http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service 
	 * then percentage encoding results into http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl%23Service
	 * and a consecutive Tomcat error.
	 * To avoid %23 instead of #, we simply skip the #-part by applying this methode to the requested Url.
	 * 
	 * @return
	 */
	public static String cutHashFragment(String url) {
		if (url == null) {
			return null;
		}
		int i = url.lastIndexOf("#");
		if (i >= 0) {
			return url.substring(0, i);
		}
		return url;
	}

}
