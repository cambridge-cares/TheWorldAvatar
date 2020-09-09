package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.apache.jena.jdbc.JenaDriver;
import org.apache.jena.jdbc.remote.RemoteEndpointDriver;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.util.List;
import java.util.logging.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class KnowledgeBaseClient {
	private static final Logger log = Logger.getLogger(KnowledgeBaseClient.class.getName());
	private static KnowledgeBaseClient instance = null;
	private static final String HTTP_PROTOCOL_PREFIX = "http:";
	
	private String queryEndpoint;
	private String updateEndpoint;
	private String query;
	
	private static synchronized KnowledgeBaseClient getInstance() {
		if (instance == null) {
			instance = new KnowledgeBaseClient();
		}
		return instance;
	}

	/*
	 * The default constructor.
	 */
	public KnowledgeBaseClient(){
		
	}
	
	/**
	 * A constructor defined to initialise only the query EndPoint URL.
	 * 
	 * @param queryEndpoint
	 */
	public KnowledgeBaseClient(String queryEndpoint){
		this.queryEndpoint = queryEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL and a set of graphs to send a data retrieval or update<p>
	 * query.    
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 */
	public KnowledgeBaseClient(String queryEndpoint, String updateEndpoint){
		this.queryEndpoint = queryEndpoint;
		this.updateEndpoint = updateEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL and a set of graphs to send a data retrieval or update<p>
	 * query.    
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 * @param query
	 */
	public KnowledgeBaseClient(String queryEndpoint, String updateEndpoint, String query){
		this.queryEndpoint = queryEndpoint;
		this.updateEndpoint = updateEndpoint;
		this.query = query;
	}
	
	/**
	 * Generates the URL of the remote data repository's EndPoint either to<p>
	 * perform a data retrieval or an update query.  
	 * 
	 * @return
	 */
	protected String getConnectionUrl() {
		StringBuilder sb = new StringBuilder();
		boolean queryFlag = false;
		sb.append(JenaDriver.DRIVER_PREFIX);
		sb.append(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX);
		if (this.queryEndpoint != null) {
			queryFlag = true;
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT, this.queryEndpoint));
		}
		if (this.updateEndpoint != null) {
			if(queryFlag){
				sb.append("&");
			}
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT, this.updateEndpoint));
		}
		return sb.toString();
	}

	/**
	 * Puts the type of an endpoint (e.g. query and update), equal to symbol<p>
	 * and end point URL in a string and returns the string.
	 * 
	 * @param endpointType
	 * @param endpointURL
	 * @return
	 */
	private String generateEndpointProperty(String endpointType, String endpointURL){
		StringBuilder sb = new StringBuilder();
		sb.append(endpointType);
		sb.append("=");
		sb.append(endpointURL);
		return sb.toString();
	}	
	
	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method.
	 * 
	 * @return
	 */
	public JSONObject executeQuery() throws SQLException{
		String connectionUrl = getConnectionUrl();
		if(connectionUrl.isEmpty()){
			return null;
		}
		if(isConnectionUrlValid(connectionUrl)){
			executeQuery(this.query);
		}
		return null;
	}
	
	/**
	 * Executes the query supplied by the calling method and returns results. 
	 * 
	 * @param query
	 * @return
	 */
	public JSONObject executeQuery(String query) throws SQLException{
        JSONObject results = new JSONObject();
		Connection conn = null;
        Statement stmt = null;
        try {
            RemoteEndpointDriver.register();
            System.out.println(getConnectionUrl());
            conn = DriverManager.getConnection(getConnectionUrl());
            stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
			System.out.println(query);
            java.sql.ResultSet rs = stmt.executeQuery(query);
            while (rs.next()) {
				// Print out type as a string
				System.out.println(rs.getString("reactionMechanism"));
			}
        } catch (SQLException e) {
        	throw new SQLException(e.getMessage());
        }
        return results;
	}
	
	/**
	 * Checks the validity of the URL generated for connecting to a remote<p>
	 * repository based on user provided inputs via one of the parameterised<p>
	 * constructors or setter methods.
	 * 
	 * @param connectionUrl provided URL that is to be used for establishing<p>
	 * a connection with the remote repository to perform query or update<p>
	 * operations.
	 * @return
	 */
	public boolean isConnectionUrlValid(String connectionUrl){
		if (!connectionUrl.startsWith(JenaDriver.DRIVER_PREFIX
				.concat(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX)
				.concat(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT)
				.concat(HTTP_PROTOCOL_PREFIX))) {
			return false;
		}
		String[] tokens = connectionUrl.split(HTTP_PROTOCOL_PREFIX);
		for(String token: tokens){
			if(token.isEmpty()){
				return false;
			}
			if(token.length()<3){
				return false;
			}
		}
		return true;
	}
	
	/**
	 * https://www.w3.org/TR/2013/REC-sparql11-http-rdf-update-20130321/#http-put<br>
	 * The method also allows to put non-RDF resources.
	 * 
	 * @param targetUrl
	 * @param content
	 * @return
	 */
	public static String put(String datasetUrl, String targetUrl, String content, String contentType) {
		
		JPSBaseLogger.info(getInstance(), "put for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());
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
		
		JPSBaseLogger.info(getInstance(), "get for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

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
		
		JPSBaseLogger.info(getInstance(), "query for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

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
		JPSBaseLogger.info(getInstance(), "SPARQL query is performed locally for targetUrl=" + targetUrl);
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
		
		JPSBaseLogger.info(getInstance(), "update for datasetUrl=" + datasetUrl + ", targetUrl=" + targetUrl + ", scenarioUrl=" + JPSContext.getScenarioUrl());

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
		JPSBaseLogger.info(getInstance(), "SPARQL update is performed locally for requestUrl=" + requestUrl);
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
	
	/**
	 * Can return the URL of the query EndPoint.  
	 * 
	 * @return
	 */
	public String getQueryEndpoint() {
		return queryEndpoint;
	}

	/**
	 * Sets the URL of the query EndPoint if provided. 
	 * 
	 * @param queryEndpoint
	 */
	public void setQueryEndpoint(String queryEndpoint) {
		this.queryEndpoint = queryEndpoint;
	}
	
	/**
	 * Returns the URL of the update EndPoint if available.
	 * 
	 * @return
	 */
	public String getUpdateEndpoint() {
		return updateEndpoint;
	}
	
	/**
	 * Set the URL of the update EndPoint if provided.
	 * 
	 * @param updateEndpoint
	 */
	public void setUpdateEndpoint(String updateEndpoint) {
		this.updateEndpoint = updateEndpoint;
	}

	/**
	 * Returns the available query.
	 * 
	 * @return
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * Sets a query if provided.
	 * 
	 * @param query
	 */
	public void setQuery(String query) {
		this.query = query;
	}

}
