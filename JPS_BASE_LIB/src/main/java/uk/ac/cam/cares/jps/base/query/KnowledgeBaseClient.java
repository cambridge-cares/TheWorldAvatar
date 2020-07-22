package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;

import java.util.logging.Logger;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.GraphQueryResult;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.rio.RDFFormat;

import com.bigdata.rdf.sail.webapp.SD;
import com.bigdata.rdf.sail.webapp.client.IPreparedTupleQuery;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository.AddOp;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class KnowledgeBaseClient {
	private static final Logger log = Logger.getLogger(KnowledgeBaseClient.class.getName());
	private static KnowledgeBaseClient instance = null;
	
	private static synchronized KnowledgeBaseClient getInstance() {
		if (instance == null) {
			instance = new KnowledgeBaseClient();
		}
		return instance;
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
	
	/**
	 * Creates the instance of the current repository (knowledge base) if it<br>
	 * exists and returns it.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 */
	public RemoteRepository getRepository(String endPointURL, String repositoryName, RDFStoreType storeType) throws Exception{
		RemoteRepository repository = null;
		if(storeType.toString().equals(RDFStoreType.BLAZEGRAPH.toString())){
			RemoteRepositoryManager repositoryManager = new RemoteRepositoryManager(endPointURL, false);
			if(repositoryExists(endPointURL, repositoryName, repositoryManager)){
				repository = repositoryManager.getRepositoryForNamespace(repositoryName);
				repositoryManager.close();
				return repository; 
			}
		}
		return repository;
	}
	
	/**
	 * Checks the availability of a repository (knowledge base) on a triple store.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param repositoryManager an instance of the repository manager.
	 * @return
	 * @throws Exception
	 */
	private boolean repositoryExists(String endPointURL, String repositoryName, RemoteRepositoryManager repositoryManager) throws Exception{
		final GraphQueryResult res = repositoryManager.getRepositoryDescriptions();
		try{
			while(res.hasNext()){
				final Statement stmt = res.next();
				if (stmt.getPredicate().toString().equals(SD.KB_NAMESPACE.stringValue())) {
					if(repositoryName.equals(stmt.getObject().stringValue())){
						return true;
					}
				}
			}
		} finally {
			res.close();
		}
		return false;
	}
	
	/**
	 * Uploads a single ontology file to the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyFilePath the absolute path to the ontology file, e.g. 
	 * C:/path/to/the/ontology/ontokin.owl and C:/path/to/the/ontology/ABF.owl.
	 * @throws Exception
	 */
	public void uploadOntology(String endPointURL, String repositoryName, String ontologyFilePath)
			throws Exception {
		RemoteRepository repository = getRepository(endPointURL, repositoryName,
				RDFStoreType.BLAZEGRAPH);
		if (repository != null) {
			final InputStream is = new FileInputStream(new File(ontologyFilePath));
			try {
				repository.add(new AddOp(is, RDFFormat.forMIMEType("application/xml")));
			} finally {
				is.close();
			}
		} else{
			log.info("The following repository does not exist: "+endPointURL+repositoryName);
			log.info("Create a repository with this name and try again.");
		}
	}
	
	/**
	 * Uploads all ontology files available under the given folder to<br>
	 * the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyDirectory the path to the folder containing a list<br>
	 * of ontologies, e.g. C:/path/to/the/ontology_folder.
	 * @throws Exception
	 */
	public void uploadOntologies(String endPointURL, String repositoryName, String ontologyDirectory) throws Exception{
		File dir = new File(ontologyDirectory);
		if(dir.isDirectory()){
			int i = 0;
			for(File file:dir.listFiles()){
				if(file.isFile()){
					uploadOntology(endPointURL, repositoryName, file.getAbsolutePath());
					log.info("["+ ++i+"] Uploaded "+file.getAbsolutePath());
				}
			}
		}
	}

	/**
	 * Performs any SPARQL query against the provided repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 * @param query the query that is being performed.
	 * @return
	 * @throws Exception
	 */
	public String query(String endPointURL, String repositoryName, RDFStoreType storeType, String query) throws Exception {
		StringBuilder json = new StringBuilder();
		RemoteRepository repository = getRepository(endPointURL, repositoryName, storeType);
		final IPreparedTupleQuery tupleQuery = repository.prepareTupleQuery(query);
		final TupleQueryResult result = tupleQuery.evaluate();
		System.out.println(result);
		try {
			json = getResultInJson(json, result);	
		} finally {
			result.close();
		}
		return json.toString();
	}

	/**
	 * Produces and returns the given result in JSON format.
	 * 
	 * @param json
	 * @param result
	 * @return
	 */
	private StringBuilder getResultInJson(StringBuilder json, TupleQueryResult result) {
		json.append("{\n");
		json.append("  \"head\" : {\n");
		json.append("    \"vars\" : [\n");
		try{
		// flag to close the header variables created above and to start the results
		boolean flag = true; 
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();
			int count = 0;
			int size = solution.getBindingNames().size();
			if(flag){
				for(String bindingName: solution.getBindingNames()){
					json.append("      \"");
					json.append(bindingName);
					json.append("\"");
					if(++count<size){
						json.append(",");
					}
					json.append("\n");
				}
				json.append("    ]\n");
				json.append("  },\n");
				json.append("  \"results\" : {\n");
				json.append("    \"bindings\" : [\n");
				flag = false;
			}
			count = 0;
			json.append("      {\n");
			for (String bindingName : solution.getBindingNames()) {
				json.append("        \"");
				json.append(bindingName);
				json.append("\" : {\n");
				json.append("          \"value\" : ");
				json.append(jsonifyString(solution.getValue(bindingName).toString()));
				json.append("\n        }");
				if(++count<size){
					json.append(",\n");
				}else{
					json.append("\n");
				}
			}
			json.append("      },\n");
		}
		json.replace(json.lastIndexOf(","), json.lastIndexOf(",")+1, "");
		}catch(QueryEvaluationException e){
			log.info(e.getMessage());
		}
		json.append("    ]\n");
		json.append("  }\n");
		json.append("}\n");
		return json;
	}
	
	/**
	 * Converts a value string into its JSON equivalent.</br>
	 * However, currently it cannot produce a valid JSON equivalent</br>
	 * for comments.
	 * 
	 * @param value
	 * @return
	 */
	private String jsonifyString(String value){
		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
		if(value.contains(stringType)){
			value = value.replace(stringType, "");
		} else if(value.contains(integerType)){
			value = value.replace(integerType, "");
			value = replaceInvertedComma(value);
		} else if(value.contains(floatType)){
			value = value.replace(floatType, "");
			value = replaceInvertedComma(value);
		} else {
			value = "\""+value+"\"";
		}
		return value;
	}
	
	/**
	 * Removes the start and end inverted commas from a string.
	 * 
	 * @param value
	 * @param type
	 * @return
	 */
	private String replaceInvertedComma(String value){
		if(value.startsWith("\"")){
			value = value.replaceFirst("\"", "");
		}
		if(value.endsWith("\"")){
			value = value.substring(0, value.length()-1);
		}		
		return value;
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
