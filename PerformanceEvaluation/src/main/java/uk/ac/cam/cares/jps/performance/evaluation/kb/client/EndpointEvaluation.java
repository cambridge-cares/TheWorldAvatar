package uk.ac.cam.cares.jps.performance.evaluation.kb.client;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.jayway.jsonpath.JsonPath;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.performance.evaluation.kb.client.configuration.EndpointConfiguration;
import uk.ac.cam.cares.jps.performance.evaluation.kb.client.configuration.EndpointProperty;

/**
 * This class evaluates the query performance of any group of end points<p>
 * connected from KnowledgeBaseClinet based on the information provided<p>
 * in the property file src/main/resources/endpoint.properties.
 * 
 * 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class EndpointEvaluation {
	/**
	 * Creates a logger that traces info, severe and other messages that<p>
	 * would be useful in debugging. 
	 */
	private static final Logger log = Logger.getLogger(EndpointEvaluation.class.getName());
	/**
	 * A member variable declared to hold an instance of ApplicationContext.
	 */
	public static ApplicationContext applicationContextEndpointEvaluation;
	/**
	 * A member variable declared to hold an instance of EndpointProperty.
	 */
	public static EndpointProperty endpointProperty;
	/**
	 * A hash map that contains the performance metrices. 
	 */
	public static Map<String, Map<String, String>> performanceMetrices = new HashMap<String, Map<String, String>>(); 

	public static final String QUERY_FILE_NAME = "queries.json";  
	/**
	 * This is the main method of the class developed as the starting point<p>
	 * of the End Point Evaluation tool.
	 * 
	 * @param args
	 */
	public static void main(String[] args) throws IOException{
		// Instantiates the EndpointEvaluation class to call different<p>
		// methods of the class.
		EndpointEvaluation epEval = new EndpointEvaluation();
		// Calls the init() method
		epEval.init();
		epEval.prepareQuery();
		epEval.performQuery();
		epEval.compare();
	}
	
	/**
	 * Prepares the group of queries for each end point.
	 * 
	 * @throws IOException
	 */
	private void prepareQuery() throws IOException{
		// Reads the end point URLs from the property file.
		String[] endpoints = endpointProperty.getEndpointURLs().split(",");
		Map<String, String> executionTimes;
		for(String endpoint:endpoints){
			executionTimes = new HashMap<String, String>();
			performanceMetrices.put(endpoint, executionTimes);
		}
		System.out.println("path:"+getClass().getClassLoader().getResource("endpoint.properties").getPath());
		InputStream inputStream = new FileInputStream(getClass().getClassLoader().getResource(QUERY_FILE_NAME).getPath());
		String jsonString = FileUtil.inputStreamToString(inputStream);
		for (String endpoint : performanceMetrices.keySet()) {
			List<String> queries = readJsonValue(jsonString);
			for (String query : queries) {
				performanceMetrices.get(endpoint).put(query, null);
			}
		}
	}
	/**
	 * Performs all queries against each end point and records the execution<p>
	 * time in the hash map called performanceMetrices.
	 */
	private void performQuery(){
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient();
		Map<String, String> queryVsTime;
		for(String endpoint:performanceMetrices.keySet()){
			kbClient.setQueryEndpoint(endpoint);
			for(String query:performanceMetrices.get(endpoint).keySet()){
				kbClient.setQuery(query);
				try{
					long start = System.currentTimeMillis();
					JSONArray result = kbClient.executeQuery();
					long time = System.currentTimeMillis() - start;
					queryVsTime = performanceMetrices.get(endpoint);
					queryVsTime.replace(query, null, time+"");
					if(result==null){
						System.out.println("No result was returned from:"+endpoint);
					}else{
						System.out.println("Result from:"+endpoint+": "+result.toString());
					}
				}catch(SQLException e){
					log.severe(e.getMessage());
				}
			}
		}
	}
	
	/**
	 * Compares the query performance of remote knowledge repository end points. 
	 */
	private void compare(){
		System.out.println("------------------------Endpoint------------------------, -------------Query-------------, time (ms)");
		for(String endpoint:performanceMetrices.keySet()){
			int queryCount = 0;
			for(String query:performanceMetrices.get(endpoint).keySet()){
				System.out.println(endpoint+", Query ["+ ++queryCount +"] "+ query);
				System.out.println("Time:"+performanceMetrices.get(endpoint).get(query));
			}
		}
	}
	
	/**
	 * Initialises the instances of some classes that allows to read properties<p>
	 *  from the file src/main/resources/endpoint.properties.<p>
	 */
	private void init(){
		if (applicationContextEndpointEvaluation == null) {
			applicationContextEndpointEvaluation = new AnnotationConfigApplicationContext(EndpointConfiguration.class);
		}
		if (endpointProperty == null) {
			endpointProperty = applicationContextEndpointEvaluation.getBean(EndpointProperty.class);
		}
	}
	
	/**
	 * Retrieves the value from a JSON string if key is provided.
	 * 
	 * @param jsonString
	 * @param queryKey
	 * @return
	 */
	private List<String> readJsonValue(String jsonString){
				return JsonPath.read(jsonString, "$.query");
	}
}
