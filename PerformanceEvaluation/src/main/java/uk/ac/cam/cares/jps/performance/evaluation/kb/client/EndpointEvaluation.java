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
}
