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
}
