package uk.ac.cam.cares.jps.scenario.testagent;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

// TODO-AE SC 20190218 delete this call after full migration to JPS BASE
//@WebServlet(urlPatterns = {"/EmissionTestAgent/*"}) 
@WebServlet(urlPatterns = {"/xxxxx/*"}) 
public class EmissionTestAgent extends JPSHttpServlet {
	
	private static final long serialVersionUID = -1180303749059632458L;
	private static Logger logger = LoggerFactory.getLogger(EmissionTestAgent.class);

	public static final String SPARQL_PREFIXES = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n"
			+ "PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n"
			+ "PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n"
			+ "PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n"
			+ "PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n"
			+ "PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n"
			+ "PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n"
			+ "PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n";
	
	
	// TODO-AE SC URGENT reuse other queries and prefixes, replace PREFIX : by keyword BASE
	public final static String SPARQL_EMISSION = SPARQL_PREFIXES + 
			"SELECT * \r\n" + 
			"WHERE {\r\n" + 
			"?generation system_performance:hasEmission ?emission .\r\n" + 
			"?emission system:hasValue ?emissionvalue . \r\n" + 
			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
			"}\r\n" + 
			"LIMIT 100";
	
//	public final static String SPARQL_EMISSION = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n" + 
//			"PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n" + 
//			"PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n" + 
//			"PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
//			"PREFIX system:	<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
//			"PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n" + 
//			"PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n" + 
//			"PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n" + 
//			"SELECT * \r\n" + 
//			"WHERE {\r\n" + 
//			"?generation system_performance:hasEmission ?emission .\r\n" + 
//			"?emission system:hasValue ?emissionvalue . \r\n" + 
//			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
//			"}\r\n" + 
//			"LIMIT 100";
	
	private static final String SPARQL_PLANT_UPDATE_EMISSION = SPARQL_PREFIXES 
			+ "DELETE { ?emissionvalue system:numericalValue ?emissionvaluenum .} "
			+ "INSERT { ?emissionvalue system:numericalValue %f .} "
			+ "WHERE { <%s> technical_system:realizes ?generation . ?generation system_performance:hasEmission ?emission . ?emission system:hasValue ?emissionvalue . "
			+ "?emissionvalue system:numericalValue ?emissionvaluenum . }";

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getPathInfo();
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		try {
			String powerplant = jo.getString("plant");
		
			logger.info("called with path=" + path + ", plant=" + powerplant);
			logger.info("scenarioURL=" + ThreadContext.get("scenariourl"));
			
			if ("/read".equals(path)) {
			
				String result = new QueryBroker().readFile(powerplant);
				result = result.substring(0, 400);
				logger.info("read result (400 signs only):\n");
				logger.info(result);
				
				AgentCaller.printToResponse(result, response);
			
			} else if ("/queryemission".equals(path)) {
			
				String result = new QueryBroker().queryFile(powerplant, SPARQL_EMISSION);
				logger.info("query result:\n");
				logger.info(result);
				
				AgentCaller.printToResponse(result, response);
			
			} else if ("/increaseemission".equals(path)) {
				
				int increment = jo.getInt("increment");
			
				String result = new QueryBroker().queryFile(powerplant, SPARQL_EMISSION);
				JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
				double emission = list.getJSONObject(0).getDouble("emissionvaluenum");
				logger.info("increasing the current emission value=" + emission + " by increment=" + increment);

				// TODO-AE SC URGENT 20190215 implement increment for EmissionTestAgent
				
				//AgentCaller.printToResponse(result, response);
			}
			
			
			
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
