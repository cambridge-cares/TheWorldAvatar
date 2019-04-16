package uk.ac.cam.cares.jps.base.test;

import java.io.IOException;
import java.util.StringTokenizer;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = {"/EmissionTestAgent/*"}) 
public class EmissionTestAgent extends JPSHttpServlet {
	
	private static final long serialVersionUID = -1180303749059632458L;
	
	// TODO-AE SC URGENT reuse other queries and prefixes, replace PREFIX : by keyword BASE
	public static final String SPARQL_PREFIXES = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n"
			+ "PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n"
			+ "PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n"
			+ "PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n"
			+ "PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n"
			+ "PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n"
			+ "PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n"
			+ "PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n";	
	
	public final static String SPARQL_PLANT_QUERY_EMISSION = SPARQL_PREFIXES + 
			"SELECT * \r\n" + 
			"WHERE {\r\n" + 
			//"<%s> technical_system:realizes ?generation . \r\n" + 
			"?generation system_performance:hasEmission ?emission .\r\n" + 
			"?emission system:hasValue ?emissionvalue . \r\n" + 
			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
			"}\r\n" + 
			"LIMIT 100";
	
	public static final String SPARQL_PLANT_UPDATE_EMISSION = SPARQL_PREFIXES 
			+ "DELETE { ?emissionvalue system:numericalValue ?emissionvaluenum .} "
			+ "INSERT { ?emissionvalue system:numericalValue %f .} "
			+ "WHERE {\r\n" + 
			"<%s> technical_system:realizes ?generation . \r\n" +
			"?generation system_performance:hasEmission ?emission .\r\n" + 
			"?emission system:hasValue ?emissionvalue . \r\n" + 
			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
			"}";

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getPathInfo();
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		try {
			String powerplant = jo.getString("plant");
		
			JPSBaseLogger.info(this, "called with path=" + path + ", plant=" + powerplant);
			JPSBaseLogger.info(this, "scenarioURL=" + ThreadContext.get("scenariourl"));
			double oldEmission = getEmission(powerplant);
			JPSBaseLogger.info(this, "oldEmission=" + oldEmission);
			
			String reducedFormula = null;
						
			if ("/read".equals(path)) {
			
				String result = new QueryBroker().readFile(powerplant);
				result = result.substring(0, 400);
				JPSBaseLogger.info(this, "read result (400 signs only):\n");
				JPSBaseLogger.info(this, result);
				
				AgentCaller.printToResponse(result, response);
			
			} else if ("/queryemission".equals(path)) {
			
				String result = new QueryBroker().queryFile(powerplant, SPARQL_PLANT_QUERY_EMISSION);
				result = JenaResultSetFormatter.convertToSimplifiedList(result).toString();
				JPSBaseLogger.info(this, "query result:\n");
				JPSBaseLogger.info(this, result);
				
				AgentCaller.printToResponse(result, response);
			
			} else if ("/getemission".equals(path)) {
				
				JSONWriter writer = new JSONStringer().object().key("hasemission")
						.object().key("hasvalue")
						.object().key("hasnumericalvalue").value(oldEmission)
						.endObject().endObject().endObject();
				String result = writer.toString();
				AgentCaller.printToResponse(result, response);
				
			} else if ("/setemission".equals(path)) {
				
				double emission = jo.getDouble("emission");
				setEmission(powerplant, emission);
				
			} else if ("/add".equals(path)) {
				
				double currentEmission = getEmission(powerplant);
				double increment = jo.getDouble("increment");
				setEmission(powerplant, currentEmission + increment);
				
			} else if ("/multiply".equals(path)) {
				
				double currentEmission = getEmission(powerplant);
				double factor = jo.getDouble("factor");
				setEmission(powerplant, currentEmission * factor);
				
			} else if ("/change".equals(path)) {
				
				// This method calls other agents in a recursive manner (see call below) to operate on the emission value.
				// Example: if formula = "-4.3*2*0.5+4.3" then this method adds -4.3 to the emission value 
				// and calls the next EmissionTestAgent with formula = "*2*0.5+4.3" etc. 
				// The last EmissionTestAgent is called with formula = "+4.3" and just adds +4.3. 
				
				String formula = jo.getString("formula");
				reducedFormula = change(powerplant, formula);
				
			} else {
				throw new JPSRuntimeException("unknown operation");
			}
					
			double newEmission = getEmission(powerplant);
			JPSBaseLogger.info(this, "newEmission=" + newEmission);
			
			if ((reducedFormula != null) && (reducedFormula.length() > 0)) {
				jo.put("formula", reducedFormula);
				AgentCaller.executeGetWithJsonParameter("/JPS_BASE/EmissionTestAgent/change", jo.toString());
			}
			
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	private double getEmission(String powerplant) {
		String result = new QueryBroker().queryFile(powerplant, SPARQL_PLANT_QUERY_EMISSION);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		return list.getJSONObject(0).getDouble("emissionvaluenum");
	}
	
	private void setEmission(String powerplant, double emission) {
		JPSBaseLogger.info(this, "setting emission value to " + emission);
		String query = MiscUtil.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, emission, powerplant);
		new QueryBroker().updateFile(powerplant, query);
	}
	
	public String change(String powerplant, String formula) {
		
		String firstSymbol = formula.substring(0,1);
		StringTokenizer tokenizer = new StringTokenizer(formula.substring(1), "+-*");
		String firstNumber = tokenizer.nextToken();
		double d = Double.parseDouble(firstNumber);
		String reducedFormula = formula.substring(1 + firstNumber.length());
		
		double currentEmission = getEmission(powerplant);
		if ("+".equals(firstSymbol)) {
			setEmission(powerplant, currentEmission + d);
		} else if ("-".equals(firstSymbol)) {
			setEmission(powerplant, currentEmission - d);
		} else if ("*".equals(firstSymbol)) {
			setEmission(powerplant, currentEmission * d);
		} else {
			throw new RuntimeException("unknow sign = " + currentEmission);
		}
		
		return reducedFormula;
	}
}
