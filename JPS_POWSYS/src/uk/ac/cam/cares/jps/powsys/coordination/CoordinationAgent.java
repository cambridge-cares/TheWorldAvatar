package uk.ac.cam.cares.jps.powsys.coordination;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/startsimulation", "/processresult", "/processresultwithpf", "/processresultwithopf" })
public class CoordinationAgent extends JPSHttpServlet implements Prefixes, Paths {

	private static final long serialVersionUID = 6859324316966357379L;
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(CoordinationAgent.class);
    }

	 @Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	
		JSONObject jo =requestParams;
		String path = request.getServletPath();

		if ("/startsimulation".equals(path)) {
			
			startSimulation(jo);
			
		} else if ("/processresult".equals(path)) {
			
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
			
		} else if ("/processresultwithpf".equals(path)) { //unused
			
			String pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, jo.toString());
			
		} else if ("/processresultwithopf".equals(path)) { //unused
			
			String pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationOPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, jo.toString());
			
		}
		return jo;
		
	}
	
	public void startSimulation(JSONObject jo) {
		
		logger.info("starting optimization for carbon tax");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/optimizeforcarbontax", jo.toString());
		
		logger.info("carbon tax optimization finished with result = " + result);
	
		JSONObject jo2= new JSONObject(result);
		
		jo.put("substitutionalgenerators",jo2.getJSONArray("substitutionalgenerators"));
		
		result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
	
		logger.info("started npp optimization asynchronously");
		//logger.info("started npp optimization synchronously");
//		JSONObject jo3= new JSONObject(result);
//		jo.put("plants",jo3.getJSONArray("plants"));
//		
//		result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
		
	}
}
