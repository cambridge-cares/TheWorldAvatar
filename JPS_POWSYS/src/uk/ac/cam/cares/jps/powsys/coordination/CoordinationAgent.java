package uk.ac.cam.cares.jps.powsys.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/startsimulation", "/processresult", "/processresultwithpf", "/processresultwithopf" })
public class CoordinationAgent extends JPSHttpServlet implements Prefixes, Paths {

	private static final long serialVersionUID = 6859324316966357379L;
	private Logger logger = LoggerFactory.getLogger(CoordinationAgent.class);

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		//String electricalNetwork = jo.getString("electricalnetwork");
		
		String path = request.getServletPath();

		String pathForENAgent = null;
		if ("/startsimulation".equals(path)) {
			
			startSimulation(jo);
			
		} else if ("/processresult".equals(path)) {
			
		} else if ("/processresultwithpf".equals(path)) {
			pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, jo.toString());
		} else if ("/processresultwithopf".equals(path)) {
			pathForENAgent = "JPS_POWSYS/ENAgent/startsimulationOPF";
			AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
			AgentCaller.executeGetWithJsonParameter(pathForENAgent, jo.toString());
		}
	}
	
	public void startSimulation(JSONObject jo) {
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/optimizeforcarbontax", jo.toString());
		
		logger.info("carbon tax optimization finished with result = " + result);
	
		result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
	
		logger.info("started npp optimization asynchronously");
	}
	
	public void processResult(JSONObject jo) {
		
		
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofit", jo.toString());
		
	}
}
