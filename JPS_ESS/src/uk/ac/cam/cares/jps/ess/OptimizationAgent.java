package uk.ac.cam.cares.jps.ess;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/OptimizationAgent"})

public class OptimizationAgent extends JPSHttpServlet {
	
	//suggesting the optimization model used based on storage technology chosen
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		
		String path="JPS_POWSYS/ENAgent/startsimulationOPF"; //later can be queried from the agent descriptions
		
		String batIRI=joforess.getString("storage");
		if(batIRI.contains("VRB")) {
			path="JPS_POWSYS/ENAgent/startsimulationOPF";
		}
		
		
		
		JSONObject resultofOptimization=new JSONObject();
		resultofOptimization.put("optimization",path);
		AgentCaller.printToResponse(resultofOptimization, response);
		
		
	}

}
