package uk.ac.cam.cares.jps.ess.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


@WebServlet(urlPatterns = { "/startsimulationCoordinationESS" })
public class CoordinationESSAgent extends JPSHttpServlet{
	
	private Logger logger = LoggerFactory.getLogger(CoordinationESSAgent.class);
	
	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String path = request.getServletPath();

		if ("/startsimulationCoordinationESS".equals(path)) {
			
			startSimulation(jo,response);
			
		} 
	}
	
	public void startSimulation(JSONObject jo,HttpServletResponse response) throws IOException {
		
		logger.info("starting the ESS ");
		
		//retrofit the generator of solar
		logger.info("sent to the retrofit= "+jo.toString());
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofitGenerator", jo.toString());
	
		//run the scenario for EN after it is retrofitted
		logger.info("starting the OPF");
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
		String dirOfOPF = new JSONObject(resultStart).getString("folder");
		
		
		
		//JSONObject jo2= new JSONObject(result);
		
		jo.put("folder",dirOfOPF);
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
	
		logger.info("started creating battery");
		JSONObject finres= new JSONObject(result); 
		
		AgentCaller.writeJsonParameter(response, finres);

		
	}

}
