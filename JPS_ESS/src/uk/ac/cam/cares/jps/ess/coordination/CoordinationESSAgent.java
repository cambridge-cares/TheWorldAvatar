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
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/RenewableGenRetrofit", jo.toString());
		
		//run the opf
		String result = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		JSONObject res1=new JSONObject(result);
		jo.put("storage",res1.getString("storage"));
	
		
		String result2 = AgentCaller.executeGetWithJsonParameter("JPS_ESS/OptimizationAgent", jo.toString());
		JSONObject res2=new JSONObject(result2);
		
		String optimizationresult=res2.getString("optimization");
		//jo.put("optimization",optimizationresult);
		
		logger.info("starting the method selected"); //in this case OPF
		
		String resultStart = AgentCaller.executeGetWithJsonParameter(optimizationresult, jo.toString());
		
		logger.info("optimatization end result= "+resultStart);
		//String eniri = new JSONObject(resultStart).getString("electricalnetwork");
		
		String resultStartLocator = AgentCaller.executeGetWithJsonParameter("JPS_ESS/LocateEnergyStorage", jo.toString());
				
		jo.put("batterylist",new JSONObject(resultStartLocator).getJSONArray("batterylist"));
		
		String finresult=AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/EnergyStrorageRetrofit", jo.toString());
	
		logger.info("started creating battery");
		JSONObject finres= new JSONObject(finresult); 
		
		AgentCaller.writeJsonParameter(response, finres);

		
	}
	

	
	
	
	

}
