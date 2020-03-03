package uk.ac.cam.cares.jps.ess.coordination;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;



@WebServlet(urlPatterns = { "/startsimulationCoordinationESS" })
public class CoordinationESSAgent extends JPSHttpServlet{
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(CoordinationESSAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(CoordinationESSAgent.class);
    @Override
   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String path = request.getServletPath();
		
		logger.info("jps request URL="+jo);
		if ("/startsimulationCoordinationESS".equals(path)) {
			
			try {
				return startSimulation(jo);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				logger.error(e.getMessage());
			}
			
		}
		return null;
	}
	
	public JSONObject startSimulation(JSONObject jo) throws IOException {
		
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
						
		jo.put("batterylist",new JSONObject(resultStart).getJSONArray("batterylist"));
		
		String finresult=AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/EnergyStrorageRetrofit", jo.toString());
	
		logger.info("started creating battery");
		JSONObject finres= new JSONObject(finresult); 
		
		return finres;
		
//JSONObject finres= new JSONObject(resultStartLocator); 
//		
//		AgentCaller.writeJsonParameter(response, finres);

		
	}
	

	
	
	
	

}
