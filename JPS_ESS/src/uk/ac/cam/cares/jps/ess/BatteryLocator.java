package uk.ac.cam.cares.jps.ess;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/LocateBattery" })
public class BatteryLocator extends JPSHttpServlet {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(BatteryLocator.class);
    }
    Logger logger = LoggerFactory.getLogger(BatteryLocator.class);
    
    
    @Override
   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	
    	JSONObject jo = AgentCaller.readJsonParameter(request);
	
		//run the opf
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
    	
		String resultStartLocator = AgentCaller.executeGetWithJsonParameter("JPS_ESS/CreateBattery", jo.toString());
    	JSONObject ans=new JSONObject(resultStartLocator); 
		jo.put("batterylist",ans.get("batterylist"));
		
		return jo;
    	
    	
    	
    	
    }

}
