package uk.ac.cam.cares.jps.des;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/DESCoordination" })

public class DESCoordination extends JPSAgent{

	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
 			
 	        String scenarioUrl = BucketHelper.getScenarioUrl();
 	        String usecaseUrl = BucketHelper.getUsecaseUrl();
 	        logger.info("DES scenarioUrl = " + scenarioUrl + ", usecaseUrl = " + usecaseUrl);
 	        requestParams.put("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
 	        AgentCaller.executeGetWithJsonParameter("JPS_DES/GetForecastData", requestParams.toString());
 	        String t =  AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgentNew", requestParams.toString());
 	 	      
 			System.gc();
    	return requestParams;
    }
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
 			
 	        String scenarioUrl = BucketHelper.getScenarioUrl();
 	        String usecaseUrl = BucketHelper.getUsecaseUrl();
 	        logger.info("DES scenarioUrl = " + scenarioUrl + ", usecaseUrl = " + usecaseUrl);
 	        validateInput(requestParams);
 	        requestParams.put("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
 	        AgentCaller.executeGetWithJsonParameter("JPS_DES/GetForecastData", requestParams.toString());
 	        String t =  AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgentNew", requestParams.toString());
 	 	      
 			System.gc();
    	return requestParams;
    }
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        return true;
    }

}
