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
		requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
    }
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
 			
 	        String scenarioUrl = BucketHelper.getScenarioUrl();
 	        String usecaseUrl = BucketHelper.getUsecaseUrl();
 	        logger.info("DES scenarioUrl = " + scenarioUrl + ", usecaseUrl = " + usecaseUrl);
 	        requestParams.put("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
 	        requestParams.put("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
 	        requestParams.put("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
 	        requestParams.put("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
 	        requestParams.put("cityIRI", "http://dbpedia.org/page/Singapore");
 	        requestParams.put("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
 	        validateInput(requestParams);
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
