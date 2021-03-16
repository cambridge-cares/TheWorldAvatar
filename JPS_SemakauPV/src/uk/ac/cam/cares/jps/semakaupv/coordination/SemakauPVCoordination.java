package uk.ac.cam.cares.jps.semakaupv.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/PeriodicCoordination")
public class SemakauPVCoordination extends JPSAgent {
	/**
	 * @author Laura Ong
	 */
	private static final long serialVersionUID = 1L;
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/SemakauElectricalNetwork.owl#SemakauElectricalNetwork";
	private String irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
    private String iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
    private String iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
   
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override 
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
			requestParams.put("electricalnetwork",ENIRI);
			requestParams.put("irradiationsensor",iriofirrS);
			requestParams.put("temperaturesensor",irioftempS);
			requestParams.put("windspeedsensor",iriofwindS);
			startSimulation(requestParams);
			return new JSONObject();
		
	}
	/** Main method coordinating both DES project and JPS Semakau Project
	 * 
	 * @param jo {"electricalnetwork":, "irradiation sensor"}
	 * @param response
	 * @throws IOException
	 */
	public void startSimulation(JSONObject jo) {
		
		AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString()); //sensorirradiri
	
		AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/SemakauPV", jo.toString()); //EN of Semakau
	
	

		
	}

}
