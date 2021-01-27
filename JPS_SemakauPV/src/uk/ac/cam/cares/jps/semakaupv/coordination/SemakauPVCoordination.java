package uk.ac.cam.cares.jps.semakaupv.coordination;

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

@WebServlet(urlPatterns = { "/startCoordinationSemakauPV" })
public class SemakauPVCoordination extends JPSHttpServlet {
	private Logger logger = LoggerFactory.getLogger(SemakauPVCoordination.class);
	
	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
			
			startSimulation(jo,response);
		
	}
	
	public void startSimulation(JSONObject jo,HttpServletResponse response) throws IOException {
		
		logger.info("starting the PV calling ");
		
		//retrofit the generator of solar
		logger.info("sent to the IrradiationandWeather= "+jo.toString());
		String result1= AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString()); //sensorirradiri
	
		String iriofirrad = new JSONObject(result1).optString("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		jo.put("irradiationsensor",iriofirrad);
		
		logger.info("started simulation");
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/SemakauPV", jo.toString()); //EN of Semakau
	
	
		JSONObject finres= new JSONObject(result); 
		
		AgentCaller.writeJsonParameter(response, finres);

		
	}

}
