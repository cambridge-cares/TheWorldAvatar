package uk.ac.cam.cares.jps.semakaupv.coordination;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

@WebServlet("/PeriodicCoordination")
public class SemakauPeriodicExecution extends HttpServlet  {
	
	String ENIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/SemakauElectricalNetwork.owl#SemakauElectricalNetwork";
	String irradSensorIRI="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
	
	public void executeSGData(JSONObject jo){
		JSONObject upcorn = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		jo.put("irradiationsensor",irradSensorIRI);

		jo.put("location", "Singapore");
		
		callAgent(jo);
	}

	public void callAgent(JSONObject jo) {
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedSemakauPV.owl#Service");
		AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/startCoordinationSemakauPV",jo.toString());
	}
	
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException {
		
		JSONObject jo = new JSONObject();
		
		JSONObject inputjo = AgentCaller.readJsonParameter(req);
		String scenarioUrl = null;
		String scenarioName = inputjo.optString("scenarioname");
		if ((scenarioName != null) && !scenarioName.isEmpty()) {
			scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
			JPSContext.putScenarioUrl(jo, scenarioUrl);
		}
		
		System.out.println("CoordinationDataCollection is called with scenarioUrl = " + scenarioUrl);
		
		executeSGData(jo);
		
		System.out.println("it is executed");
	}
}
