package uk.ac.cam.cares.jps.powsys.retrofit;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet("/EnergyStrorageRetrofit")
public class BatteryRetrofit extends GeneralRetrofitAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;



	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String electricalNetwork = jo.getString("electricalnetwork");

		JSONArray ja = jo.getJSONArray("batterylist");
		List<String> BatteryList = MiscUtil.toList(ja);
		retrofitEnergyStorage(electricalNetwork, BatteryList);
		AgentCaller.printToResponse(jo, response);
		
	}
	
	
	
	public void retrofitEnergyStorage(String electricalNetwork, List<String> BatteryList) {
		
//		OntModel model = ENAgent.readModelGreedy(electricalNetwork);
//		List<BusInfo> buses = queryBuses(model);
//		BusInfo slackBus = findFirstSlackBus(buses);
		
		List<GeneratorInfo> batteries = new ArrayList<GeneratorInfo>();
		//QueryBroker broker = new QueryBroker();
		for (String currentGen : BatteryList) {
			String batIri = currentGen;
			GeneratorInfo info = new GeneratorInfo();
			info.generatorIri = batIri;	
			batteries.add(info);
		}

		
		addGeneratorsToElectricalNetwork(electricalNetwork, batteries);
		
		logger.info("finished retrofitting energy storage");
	}
	
}
