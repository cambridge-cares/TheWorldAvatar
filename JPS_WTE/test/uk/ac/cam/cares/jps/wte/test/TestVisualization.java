package uk.ac.cam.cares.jps.wte.test;

import java.io.IOException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;
import uk.ac.cam.cares.jps.wte.visualization.WTEVisualization;

public class TestVisualization  extends TestCase {
	public String WasteTopNode = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	public void testFCQueryDirect(){
		WTEVisualization a = new WTEVisualization();
		OntModel model = WastetoEnergyAgent.readModelGreedy(WasteTopNode);
		try {
			String g = a.createMarkers(model);
			JSONObject jo = new JSONObject(g);
			System.out.println(g);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public void testFCQueryAgent(){
		JSONObject jo = new JSONObject().put("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WTEVisualization/createMarkers", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public void testreadInputsDirect(){
		WTEVisualization a = new WTEVisualization();
		OntModel model = WastetoEnergyAgent.readModelGreedy(WasteTopNode);
		String g = a.readInputs(model);
		JSONObject jo = new JSONObject(g);
		System.out.println(g);
	}
	public void testreadInputsAgent(){
		JSONObject jo = new JSONObject().put("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WTEVisualization/readInputs", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
