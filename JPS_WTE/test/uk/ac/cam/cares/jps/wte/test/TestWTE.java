package uk.ac.cam.cares.jps.wte.test;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.wte.WTESingleAgent;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class TestWTE extends TestCase {
	static String iriofnetwork="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	String usecaseID = UUID.randomUUID().toString();
	/**Query no of FC
	 */
	public void testQueryFC() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String query= a.FCQuery;
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of result="+resultList.size()); //should be 109*15
		
	}
	/** Query for costs after simulation finishes running
	 * 
	 */
	public void testQuerytopnode() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		OntModel model=JenaHelper.createModel(iriofnetwork);
		String query= a.wasteSystemQuery;
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of result="+resultList.size()); 
        assertEquals(1, resultList.size());
        System.out.println(Arrays.toString(keys));
        System.out.println(Arrays.toString(resultList.get(0)));
	
	}
	/** Query transport data
	 * 
	 */
	public void testQuerytransport() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String query= a.transportQuery;
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of result="+resultList.size()); 
        assertEquals(1, resultList.size());
        System.out.println(Arrays.toString(keys));
        System.out.println(Arrays.toString(resultList.get(0)));
	
	}
	/** Run simulation via Scenario Client to get individual scenarios
	 * 
	 * @throws JSONException
	 */
	public void testCreateScenarioAndCallWTEAgent() throws JSONException {
		
		String scenarioName = "testwaste2-"+usecaseID;
		String json = new JSONStringer().object()
				.key("wastenetwork").value(iriofnetwork)
				.endObject().toString();
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_WTE/WastetoEnergyAgent/startsimulation", json);
		
		System.out.println(result);
		
	}
	/** Query in Directly (WTE Agent)
	 * 
	 * @throws Exception
	 */
	public void testInSuccession() throws Exception {
		WastetoEnergyAgent ag = new WastetoEnergyAgent();
		String baseUrl = QueryBroker.getLocalDataPath();
		String sourceName = BucketHelper.getScenarioName(baseUrl);
		OntModel model= WastetoEnergyAgent.readModelGreedy("http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		List<String[]> inputsondata = ag.prepareCSVFC(ag.FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
		ag.prepareCSVWT(ag.WTquery,"Location.csv", baseUrl,model); 
		ag.prepareCSVTransport(ag.transportQuery,"transport.csv", baseUrl,model); 
		ag.prepareCSVCompTECHBased(ag.compquery,baseUrl,model);
		ag.prepareCSVTECHBased(ag.WTFTechQuery,baseUrl,model,"offsite");
		List<String[]> propertydataonsite=ag.prepareCSVTECHBased(ag.WTFTechOnsiteQuery,baseUrl,model,"onsite");
		ag.copyTemplate(baseUrl, "SphereDist.m");
		ag.copyTemplate(baseUrl, "Main.m");
		ag.copyTemplate(baseUrl, "D2R.m");
		
		try {
			ag.createBat(baseUrl);
			ag.runModel(baseUrl);
//            notifyWatcher(requestParams, baseUrl+"/number of units (onsite).csv",
//                    request.getRequestURL().toString().replace(SIM_START_PATH, SIM_PROCESS_PATH));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	public void testReadResult() {
		WTESingleAgent ag = new WTESingleAgent();
		try {
			List<String[]> ae = ag.readResult("C:\\Users\\ongajong\\4.30 WTE-JPS Cluster Continuous and Problem of non-real objective", "x_cluster_allocation.csv");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
