package uk.ac.cam.cares.jps.wte.test;

import java.io.IOException;
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
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class TestWTE extends TestCase {
	static String iriofnetwork="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	String usecaseID = UUID.randomUUID().toString();
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
	
	
	
	public void testQueryFC2() {
		OntModel model= WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String baseUrl= QueryBroker.getLocalDataPath();
		new WastetoEnergyAgent().prepareCSVFC(WastetoEnergyAgent.FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model);
		new WastetoEnergyAgent().prepareCSVWT(WastetoEnergyAgent.WTquery,"Location.csv",baseUrl,model);
		new WastetoEnergyAgent().prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
		new WastetoEnergyAgent().prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model,"offsite");
	}
	
	public void testCreateBatchFile() {
		String baseUrl= QueryBroker.getLocalDataPath();
		try {
			new WastetoEnergyAgent().createBat(baseUrl);
			//new WastetoEnergyAgent().runModel(baseUrl);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void testDirectCall() {
		new WastetoEnergyAgent().runTestInSequence(iriofnetwork);
		
	}
	
	public void xxxtestAgentCall() {
		/**
		 * not working anymore because in knowledgebaseclient, 
		 * ResourcePathConverter.convertToLocalPath(requestUrl) is replaced with ResourcePathConverter.convert(requestUrl);
		 * for the update function
		 */
		JSONObject jo = new JSONObject();
		jo.put("wastenetwork", iriofnetwork);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WastetoEnergyAgent", jo.toString());
		
	}
	
	public void testCreateScenarioAndCallWTEAgent() throws JSONException {
		
		String scenarioName = "testwaste2-"+usecaseID;
		String json = new JSONStringer().object()
				.key("wastenetwork").value(iriofnetwork)
				.endObject().toString();
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_WTE/WastetoEnergyAgent/startsimulation", json);
		
		System.out.println(result);
		
	}
	
	public void testQueryScenarioWTEAgent() throws JSONException {
		 String wasteSystemQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "SELECT ?entity ?comp " 
				+ "WHERE {"
				+ "?entity  a j2:CompositeSystem ."
				+ "?entity   j2:hasSubsystem ?comp ." 
				
				+ "}";
		String path = "http://localhost:8080/jps/scenario/testwaste2"; //the scenario name must be existed in the first place!!!
		System.out.println("path= "+path);
		String result = new ScenarioClient().query(path, iriofnetwork, wasteSystemQuery);
		
		System.out.println(result);
		
	}
	
	public void testQueryBaseWTEAgent() throws JSONException {
		 String wasteSystemQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "SELECT ?entity ?comp " 
				+ "WHERE {"
				+ "?entity  a j2:CompositeSystem ."
				+ "?entity   j2:hasSubsystem ?comp ." 
				
				+ "}";
		String path = "http://localhost:8080/jps/scenario/base"; //the scenario name must be existed in the first place!!!
		System.out.println("path= "+path);
		String result = new ScenarioClient().query(path, iriofnetwork, wasteSystemQuery);
		
		System.out.println(result);
		
	}
	
	public void testquery() throws IOException { 
		String result = new QueryBroker().queryFile(iriofnetwork,WastetoEnergyAgent.wasteSystemOutputQuery);
		String[] keyswt = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
		System.out.println("answer number= "+resultList.size());
	}
	public void testScenarioCoordination() throws IOException { 
		String scenarioName = "testwaste2-"+usecaseID;
		String json = new JSONStringer().object()
				.key("wastenetwork").value(iriofnetwork)
				.endObject().toString();
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_WTE/startsimulationCoordinationWTE", json);
		System.out.println(result);
	}

}
