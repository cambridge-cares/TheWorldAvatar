package uk.ac.cam.cares.jps.wte.test;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class TestWTE extends TestCase {
	static String iriofnetwork="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	
	public void xxxtestQueryFC() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String query= a.FCQuery;
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of result="+resultList.size()); //should be 109*15
        assertEquals(1635, resultList.size());
        System.out.println(Arrays.toString(keys));
        System.out.println(Arrays.toString(resultList.get(0)));
        System.out.println(Arrays.toString(resultList.get(1)));
        System.out.println(Arrays.toString(resultList.get(2)));
        System.out.println(Arrays.toString(resultList.get(109)));
        System.out.println(Arrays.toString(resultList.get(110)));
        System.out.println(Arrays.toString(resultList.get(111)));
		
	}
	
	public void xxxtestQuerytopnode() {
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
	
	public void xxxtestQuerytransport() {
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
	
	public void xxxtestQueryWTF() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		String iriofnetwork="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String query= a.createWTFQuery("j1:OffsiteWasteTreatmentFacility","j1:OffSiteCoDigestion");
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of result="+resultList.size()); 
        assertEquals(3, resultList.size());
        System.out.println(Arrays.toString(keys));
        System.out.println(Arrays.toString(resultList.get(0)));
        System.out.println(Arrays.toString(resultList.get(1)));
        System.out.println(Arrays.toString(resultList.get(2)));
	
	}
	
	public void testQueryFC2() {
		OntModel model= WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String baseUrl= QueryBroker.getLocalDataPath();
		new WastetoEnergyAgent().prepareCSVFC(WastetoEnergyAgent.FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model);
		new WastetoEnergyAgent().prepareCSVWT(WastetoEnergyAgent.WTquery,"Location.csv",baseUrl,model);
		new WastetoEnergyAgent().prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
		new WastetoEnergyAgent().prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model);
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
	
	public void testAgentCall() {
		JSONObject jo = new JSONObject();
		jo.put("wastenetwork", iriofnetwork);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WastetoEnergyAgent", jo.toString());
		
	}
	
	public void testquery() throws IOException {
//		OntModel model= WastetoEnergyAgent.readModelGreedy(iriofnetwork);
//		ResultSet resultSet = JenaHelper.query(model, WastetoEnergyAgent.wasteSystemOutputQuery);
//		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String result = new QueryBroker().queryFile(iriofnetwork,WastetoEnergyAgent.wasteSystemOutputQuery);
		String[] keyswt = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
		System.out.println("answer number= "+resultList.size());
	}

}
