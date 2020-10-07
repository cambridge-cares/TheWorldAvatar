package uk.ac.cam.cares.bio.test;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.bio.SelectionAgent;

public class Test_SelectionAgent extends TestCase {
	String compIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/R-301.owl#R-301";
	String componentlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "SELECT ?plant "
			+ "WHERE {"
			+ "?place j2:hasSubsystem ?plant ."
			+ "?place  a  j1:Eco-industrialPark ."
			+ "}";
   
	String plantInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "SELECT ?plant ?path "
			+ "WHERE {"
			+ "?plant a  j2:CompositeSystem ."
			+ "?plant j2:isModeledBy ?model ."
			+ "?model j2:hasURLPath ?path ."
			+ "?plant   j2:hasSubsystem <"+compIRI+"> ." 
			+ "}";
	
	String topnode="http://www.theworldavatar.com/kb/TheWorld.owl#TheWorld";
	
	
	
	public static OntModel readModelGreedy(String iriofEIP) {
		String plantlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofEIP, plantlistInfo);
	}
	
	public void testcall() {
		JSONObject jo= new JSONObject();
		jo.put("componentIRI",compIRI);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoModelSelection", jo.toString());
		System.out.println(resultStart);
	}
	public void testdirectcall() {
		JSONObject response = new SelectionAgent().extractedJSONOutput(topnode, componentlistInfo, compIRI);
		System.out.println(response);
	}
	
	public void xxxtestCallSelectionAgentFromCreatedWorld() throws JSONException {
		
		String scenarioName = "testingrand";
		
		JSONObject jo = new JSONObject();
		jo.put("componentIRI",compIRI);
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_BIODIESELPLANT3/DoModelSelection", jo.toString());
		
		System.out.println(result);
		
	}
	
   public void testQuery() {
	   
    	String componentlistInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?plant "
				+ "WHERE {"
				+ "?place j2:hasSubsystem ?plant ."
				+ "?place  a  j1:Eco-industrialPark ."
				+ "}";
	   String topnode="http://www.theworldavatar.com/kb/TheWorld.owl#TheWorld";
    	String plantInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?plant ?path "
				+ "WHERE {"
				+ "?plant a  j2:CompositeSystem ."
				+ "?plant j2:isModeledBy ?model ."
				+ "?model j2:hasURLPath ?path ."
				+ "?plant   j2:hasSubsystem <"+compIRI+"> ." 
				+ "}";
	   
	   OntModel model=readModelGreedy(topnode);
	   ResultSet resultSet = JenaHelper.query(model, componentlistInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysplant = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysplant);
        System.out.println("what is resultList size= "+resultList.size());
        for(int d=0;d<resultList.size();d++) {
        	//OntModel model2=readModelGreedy(resultList.get(d)[0]);
			String result2 = new QueryBroker().queryFile(resultList.get(d)[0], plantInfo);
			String[] keysplant2 = JenaResultSetFormatter.getKeys(result2);
			List<String[]> resultList2 = JenaResultSetFormatter.convertToListofStringArrays(result2, keysplant2);
	        if(resultList2.size()>0) {
	        System.out.println("what is the plant= "+resultList2.get(0)[0]);
	        System.out.println("what is the path= "+resultList2.get(0)[1]);
	        }
        }
   }
}
