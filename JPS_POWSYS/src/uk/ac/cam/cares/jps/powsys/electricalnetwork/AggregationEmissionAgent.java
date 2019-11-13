package uk.ac.cam.cares.jps.powsys.electricalnetwork;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = { "/aggregateemission" })
public class AggregationEmissionAgent extends JPSHttpServlet {
	
    public static final String UPDATE_PATH = "/AggregationEmissionAgent/update";
    public static final String SUM_PATH = "/AggregationEmissionAgent/sum";
	
	String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		    + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
		    + "PREFIX technical_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
		    + "SELECT ?entity ?V_Actual_CO2_Emission ?V_Design_CO2_Emission ?plant "
		    
		    + "WHERE {?entity  a  j1:PowerGenerator  ."
		    + "?entity   j2:isSubsystemOf ?plant ." // plant
		    + "?entity   technical_system:realizes ?generation ."
		    + "?generation j9:hasEmission ?emission ." 
		    
		    + "?emission a j9:Actual_CO2_Emission ."
		    + "?emission   j2:hasValue ?valueemission ."
		    + "?valueemission   j2:numericalValue ?V_Actual_CO2_Emission ." //

		    
		    + "?generation j9:hasEmission ?v_emission ." 
		    + "?v_emission a j9:CO2_emission ."
		    + "?v_emission   j2:hasValue ?valueemission_d ."
		    + "?valueemission_d   j2:numericalValue ?V_Design_CO2_Emission ." //
		    

		    + "}";
	
	String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "SELECT ?component "
			+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";
	
	String plantInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> "
			+ "SELECT ?chimney "
			+ "WHERE {?entity  a  j1:PowerPlant  ." 
			+ "?entity   j2:hasSubsystem ?chimney ."
			+ "?chimney  a j3:Pipe ."
			+ "}";
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	
	
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException { 
		
		JSONObject joforEN = AgentCaller.readJsonParameter(request);

		String iriofnetwork = joforEN.getString("electricalnetwork");
		
		
		JSONObject result=sumEmissionResult(iriofnetwork);
		AgentCaller.printToResponse(result, response);
	
	}
	
	public static List<String[]> provideGenlist(String iriofnetwork) {
		String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity " 
				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
				+ "}";
		

		OntModel model = ENAgent.readModelGreedy(iriofnetwork);
		ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		return resultListfromquery;
	}
	
	public JSONObject sumEmissionResult(String ENIRI) {
		List<String[]> genList = provideGenlist(ENIRI);	
		QueryBroker broker  = new QueryBroker();
		List<String> plantunique=new ArrayList<String>();
		List<String> emplantunique=new ArrayList<String>();

		for(int d=0;d<genList.size();d++) {
			String result = broker.queryFile(genList.get(d)[0], genInfo);	
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			if(!plantunique.contains(resultList.get(0)[3])) {
				plantunique.add(resultList.get(0)[3]);
			}
			emplantunique.add(resultList.get(0)[1]+"from"+resultList.get(0)[3]);
			
		}

        int sizeofplant=plantunique.size();
        System.out.println("uniqueplantsize= "+sizeofplant);
        Double[]plantactco2=new Double[sizeofplant];
        int index=0;
		for (int t = 0; t < plantunique.size(); t++) {
			plantactco2[index] = 0.0;
			for (int x = 0; x < emplantunique.size(); x++) {
				String name = emplantunique.get(x).split("from")[1];
				String value = emplantunique.get(x).split("from")[0];

				if (name.contains(plantunique.get(t))) {
					plantactco2[index] = plantactco2[index] + Double.valueOf(value);
					System.out.println(name);
					System.out.println(value);
				}
			}

			index++;
		}

		JSONObject ans = new JSONObject();
		JSONArray plant = new JSONArray();
		JSONArray chimney = new JSONArray();
		JSONArray emission = new JSONArray();
		for (int f = 0; f < plantunique.size(); f++) {
			String result = broker.queryFile(plantunique.get(f), plantInfo);	
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			chimney.put(resultList.get(0)[0]);
			plant.put(plantunique.get(f));
			emission.put(plantactco2[f]);
		}
		ans.put("plant", plant);
		ans.put("emission", emission);
		ans.put("chimney", chimney);
		
//		System.out.println(plantunique.get(2));
//		System.out.println("total actco2 for plant 1= " + plantactco2[2]);
       
	return ans;
	}
	
	public void updateEmission() {
		
		
	}
	
	

}
