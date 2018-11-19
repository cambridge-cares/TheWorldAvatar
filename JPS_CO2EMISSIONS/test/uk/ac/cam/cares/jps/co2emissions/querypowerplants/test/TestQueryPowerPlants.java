package uk.ac.cam.cares.jps.co2emissions.querypowerplants.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestQueryPowerPlants extends TestCase {
	
	private String getContextPathForJPSplantquery() {
		return "/JPS_CO2EMISSIONS/QueryPowerPlants";
	}
	
	static String country="Japan";
	static String sparqlstring = "PREFIX cp:<http://dbpedia.org/resource/> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
			+ "PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
			+ "SELECT ?entity ?vcapa ?generation ?technology ?vemission ?fuel ?vyear"
			+ "{graph ?g "
			+ " {?entity  j2:hasAddress cp:"+country+" ." 
			+ "?entity   j3:realizes ?generation ."
			+ "?generation j8:usesGenerationTechnology ?technology ."
			+ "?generation j8:consumesPrimaryFuel ?fuel ."
			+ "?generation   j5:hasEmission ?emission  ."
			+ "?emission   j2:hasValue ?valueemission ."
			+"FILTER regex(STRBEFORE(STR(?emission),\"#\"), STRBEFORE(STR(?entity),\"#\")) ."
			+ "?valueemission   j2:numericalValue ?vemission ."
			+ "?entity   j4:designCapacity ?capa  ."
			+ "?capa   j2:hasValue ?valuecapa ."
			+ "?valuecapa   j2:numericalValue ?vcapa ." 
			+ "?entity   j8:hasYearOfBuilt ?year  ."
			+ "?year   j2:hasValue ?valueyear ."
			+ "?valueyear   j2:numericalValue ?vyear ." 
			+ "}"
			+ "}";
	ArrayList <String> datalist = new ArrayList <String>();

	

	public void testCallAgent() throws IOException, URISyntaxException, JSONException {
	
		String resultjson = AgentCaller.executeGet(getContextPathForJPSplantquery(), "query", sparqlstring);

		
		JSONArray queryresult = new JSONObject(resultjson).getJSONArray("results");
		
		JSONObject plant1info=queryresult.getJSONObject(0);
		JSONObject plant3info=queryresult.getJSONObject(2);
		

		
		System.out.println(resultjson);
		System.out.println ("result of plant 1= "+plant1info);
		
		assertEquals("http://www.theworldavatar.com/kb/powerplants/Aioi_Thermal_Power_Plant_Japan_01.owl#Aioi_Thermal_Power_Plant_Japan_01", plant1info.getString("entity"));
		assertEquals("1125", plant1info.getString("vcapa"));
		assertEquals("http://www.theworldavatar.com/kb/powerplants/Aioi_Thermal_Power_Plant_Japan_03.owl#Aioi_Thermal_Power_Plant_Japan_03", plant3info.getString("entity"));
		assertEquals("1983", plant3info.getString("vyear"));
		
	}
	
	
}

