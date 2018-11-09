package uk.ac.cam.cares.jps.sparqlmock.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.co2emissions.sparqlmock.QueryPowerPlants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class Testsparqlmock extends TestCase {
	
	private String getContextPathForJPSplantquery() {
		return "/JPS_CO2EMISSIONS/QueryPowerPlants";
	}
	
	static String country="Zimbabwe";
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


		//JSONObject dataSet = new JSONObject();
		
	 //iri					
					

			
		
		String resultjson = AgentCaller.executeGet(getContextPathForJPSplantquery(), "query", sparqlstring);

		
		JSONArray queryresult = new JSONObject(resultjson).getJSONArray("results");
		
		JSONObject plant1=queryresult.getJSONObject(0);
		
		System.out.println(resultjson);
		System.out.println ("result of plant 1= "+plant1);
		
		System.out.println("result of plant0 picked= "+plant1.getString("plant"));
		System.out.println("result of capacity of plant0 picked= "+plant1.getString("capacity"));
		




		//assertEquals(0, emission, 1000.);
		//assertEquals(5000.0, capacity, 1000.);


	}
	
	
	public void testqueryPowerplantProperty() {
		
		QueryPowerPlants a= new QueryPowerPlants();
		ArrayList <String> Def= a.queryPowerplantProperty(sparqlstring);
		//System.out.println("query= "+sparqlstring);
		

		System.out.println("size= "+Def.size());
	}
	
	
	
	
//	public static void main(String[] args) {
//		
//		Testsparqlmock abc = new Testsparqlmock();
//		ArrayList <String> Def= abc.queryPowerplantProperty(sparqlstring);
//		
//		System.out.println("size= "+Def.size());
//	
//	}
}

