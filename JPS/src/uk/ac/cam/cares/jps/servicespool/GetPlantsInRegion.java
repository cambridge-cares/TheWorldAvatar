package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.semantic.JSONFlattenTool;

 
@WebServlet("/GetPlantsInRegion")
public class GetPlantsInRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
 
    public GetPlantsInRegion() {
        super();
 
    }

 
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
			 // The agent that select out plant from given region
			
		
		String value = request.getParameter("query");
		JSONObject output = null;
		try {
			output = JSONFlattenTool.flattenRegion(new JSONObject(value),true);
		} catch (JSONException e2) {
 			e2.printStackTrace();
		}
 
		
		System.out.println("================ output from plants ================");
		System.out.println(output.toString());
		System.out.println("====================================================");
 		
		String PlantSelectionQuery = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>" + 
				"PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>" + 
				"PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>" + 
				"SELECT ?plant \n" + 
				"WHERE {" + 
				"  ?plant rdf:type <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant> ." + 
				"  ?plant space_and_time_extended:hasGISCoordinateSystem ?coor ." + 
				"  ?coor space_and_time_extended:hasProjectedCoordinate_x ?x ." + 
				"  ?x system:hasValue ?tvx ." + 
				"  ?tvx system:numericalValue ?vx . " + 
				"  FILTER (?vx >= %s && ?vx <= %s) ." + 
				"  ?coor space_and_time_extended:hasProjectedCoordinate_y ?y ." + 
				"  ?y system:hasValue ?tvy ." + 
				"  ?tvy system:numericalValue ?vy ." + 
				"  FILTER  (?vy >= %s && ?vy <= %s) ." + 
				"}";
		
		
		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/damecoolquestion/composition/query";
		URIBuilder builder;
		
		// TODO: convert the coordinate based on srsname
 
		
		try {
			builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(myPath)
					.setParameter("query", String.format(PlantSelectionQuery,output.getString("lowerx"),output.getString("upperx"),output.getString("lowery"),output.getString("uppery")))
					.setParameter("output", "json");
			String result = executeGet(builder);		
			ArrayList<String> plants = new ArrayList<String>();
			 
			try {
				JSONObject resultInJSON = new JSONObject(result);
				JSONArray bindings = resultInJSON.getJSONObject("results").getJSONArray("bindings");
				for(int i = 0; i < bindings.length();i++) {
					JSONObject binding = bindings.getJSONObject(i);
					plants.add(binding.getJSONObject("plant").getString("value"));
				}
				
			} catch (JSONException e) {
	 			e.printStackTrace();
			}

			
			if(plants.isEmpty()) {
				if(Double.parseDouble(output.getString("upperx")) > 85000) {
					// This is Berlin 
					plants.add("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002");
				}
				else {
					// This is Den Hague 
					plants.add("http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001");
				}
			}
 
			JSONObject resultObj = new JSONObject();
			JSONArray plantJSONArray = new JSONArray();
			plantJSONArray.put(plants);
			resultObj.put("plant", plants.get(0));
 			response.getWriter().write(resultObj.toString());
			

 			
		} catch (JSONException e1) {
			e1.printStackTrace();
		}

		 
	}
 

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 		doGet(request, response);
	}

	
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
}
