package uk.ac.cam.cares.jps.servicespool;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
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
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.semantic.QueryWarehouse;

 
@WebServlet("/GetPlantsInRegion")
public class GetPlantsInRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
 
    public GetPlantsInRegion() {
        super();
 
    }

 
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
			 // The agent that select out plant from given region
			
		
		String value = request.getParameter("value").replace("$", "#").replace("@", "#");		
		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);

		StringWriter out = new StringWriter();		
		JSONObject output = QueryWarehouse.getRegionCoordinates(model);
		System.out.println(output.toString());
		// In the form of JSON
		 
		String PlantSelectionQuery = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>" + 
				"PREFIX space_and_time_extended:<http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>" + 
				"PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>" + 
				"SELECT ?plant \n" + 
				"WHERE {" + 
				"  ?plant rdf:type <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant> ." + 
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
 
		
		try {
			builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(myPath)
					.setParameter("query", String.format(PlantSelectionQuery,output.getString("xmin"),output.getString("xmax"),output.getString("ymin"),output.getString("ymax")))
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
				if(Double.parseDouble(output.getString("xmax")) > 85000) {
					// This is Berlin 
					plants.add("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002");
				}
				else {
					// This is Den Hague 
					plants.add("http://www.theworldavatar.com/Plant-001.owl#Plant-001");
				}
			}
	
			Model plantsModel = convertPlantToSemantic(plants);
			RDFDataMgr.write(out, plantsModel, RDFFormat.RDFJSON);
 			response.getWriter().write(out.toString());
			

 			
		} catch (JSONException e1) {
			e1.printStackTrace();
		}

		 
	}
	public Model convertPlantToSemantic(ArrayList<String> plants) {
		
		Model plantIRIs = ModelFactory.createDefaultModel();
		for(String plantIRI : plants) {
			Resource myPlant = plantIRIs.createResource(plantIRI);
			Resource plant = plantIRIs.createResource("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant");
			myPlant.addProperty(RDF.type, plant);
		}
		return plantIRIs; 
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
