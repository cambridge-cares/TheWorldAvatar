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
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Property;

import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.semantic.QueryWarehouse;


@WebServlet("/GetBuildingListFromRegion")
public class GetBuildingListFromRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
   
    public GetBuildingListFromRegion() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
		String value = request.getParameter("value").replace("$", "#").replace("@", "#");
		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);
		JSONObject region = QueryWarehouse.getRegionCoordinates(model);
 		String cityIRI = QueryWarehouse.getCityIRI(model).replace("resource", "page");
 
 		System.out.println("==================================GetBuildingFromRegion ==========");
 		System.out.println("value : " + value);
 		System.out.println("cityIRI: " +  cityIRI);
 		System.out.println("region:  " + region.toString());
 		System.out.println("==================================GetBuildingFromRegion ==========");

 		
 		
		String myHost = request.getServerName();
		int myPort = request.getServerPort();
		
		
		String myPathBuildingList = "/JPS/buildings/fromregion";
		URIBuilder builderBuildingList;
		try {
			builderBuildingList = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(myPathBuildingList)
					.setParameter("cityiri", cityIRI.trim())
					.setParameter("buildinglimit", "25")
					.setParameter("lowerx", String.valueOf(region.getDouble("xmin")) )
					.setParameter("lowery", String.valueOf(region.getDouble("ymin")) )
					.setParameter("upperx", String.valueOf(region.getDouble("xmax")) )
					.setParameter("uppery", String.valueOf(region.getDouble("ymax")) );
			String buildingList = executeGet(builderBuildingList);
			response.getWriter().write(convertBuildingListToSemantic(buildingList));
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		}
	
	public String convertBuildingListToSemantic(String buildingList) {

		Model rdfModel = ModelFactory.createDefaultModel();
		Resource BuildingListModel = rdfModel.createResource("http://test.com/Instance/myBuildingList");
		Resource BuildingListType = rdfModel.createResource("http://test.com/ontology/BuildingList");
		BuildingListModel.addProperty(RDF.type, BuildingListType);
		Property hasBuilding = rdfModel.createProperty("http://test.com/property/hasBuilding");
		
		
		try {
			JSONArray listInJSON = new JSONArray(buildingList);
			for(int i = 0; i < listInJSON.length(); i++) {
				String buildingIRI = listInJSON.getString(i);
				Resource aBuilding = rdfModel.createResource(buildingIRI.trim());
				BuildingListModel.addProperty(hasBuilding, aBuilding);
			}
			
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, rdfModel, Lang.RDFJSON);
		return out.toString();
	}


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			//request.setHeader(HttpHeaders.ACCEPT, "application/sparql-results+json");
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
