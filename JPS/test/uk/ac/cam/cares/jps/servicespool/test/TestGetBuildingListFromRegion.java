package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.semantic.QueryWarehouse;

class TestGetBuildingListFromRegion {

	@Test
	void test() throws UnsupportedEncodingException {
		String value = "{\"http://test.com/upperPoint\":{\"http://test.com/Property/hasX\":[{\"datatype\":\"http://www.w3.org/2001/XMLSchema@string\",\"type\":\"literal\",\"value\":\"13.430676467370517\"}],\"http://test.com/Property/hasY\":[{\"datatype\":\"http://www.w3.org/2001/XMLSchema@string\",\"type\":\"literal\",\"value\":\"52.51811955356901\"}],\"http://www.w3.org/1999/02/22-rdf-syntax-ns@type\":[{\"type\":\"literal\",\"value\":\"http://test.com/ontology/Point\"}]},\"http://test.com/lowerPoint\":{\"http://test.com/Property/hasX\":[{\"datatype\":\"http://www.w3.org/2001/XMLSchema@string\",\"type\":\"literal\",\"value\":\"13.413274295281894\"}],\"http://test.com/Property/hasY\":[{\"datatype\":\"http://www.w3.org/2001/XMLSchema@string\",\"type\":\"literal\",\"value\":\"52.5035060960622\"}],\"http://www.w3.org/1999/02/22-rdf-syntax-ns@type\":[{\"type\":\"literal\",\"value\":\"http://test.com/ontology/Point\"}]},\"http://test.com/aRegionInstance\":{\"http://test.com/Property/referenceSystem\":[{\"type\":\"literal\",\"value\":\"EPSG:4326\"}],\"http://test.com/Property/upperPoint\":[{\"type\":\"uri\",\"value\":\"http://test.com/upperPoint\"}],\"http://test.com/Property/lowerPoint\":[{\"type\":\"uri\",\"value\":\"http://test.com/lowerPoint\"}],\"http://www.w3.org/1999/02/22-rdf-syntax-ns@type\":[{\"type\":\"literal\",\"value\":\"http://test.com/ontology/Region\"}]}}\r\n";
		String myHost = "localhost";
		int myPort = 8080;
  
		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);
		JSONObject region = QueryWarehouse.getRegionCoordinates(model);
		
		String myPathBuildingList = "/JPS/buildings/fromregion";
		URIBuilder builderBuildingList;
		try {
			builderBuildingList = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(myPathBuildingList)
					.setParameter("cityiri", "http://dbpedia.org/page/Berlin")
					.setParameter("buildinglimit", "25")
					.setParameter("lowerx", String.valueOf(region.getDouble("xmin")) )
					.setParameter("lowery", String.valueOf(region.getDouble("ymin")) )
					.setParameter("upperx", String.valueOf(region.getDouble("xmax")) )
					.setParameter("uppery", String.valueOf(region.getDouble("ymax")) );
			String buildingList = executeGet(builderBuildingList);
			System.out.println(buildingList);
		} catch (JSONException e) {
			e.printStackTrace();
		}

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
