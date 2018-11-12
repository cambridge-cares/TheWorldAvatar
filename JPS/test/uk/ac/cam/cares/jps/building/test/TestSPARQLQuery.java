package uk.ac.cam.cares.jps.building.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestSPARQLQuery {

	public String query = "PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
			"PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
			"PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>\r\n" + 
			"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\r\n" + 
			"SELECT  distinct ?bdn\r\n" + 
			"WHERE {\r\n" + 
			"{\r\n" + 
			"?bdn a citygml:BuildingType.\r\n" + 
			"?bdn citygml:boundedBy ?g.\r\n" + 
			"?g a citygml:GroundSurfaceType.\r\n" + 
			"?g citygml:lod2MultiSurface ?ms.\r\n" + 
			"?ms citygml:surfaceMember ?pol.\r\n" + 
			"?pol citygml:exterior ?lring.\r\n" + 
			"?lring sys:contains ?po.\r\n" + 
			"?po space_and_time_extended:hasGISCoordinateSystem ?co.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_x ?xe.\r\n" + 
			"?xe sys:hasValue ?xv.\r\n" + 
			"?xv sys:numericalValue ?x.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_y ?ye.\r\n" + 
			"?ye sys:hasValue ?yv.\r\n" + 
			"?yv sys:numericalValue ?y.\r\n" + 
			"} UNION {\r\n" + 
			"?bdn a citygml:BuildingType.\r\n" + 
			"?bdn citygml:consistsOfBuildingPart ?part.\r\n" + 
			"?part a citygml:BuildingPartType.\r\n" + 
			"?part citygml:boundedBy ?g.\r\n" + 
			"?g a citygml:GroundSurfaceType.\r\n" + 
			"?g citygml:lod2MultiSurface ?ms.\r\n" + 
			"?ms citygml:surfaceMember ?pol.\r\n" + 
			"?pol citygml:exterior ?lring.\r\n" + 
			"?lring sys:contains ?po.\r\n" + 
			"?po space_and_time_extended:hasGISCoordinateSystem ?co.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_x ?xe.\r\n" + 
			"?xe sys:hasValue ?xv.\r\n" + 
			"?xv sys:numericalValue ?x.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_y ?ye.\r\n" + 
			"?ye sys:hasValue ?yv.\r\n" + 
			"?yv sys:numericalValue ?y.\r\n" + 
			"}\r\n" + 
			"Filter(xsd:double(?x) > \"391937.258878\"^^xsd:double && xsd:double(?y) > \"5819820.136031\"^^xsd:double && xsd:double(?x) < \"392050.641151\"^^xsd:double && xsd:double(?y) < \"5820066.169883\"^^xsd:double) \r\n" + 
			"}\r\n" + 
			"LIMIT 25";
	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void test() {
		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/damecoolquestion/berlinbuildings/query";
		URIBuilder builder;
		
		System.out.println(query);
		
		  builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setParameter("query", query)
				.setParameter("output", "json");
		  
		String result = executeGet(builder);
		System.out.println(result);
		
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
