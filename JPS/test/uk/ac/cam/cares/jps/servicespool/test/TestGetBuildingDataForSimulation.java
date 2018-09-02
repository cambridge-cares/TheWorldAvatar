package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringWriter;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;

class TestGetBuildingDataForSimulation {

	@Test
	void test() {


	
		Model regionModel = ModelFactory.createDefaultModel();
		Property hasX = regionModel.createProperty("http://test.com/Property/hasX");
		Property hasY = regionModel.createProperty("http://test.com/Property/hasY");
		Property hasUpperPoint = regionModel.createProperty("http://test.com/Property/upperPoint");
		Property hasLowerPoint = regionModel.createProperty("http://test.com/Property/lowerPoint");
		Resource Region = regionModel.createResource("http://test.com/aRegionInstance");
		Region.addProperty(RDF.type, "http://test.com/ontology/Region");
		Resource UpperPoint = regionModel.createResource("http://test.com/upperPoint");
		UpperPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		UpperPoint.addLiteral(hasX, 80000);
		UpperPoint.addLiteral(hasY, 455190);
		Resource LowerPoint = regionModel.createResource("http://test.com/lowerPoint");
		LowerPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		LowerPoint.addLiteral(hasX, 79480);
		LowerPoint.addLiteral(hasY, 454670);
		Region.addProperty(hasLowerPoint, LowerPoint);
		Region.addProperty(hasUpperPoint, UpperPoint);
		Model plantIRI = ModelFactory.createDefaultModel();
		Resource myPlant = plantIRI.createResource("http://www.theworldavatar.com/Plant-001.owl#Plant-001");
		Resource plant = plantIRI.createResource("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant");
		myPlant.addProperty(RDF.type, plant);
		Model cityIRI = ModelFactory.createDefaultModel();
		Resource myCity = cityIRI.createResource(BuildingQueryPerformer.THE_HAGUE_IRI);
		Resource city = cityIRI.createResource("http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City");
		myCity.addProperty(RDF.type,city);
		
		
		
		Model dataBundle = ModelFactory.createDefaultModel();
		dataBundle.add(plantIRI);
		dataBundle.add(cityIRI); 
		dataBundle.add(regionModel);
		 
		String myHost = "localhost";
		int myPort = 8080;
		String ADMSAgentPath = "/JPS/GetBuildingDataForSimulation";
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, dataBundle, RDFFormat.RDFJSON);
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(ADMSAgentPath)
				.setParameter("value", out.toString());
		String result = executeGet(builder);
		System.out.println("---------- Result Now ---------- xx" + result);

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
