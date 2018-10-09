package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.Assert.*;

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
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.rdf.model.Property;

import org.apache.jena.vocabulary.RDF;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.CRSTransformer;

public class TestGetPlantsInRegion {

	@Test
	public void test() {


		String myHost = "localhost";
		int myPort = 8080;
		String ADMSAgentPath = "/JPS/GetPlantsInRegion";
		//===========================================================================
		Model rdfModel = ModelFactory.createDefaultModel();
		Property hasX = rdfModel.createProperty("http://test.com/Property/hasX");
		Property hasY = rdfModel.createProperty("http://test.com/Property/hasY");
		Property hasUpperPoint = rdfModel.createProperty("http://test.com/Property/upperPoint");
		Property hasLowerPoint = rdfModel.createProperty("http://test.com/Property/lowerPoint");
		Property hasReferenceSystem = rdfModel.createProperty("http://test.com/Property/referenceSystem");
		
		
		Resource Region = rdfModel.createResource("http://test.com/aRegionInstance");
		Region.addProperty(RDF.type, "http://test.com/ontology/Region");
		Resource UpperPoint = rdfModel.createResource("http://test.com/upperPoint");
		UpperPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		UpperPoint.addLiteral(hasX, 80000);
		UpperPoint.addLiteral(hasY, 455190);
		Resource LowerPoint = rdfModel.createResource("http://test.com/lowerPoint");
		LowerPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		LowerPoint.addLiteral(hasX, 79480);
		LowerPoint.addLiteral(hasY, 454670);
		Region.addProperty(hasLowerPoint, LowerPoint);
		Region.addProperty(hasUpperPoint, UpperPoint);
		Region.addLiteral(hasReferenceSystem, CRSTransformer.EPSG_25833);
		
		
		
		
		
		
		
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, rdfModel, RDFFormat.RDFJSON);
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(ADMSAgentPath)
				.setParameter("value", out.toString());
		String result = executeGet(builder);
		System.out.println(result);
		
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
