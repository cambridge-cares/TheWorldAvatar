package uk.ac.cam.cares.jps.servicespool.test;

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.CRSTransformer;

public class TestWholeADMS {

	@Test
	public void test() throws UnsupportedEncodingException {


		// 1. define inputs to be ... 
	
		Model regionModel = ModelFactory.createDefaultModel();
		Property hasX = regionModel.createProperty("http://test.com/Property/hasX");
		Property hasY = regionModel.createProperty("http://test.com/Property/hasY");
		Property hasUpperPoint = regionModel.createProperty("http://test.com/Property/upperPoint");
		Property hasLowerPoint = regionModel.createProperty("http://test.com/Property/lowerPoint");
		Property hasReferenceSystem = regionModel.createProperty("http://test.com/Property/referenceSystem");
		
		
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
		Region.addLiteral(hasReferenceSystem, CRSTransformer.EPSG_25833);
		
		
		String GetPlantPath = "/JPS/GetPlantsInRegion";
		Model Plant = callAgentSemantically(regionModel, GetPlantPath);
		//System.out.println(convertTORDFJSON(Plant));

		Model cityIRI = ModelFactory.createDefaultModel();
		Resource myCity = cityIRI.createResource("http://dbpedia.org/resource/The_Hague");
		Resource city = cityIRI.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time_v1.owl#City");
		myCity.addProperty(RDF.type,city);
	
		//================================================================================================
		// Make requests ... 
		// 1. Request weather 
		
		
		String WeatherPath = "/JPS_COMPOSITION/CityToWeather";
		Model WeatherResult = callAgentSemantically(cityIRI,WeatherPath);
	    //System.out.println(convertTORDFJSON(WeatherResult));
		// =================================================================================================
		
		String GetBuildingForSimulationPath = "/JPS/GetBuildingDataForSimulation";
		Model dataBundle = ModelFactory.createDefaultModel();
		dataBundle.add(Plant);
		dataBundle.add(cityIRI); 
		dataBundle.add(regionModel);
		Model BuildingForSimulation = callAgentSemantically(dataBundle, GetBuildingForSimulationPath);
		//System.out.println(convertTORDFJSON(BuildingForSimulation));
		//System.out.println("\n===================================================\n");
		// =================================================================================================

		// =================================================================================================

		
		
		
		Model inputDataBundleForADMS = ModelFactory.createDefaultModel();
		inputDataBundleForADMS.add(Plant);
		inputDataBundleForADMS.add(WeatherResult);
		inputDataBundleForADMS.add(BuildingForSimulation);
		inputDataBundleForADMS.add(regionModel);

		
		String ADMSAgentPath = "/JPS/ADMSAgent";
		String rst = callAgentSimple(inputDataBundleForADMS,ADMSAgentPath);
 
		System.out.println(rst);
		//================================================================================================
		
	}

	public Model callAgentSemantically(Model input, String path) throws UnsupportedEncodingException {
		String myHost = "localhost";
		int myPort = 8080;
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, input, RDFFormat.RDFJSON);
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(path)
				.setParameter("value",out.toString());
		
		Model resultModel = ModelFactory.createDefaultModel();
		String resultInString = executeGet(builder);
		
		RDFDataMgr.read(resultModel, new ByteArrayInputStream(resultInString.getBytes("UTF-8")), Lang.RDFJSON);
		return resultModel;
	}
	
	
	public String callAgentSimple(Model input, String path) throws UnsupportedEncodingException {
		String myHost = "localhost";
		int myPort = 8080;
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, input, RDFFormat.RDFJSON);
				
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(path)
				.setParameter("value",out.toString().replace("#", "$"));

		String resultInString = executeGet(builder);
		return resultInString;
	}
	
	
	public String convertTORDFJSON(Model model) {
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, model, RDFFormat.RDFJSON);
		return out.toString();
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
	
	public String executePost(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpPost request = new HttpPost(uri);
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
