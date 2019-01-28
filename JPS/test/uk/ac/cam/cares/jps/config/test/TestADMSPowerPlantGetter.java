package uk.ac.cam.cares.jps.config.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;

import com.google.gson.Gson;

import junit.framework.TestCase;

/** 
 * JSON Object Format 
 * */

class PowerPlant {
	String type;
	List<Feature> features;
	
	String getType() {
		return type;
	}
	
	List<Feature> getFeatures() {
		return features;
	}
}

class Feature {
	String type;
	Properties properties;
	Geometry geometry;
	
	String getType() {
		return type;
	}
	
	Properties getProperties() {
		return properties;
	}
	
	Geometry getGeometry() {
		return geometry;
	}
}

class Properties {
	double height;
	double minHeight;
	String color;
	String roofColor;
	
	double getHeight() {
		return height;
	}
	
	double getMinHeight() {
		return minHeight;
	}
	
	String getColor() {
		return color;
	}
	
	String getRoofColor() {
		return roofColor;
	}
}

class Geometry {
	String type;
	List<List<List<Double>>> coordinates;
	
	String getType() {
		return type;
	}
	
	List<List<List<Double>>> getCoordinates() {
		return coordinates;
	}
}

/**
 * 
 * @author WE
 * Validating ADMSPowerPlantGetter endpoint when location is set to The Hague
 *
 */

public class TestADMSPowerPlantGetter extends TestCase {
	
	public void testADMSPowerPlantGetterPythonScript() throws ClientProtocolException, IOException, URISyntaxException {
		
		// Send HTTP get request to endpoint
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8080")
				.setPath("/JPS/ADMSPowerPlantGetter")
				.setParameter("location", "The Hague");
		
		URI uri = builder.build();
		HttpGet getRequest = new HttpGet(uri);
		HttpClient httpClient = HttpClientBuilder.create().build();
		HttpResponse httpResponse = httpClient.execute(getRequest);
		
		// Parse response into string
		BufferedReader rd = new BufferedReader(new InputStreamReader(
			    httpResponse.getEntity().getContent()));
		
		StringBuilder total = new StringBuilder();
		String line = null;

		while ((line = rd.readLine()) != null) {
			total.append(line);
		}
		rd.close();
		String body = total.toString();	
		
		System.out.println(body);
		// Parse json into Java object
		Gson g = new Gson();
		PowerPlant powerPlant = g.fromJson(body, PowerPlant.class);
		List<Double> actualFirstPoint = powerPlant.getFeatures().get(0).getGeometry().getCoordinates().get(0).get(0);
		//List<Double> expectedFirstPoint = Arrays.asList(4.290850963814782, 52.07602183798938);
		System.out.println(actualFirstPoint.toString());
		//assertEquals(expectedFirstPoint, actualFirstPoint);
		
//		System.out.println(EntityUtils.toString(httpResponse.getEntity()));
	}

}
