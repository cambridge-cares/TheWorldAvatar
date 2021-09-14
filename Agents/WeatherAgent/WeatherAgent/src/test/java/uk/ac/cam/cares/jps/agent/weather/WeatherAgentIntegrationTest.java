package uk.ac.cam.cares.jps.agent.weather;

import static org.mockito.Mockito.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;

import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class WeatherAgentIntegrationTest {
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
	
	// to record original values in the properties file
	private Properties props;
	private File props_file;
	
	RemoteStoreClient storeClient;
	TimeSeriesClient<Long> tsClient;
	
	// this string is copied from the blazegraph workbench window when you create a new namespace
    // the name of the namespace is weather, with geospatial enabled
    static String postbody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n"
    		+ "com.bigdata.rdf.sail.isolatableIndices=false\r\n"
    		+ "com.bigdata.namespace.weather.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n"
    		+ "com.bigdata.rdf.sail.truthMaintenance=false\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n"
    		+ "com.bigdata.rdf.sail.namespace=weather\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n"
    		+ "com.bigdata.namespace.weather.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=true\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";
	/**
	 * initialise properties file with the urls for the test containers
	 * @throws IOException 
	 * @throws URISyntaxException 
	 */
	@BeforeEach
	public void init() throws IOException, URISyntaxException {
		try {
			// Start Blazegraph container
			blazegraph.start();
			// Start postgreSQL container
			postgres.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("AgentIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		InputStream inputstream = new ClassPathResource("credentials.properties").getInputStream();
		props_file = new ClassPathResource("credentials.properties").getFile();
		
		props = new Properties();
		props.load(inputstream);
		
        URIBuilder builder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace");
		
		// create a new namespace (endpoint) on blazegraph with geospatial enabled
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost(builder.build());
		postRequest.setEntity(new StringEntity(postbody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
		
		String sparql_endpoint = response.getLastHeader("Location").getValue();
		
		FileOutputStream outputStream = new FileOutputStream(props_file);
		Properties temp_props = new Properties(props);
		temp_props.setProperty("kg.url", sparql_endpoint);
		temp_props.setProperty("db.url",postgres.getJdbcUrl());
		temp_props.setProperty("db.user", postgres.getUsername());
		temp_props.setProperty("db.password", postgres.getPassword());
		temp_props.store(outputStream, "Temporary file created by WeatherAgentIntegrationTest, this should be overwritten with the original values after the test completes");
		outputStream.close();
		
		storeClient = new RemoteStoreClient(sparql_endpoint,sparql_endpoint);	
     	tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
	}
	
	@Test
	public void integrationTest() {
		// create a station in the test container
		JSONObject mockCreateRequest = new JSONObject();
		mockCreateRequest.put("latlon", "1.0#0.10");
		CreateStation create = new CreateStation();
		// the MockWeatherQueryClient does not make an API connection
		create.setWeatherQueryClient(new MockWeatherQueryClient(storeClient, tsClient));
		JSONObject response = create.processRequestParameters(mockCreateRequest);
		
		String createdStation = response.getString("station");
		
		// try to query this station within a circle
		JSONObject mockCircleRequest = new JSONObject();
		mockCircleRequest.put("radius", 1);
		mockCircleRequest.put("centre", "1.0#0.10");
		GetStationsInCircle getStationsInCircle = new GetStationsInCircle();
		List<Object> stationsInCircle = getStationsInCircle.processRequestParameters(mockCircleRequest)
				.getJSONArray("station").toList();
		Assertions.assertTrue(stationsInCircle.contains(createdStation));

		// try to query this within a rectangle
		JSONObject mockRectangleRequest = new JSONObject();
		mockRectangleRequest.put("southwest", "0.9#0");
		mockRectangleRequest.put("northeast", "1.1#0.2");
		GetStationsInRectangle getStationsInRectangle = new GetStationsInRectangle();
		List<Object> stationsInRectangle = getStationsInRectangle.processRequestParameters(mockRectangleRequest)
				.getJSONArray("station").toList();
		Assertions.assertTrue(stationsInRectangle.contains(createdStation));
		
		// get weather data for this station
		JSONObject mockGetRequest = new JSONObject();
		mockGetRequest.put("station", createdStation);
		GetWeatherData getWeatherData = new GetWeatherData();
		getWeatherData.setWeatherQueryClient(new MockWeatherQueryClient(storeClient, tsClient));
		
		// get latest weather data (probably most common query)
		HttpServletRequest httprequest = mock(HttpServletRequest.class); 
		when(httprequest.getServletPath()).thenReturn(GetWeatherData.urlPatternLatest);
		JSONObject timeSeriesResponse = getWeatherData.processRequestParameters(mockGetRequest, httprequest);
		
		// try to deserialise the response into a TimeSeries object
		Type timeSeriesType = new TypeToken<TimeSeries<Long>>() {}.getType();
        new Gson().fromJson(timeSeriesResponse.toString(), timeSeriesType);
        
		// get historical weather data, requires an additional input:
		// number of hours to query backwards
		// this will give a time series from 10 hours before the current time
		// up to the current time. For this test there is only 1 value
		mockGetRequest.put("hour", 10); 
		when(httprequest.getServletPath()).thenReturn(GetWeatherData.urlPatternHistory);
		timeSeriesResponse = getWeatherData.processRequestParameters(mockGetRequest, httprequest);
		new Gson().fromJson(timeSeriesResponse.toString(), timeSeriesType);
	}
	
	/**
	 * revert values in the properties file to the original values after tests
	 * @throws IOException
	 */
	@AfterEach
	public void cleanup() throws IOException {
		FileOutputStream outputStream = new FileOutputStream(props_file);
		props.store(outputStream, "Reverted to original values after being used by WeatherAgentIntegrationTest");
		outputStream.close();
		
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if (postgres.isRunning()) {
			postgres.stop();
		}
	}
}
