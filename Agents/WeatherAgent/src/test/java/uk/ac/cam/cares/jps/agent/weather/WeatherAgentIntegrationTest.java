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
import java.time.Instant;

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

/**
 * you must have the docker environment configured to run these tests
 * @author Kok Foong Lee
 *
 */
public class WeatherAgentIntegrationTest {
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:14.4");
	
	RemoteStoreClient storeClient;
	TimeSeriesClient<Instant> tsClient;
	
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
        URIBuilder builder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace");
		
		// create a new namespace (endpoint) on blazegraph with geospatial enabled
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost(builder.build());
		postRequest.setEntity(new StringEntity(postbody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
		
		String sparql_endpoint = response.getLastHeader("Location").getValue();
		
		storeClient = new RemoteStoreClient(sparql_endpoint,sparql_endpoint);	
     	tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
	}
	
	// commented out tests that requires 
	@Test
	public void integrationTest() {
		// create a station in the test container
		JSONObject mockCreateRequest = new JSONObject();
		mockCreateRequest.put("latlon", "1.0#0.10");
		CreateStation create = new CreateStation();
		// the MockWeatherQueryClient does not make an API connection
		MockWeatherQueryClient weatherClient = new MockWeatherQueryClient(storeClient, tsClient);
		create.setWeatherQueryClient(weatherClient);
		// JSONObject response = create.processRequestParameters(mockCreateRequest);
		JSONObject response = new JSONObject();
		
		String createdStation = response.getString("station");
		
		// get weather data for this station
		JSONObject mockGetRequest = new JSONObject();
		mockGetRequest.put("station", createdStation);
		
		// delete this station
		DeleteStation delete = new DeleteStation();
		delete.setWeatherQueryClient(weatherClient);
		// delete.processRequestParameters(mockGetRequest);
	}
	
	/**
	 * revert values in the properties file to the original values after tests
	 * @throws IOException
	 */
	@AfterEach
	public void cleanup() throws IOException {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if (postgres.isRunning()) {
			postgres.stop();
		}
	}
}
