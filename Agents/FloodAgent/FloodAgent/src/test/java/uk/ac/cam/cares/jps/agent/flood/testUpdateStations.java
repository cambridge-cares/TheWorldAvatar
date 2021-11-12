package uk.ac.cam.cares.jps.agent.flood;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.http.ParseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class testUpdateStations {
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
	
	// this string is copied from the blazegraph workbench window when you create a new namespace
    // the name of the namespace is flood, with geospatial enabled
    static String postbody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n"
    		+ "com.bigdata.rdf.sail.isolatableIndices=false\r\n"
    		+ "com.bigdata.namespace.flood.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n"
    		+ "com.bigdata.rdf.sail.truthMaintenance=false\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n"
    		+ "com.bigdata.rdf.sail.namespace=flood\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n"
    		+ "com.bigdata.namespace.flood.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=true\r\n"
    		+ "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";
	
	RemoteStoreClient storeClient;
	TimeSeriesClient<Instant> tsClient;
	FloodSparql sparqlClient;
	APIConnector api;
	
	@BeforeEach
	public void startContainers() throws IOException, URISyntaxException {
		try {
			// Start Blazegraph container
			blazegraph.start();
			// Start postgreSQL container
			postgres.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("Docker container startup failed. Please try running tests again");
		}
		
        URIBuilder builder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace");
		
		// create a new namespace (endpoint) on blazegraph with geospatial enabled
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost(builder.build());
		postRequest.setEntity(new StringEntity(postbody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
		
		String sparql_endpoint = response.getLastHeader("Location").getValue();
		
		// clients that connect to the postgres and blazegraph test containers
		storeClient = new RemoteStoreClient(sparql_endpoint,sparql_endpoint);	
     	tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
     	sparqlClient = new FloodSparql(storeClient);
	}
	
	@Test
	public void testProcessAPIResponse() throws ParseException, IOException, URISyntaxException {
		// create a mock api connector that returns data in the test/resources folder 
     	// instead of downloading it from the web
     	InputStream is = getClass().getClassLoader().getResourceAsStream("sample_response.txt");
		StringEntity entity = new StringEntity(IOUtils.toString(is,StandardCharsets.UTF_8),ContentType.APPLICATION_JSON);
		api = mock(APIConnector.class);
		when(api.getData()).thenReturn(entity);
		
		List<Map<?,?>> processed_data = UpdateStations.processAPIResponse(api);
		Map<String, List<Instant>> map1 = (Map<String, List<Instant>>) processed_data.get(0);
		Map<String, List<Double>> map2 = (Map<String, List<Double>>) processed_data.get(1);
	}
	
	@Test
	public void testUploadDataToRDB() throws UnsupportedCharsetException, IOException, URISyntaxException {
		// create a mock api connector that returns data in the test/resources folder 
     	// instead of downloading it from the web
		// this returns the rdf file containing stations info
     	InputStream is = getClass().getClassLoader().getResourceAsStream("stations.rdf");
		StringEntity entity = new StringEntity(IOUtils.toString(is,StandardCharsets.UTF_8),ContentType.create("application/rdf+xml"));
		api = mock(APIConnector.class);
		when(api.getData()).thenReturn(entity);
		
		InitialiseStations.setAPIConnector(api);
		InitialiseStations.setSparqlClient(sparqlClient);
		InitialiseStations.setStoreClient(storeClient);
		InitialiseStations.setTsClient(tsClient);
		
		InitialiseStations.main(new String[0]);
		
		// mock response for update API
		is = getClass().getClassLoader().getResourceAsStream("sample_response.txt");
		entity = new StringEntity(IOUtils.toString(is,StandardCharsets.UTF_8),ContentType.APPLICATION_JSON);
		api = mock(APIConnector.class);
		when(api.getData()).thenReturn(entity);
		
		List<Map<?,?>> processed_data = UpdateStations.processAPIResponse(api);
		UpdateStations.uploadDataToRDB(LocalDate.now(), tsClient, sparqlClient, processed_data);
	}
	
	@AfterEach
	public void cleanup() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if (postgres.isRunning()) {
			postgres.stop();
		}
	}
}
