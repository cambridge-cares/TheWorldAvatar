package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.json.JSONArray;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * it is necessary to have docker installed to run these tests
 * @author Kok Foong Lee
 *
 */
@Testcontainers
public class WeatherQueryClientTest {
	// Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    WeatherQueryClient weatherClient;
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
        
	@BeforeEach
	public void initialiseNamespace() throws ClientProtocolException, IOException, URISyntaxException {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace");
		
		// create a new namespace (endpoint) on blazegraph with geospatial enabled
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost(builder.build());
		postRequest.setEntity(new StringEntity(postbody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
		
		String sparql_endpoint = response.getLastHeader("Location").getValue();
 		
 		// Set up a kb client that points to the location of the triple store
     	storeClient = new RemoteStoreClient(sparql_endpoint,sparql_endpoint);	
     	tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
     	weatherClient = new MockWeatherQueryClient(storeClient, tsClient);
	}
	
	/**
	 * tests for core functions
	 * @throws InterruptedException 
	 */
	@Test
	public void testCoreFunctions() throws InterruptedException {
        // creating a station
		String station = weatherClient.createStation("0#0");
        Assertions.assertTrue(stationExists(station));
        
        // getting weather data
        weatherClient.getLatestWeatherData(station);
        TimeSeries<Long> historicalData1 = weatherClient.getHistoricalWeatherData(station,1);
        
        // updating data
        long oldtimestamp = weatherClient.getLastUpdateTime(station);
        Thread.sleep(1500); // wait for 1.5 second to ensure next timestamp is at least 1 second greater
        weatherClient.updateStation(station);
        long newtimestamp = weatherClient.getLastUpdateTime(station);
        Assertions.assertTrue(newtimestamp > oldtimestamp);
        TimeSeries<Long> historicalData2 = weatherClient.getHistoricalWeatherData(station,1);
        Assertions.assertTrue(historicalData2.getTimes().size() > historicalData1.getTimes().size());
        
        // geospatial functions
        Assertions.assertTrue(weatherClient.getStationsInCircle("0#0", 1).get(0).contains(station));
        Assertions.assertTrue(weatherClient.getStationsInRectangle("-0.1#-0.1", "0.1#0.1").contains(station));
        
        weatherClient.deleteStation(station);
        Assertions.assertFalse(stationExists(station));
	}
	
	/**
	 * performs a simple check to see whether the triple <station> a station:WeatherStation exists
	 * @param station
	 * @return
	 */
	private boolean stationExists(String station) {
		boolean exist = false;
		SelectQuery query = Queries.SELECT();
		Variable var = query.var();
		query.select(var).where(var.isA(iri(WeatherQueryClient.ontostation+"WeatherStation")));
		JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
		
		if (queryResult.length() == 1)  {
			String queriedIRI = queryResult.getJSONObject(0).getString(var.getQueryString().substring(1));
			if (queriedIRI.equals(station)) {
				exist = true;
			}
		}
		return exist;
	}
	
	@AfterEach
	public void stopContainers() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if (postgres.isRunning()) {
			postgres.stop();
		}
	}
}
