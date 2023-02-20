package uk.ac.cam.cares.jps.agent.weather;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.net.URISyntaxException;
import java.time.Instant;

import org.apache.http.client.utils.URIBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.json.JSONArray;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
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
 * Require support from the "stack" before proper tests can be written
 * @author Kok Foong Lee
 *
 */
@Disabled
@Testcontainers
public class WeatherQueryClientTest {
	// Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
	RemoteStoreClient kgClient;
    WeatherQueryClient weatherClient;

	@BeforeEach
	public void initialise() throws URISyntaxException {
		URIBuilder kgbuilder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace/kb/sparql");
		
		String kg_endpoint = kgbuilder.build().toString();
 		
 		// Set up a kb client that points to the location of the triple store
     	kgClient = new RemoteStoreClient(kg_endpoint,kg_endpoint);	
     	TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
     	weatherClient = new MockWeatherQueryClient(kgClient, tsClient, null);
	}
	
	/**
	 * tests for core functions
	 * @throws InterruptedException 
	 */
	@Test
	public void testCoreFunctions() throws InterruptedException {
        // creating a station
		// String station = weatherClient.createStation("0#0");
		String station = "";
        Assertions.assertTrue(stationExists(station));
        
        // getting weather data
        weatherClient.getLatestWeatherData(station);
        TimeSeries<Instant> historicalData1 = weatherClient.getHistoricalWeatherData(station,1);
        
        // updating data
        Instant oldtimestamp = weatherClient.getLastUpdateTime(station);
        Thread.sleep(1500); // wait for 1.5 second to ensure next timestamp is at least 1 second greater
        weatherClient.updateStation(station, null);
        Instant newtimestamp = weatherClient.getLastUpdateTime(station);
        Assertions.assertTrue(newtimestamp.isAfter(oldtimestamp));
        TimeSeries<Instant> historicalData2 = weatherClient.getHistoricalWeatherData(station,1);
        Assertions.assertTrue(historicalData2.getTimes().size() > historicalData1.getTimes().size());
        
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
		query.select(var).where(var.isA(iri(WeatherQueryClient.ontoems+"ReportingStation")));
		JSONArray queryResult = kgClient.executeQuery(query.getQueryString());
		
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
