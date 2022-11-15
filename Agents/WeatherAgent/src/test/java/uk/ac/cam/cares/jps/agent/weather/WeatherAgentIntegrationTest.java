package uk.ac.cam.cares.jps.agent.weather;

import static org.mockito.Mockito.*;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.time.Instant;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.utils.URIBuilder;

import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.ClassPathResource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Require support from the "stack" before proper tests can be written
 * @author Kok Foong Lee
 *
 */
@Disabled
public class WeatherAgentIntegrationTest {
	@Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	@Container
	private GenericContainer<?> ontop = new GenericContainer<>(DockerImageName.parse("ontop/ontop-endpoint:4.2.1")).withExposedPorts(8080);

	@Container
	private GenericContainer<?> stack_manager = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/stack-manager:1.2.0")).withExposedPorts(8080);

	// Create Docker container with postgres 13.3 image from Docker Hub
	// Using newer versions will cause connection issues at the time of writing
	DockerImageName postgisImage = DockerImageName.parse("postgis/postgis:13-3.2").asCompatibleSubstituteFor("postgres");
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(postgisImage);
	
	RemoteStoreClient kgClient;
	TimeSeriesClient<Instant> tsClient;
	RemoteStoreClient ontopClient;

	/**
	 * initialise properties file with the urls for the test containers
	 * @throws IOException 
	 * @throws URISyntaxException 
	 * @throws InterruptedException
	 * @throws UnsupportedOperationException
	 * @throws SQLException
	 */
	@BeforeEach
	public void init() throws IOException, URISyntaxException {
		Network network = Network.newNetwork();
		// Start test containers
		blazegraph.withNetwork(network);
		blazegraph.withNetworkAliases("blazegraph");
		blazegraph.setStartupAttempts(2);
		blazegraph.start();

		postgres.setStartupAttempts(2);
		postgres.withNetwork(network);
		postgres.withNetworkAliases("postgis");
		postgres.start();
		// Connection conn = DriverManager.getConnection("jdbc:tc:postgis:14-3.2:///timeseries", "postgres", "postgres");

		ontop.setStartupAttempts(2);
		ontop.withNetwork(network);
		ontop.withNetworkAliases("ontop");
		ontop.addEnv("ONTOP_MAPPING_FILE", "/ontop.obda");
		// ontop is connecting to postgis from within the network, so not localhost
		ontop.addEnv("ONTOP_DB_URL", "jdbc:postgresql://postgis:5432/test?loggerLevel=OFF");
		ontop.addEnv("ONTOP_DB_USER", postgres.getUsername());
		ontop.addEnv("ONTOP_DB_PASSWORD", postgres.getPassword());
		ontop.addEnv("ONTOP_DB_DRIVER", postgres.getDriverClassName());
		ontop.addEnv("ONTOP_LAZY_INIT", "false");
		ontop.addEnv("ONTOP_CORS_ALLOWED_ORIGINS", "*");

		// postgres driver volume		
		String postgresDriver = Paths.get(System.getProperty("user.home"), ".m2/repository/org/postgresql/postgresql/42.4.1/postgresql-42.4.1.jar").toString();
		ontop.withFileSystemBind(postgresDriver, "/opt/ontop/jdbc/postgresql-42.4.1.jar");

		// mapping file volume
		ontop.withFileSystemBind(new ClassPathResource("ontop.obda").getFile().getPath(), "/ontop.obda");

		ontop.start(); 

        URIBuilder kgbuilder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace/kb/sparql");
		String kg_endpoint = kgbuilder.build().toString();

		URIBuilder ontopbuilder = new URIBuilder().setScheme("http").setHost(ontop.getHost()).setPort(ontop.getFirstMappedPort()).setPath("sparql");
		String ontop_endpoint = ontopbuilder.build().toString();
		
		kgClient = new RemoteStoreClient(kg_endpoint,kg_endpoint);	
     	tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
		ontopClient = new RemoteStoreClient(ontop_endpoint);
	}
	
	// commented out tests that requires 
	@Test
	public void integrationTest() throws ServletException, IOException {
		// create a station in the test container
		JSONObject mockCreateRequest = new JSONObject();
		mockCreateRequest.put("latlon", "1.0#0.10");
		CreateStation create = new CreateStation();
		// the MockWeatherQueryClient does not make an API connection
		MockWeatherQueryClient weatherClient = new MockWeatherQueryClient(kgClient, tsClient, ontopClient);
		create.setWeatherQueryClient(weatherClient);
		
		HttpServletRequest request = mock(HttpServletRequest.class);       
        HttpServletResponse response = mock(HttpServletResponse.class);    

		when(request.getParameter("lat")).thenReturn("1.0");
        when(request.getParameter("lon")).thenReturn("0.0");

		create.doPut(request, response);

        when(request.getParameter("username")).thenReturn("me");
        when(request.getParameter("password")).thenReturn("secret");
		
		String createdStation = "";
		
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
