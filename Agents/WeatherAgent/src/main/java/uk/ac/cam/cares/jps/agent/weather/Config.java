package uk.ac.cam.cares.jps.agent.weather;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class Config extends ContainerClient{
	private static boolean initialised = false;
	private static PostGISEndpointConfig postGISEndpointConfig = null;
	public static String apikey = System.getenv("API_KEY"); // api key for open weather
	public static String dburl;
	public static String dbuser;
	public static String dbpassword;
    
	private static BlazegraphEndpointConfig blazegraphEndpointConfig;
	public static String kgurl;
	public static String kguser;
	public static String kgpassword;

	private static final Logger LOGGER = LogManager.getLogger(Config.class);

	public void initProperties() {

		if (!initialised) {
			try {
				postGISEndpointConfig = this.readEndpointConfig("postgis",
						PostGISEndpointConfig.class);

				Config.apikey = System.getenv("API_KEY");
				Config.dburl = postGISEndpointConfig.getJdbcURL("");
				Config.dbuser = postGISEndpointConfig.getUsername();
				Config.dbpassword = postGISEndpointConfig.getPassword();

				blazegraphEndpointConfig = this.readEndpointConfig("blazegraph",
						BlazegraphEndpointConfig.class);
				Config.kgurl = blazegraphEndpointConfig.getUrl("kg");
				Config.kguser = blazegraphEndpointConfig.getUsername();
				Config.kgpassword = blazegraphEndpointConfig.getPassword();

				initialised = true;
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
			}
		}
	}
}
