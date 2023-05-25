package uk.ac.cam.cares.jps.agent.weather;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class Config extends ContainerClient{
	private static boolean initialised = false;
	public static PostGISEndpointConfig postGISEndpointConfig = null;
	public static String apikey = System.getenv("API_KEY"); // api key for open weather
	public static String dburl;
	public static String dbuser;
	public static String dbpassword;
    
	private static BlazegraphEndpointConfig blazegraphEndpointConfig;
	public static String kgurl;
	public static String kguser;
	public static String kgpassword;

	public static String ontop_url;

	private static OntopEndpointConfig ontopEndpointConfig;

	public static String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
	public static String DATABASE = System.getenv("DATABASE");
	public static String LAYERNAME = System.getenv("LAYERNAME"); // layer name in geoserver, also tablename in postgis

	private static final Logger LOGGER = LogManager.getLogger(Config.class);

	public void initProperties() {

		if (!initialised) {
			try {
				postGISEndpointConfig = this.readEndpointConfig("postgis",
						PostGISEndpointConfig.class);

				Config.apikey = System.getenv("API_KEY");
				Config.dburl = postGISEndpointConfig.getJdbcURL(DATABASE);
				Config.dbuser = postGISEndpointConfig.getUsername();
				Config.dbpassword = postGISEndpointConfig.getPassword();

				blazegraphEndpointConfig = this.readEndpointConfig("blazegraph",
						BlazegraphEndpointConfig.class);
				Config.kgurl = blazegraphEndpointConfig.getUrl("kb");
				Config.kguser = blazegraphEndpointConfig.getUsername();
				Config.kgpassword = blazegraphEndpointConfig.getPassword();

				ontopEndpointConfig = this.readEndpointConfig("ontop", OntopEndpointConfig.class);
				Config.ontop_url = ontopEndpointConfig.getUrl();

				initialised = true;
			} catch (Exception e) {
				LOGGER.error("This is fine running under test mode");
				LOGGER.error(e.getMessage());
			}
		}
	}
}
