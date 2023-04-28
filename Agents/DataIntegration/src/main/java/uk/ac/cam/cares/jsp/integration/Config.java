package uk.ac.cam.cares.jsp.integration;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Config  extends ContainerClient {
    private static boolean initialised = false;
    public static PostGISEndpointConfig postGISEndpointConfig = null;
    public static String dburl;
    public static String dbuser;
    public static String dbpassword;

//    private static BlazegraphEndpointConfig blazegraphEndpointConfig;
//    public static String kgurl;
    public static String kguser;
    public static String kgpassword;
//    public static String ontop_url;

    private static OntopEndpointConfig ontopEndpointConfig;
    public static String DATABASE = System.getenv("DATABASE");
    private static final Logger LOGGER = LogManager.getLogger(Config.class);

    public void initProperties() {

        if (!initialised) {
            try {
                postGISEndpointConfig = this.readEndpointConfig("postgis",
                        PostGISEndpointConfig.class);

                Config.dburl = postGISEndpointConfig.getJdbcURL(DATABASE);
                Config.dbuser = postGISEndpointConfig.getUsername();
                Config.dbpassword = postGISEndpointConfig.getPassword();

//                blazegraphEndpointConfig = this.readEndpointConfig("blazegraph",
//                        BlazegraphEndpointConfig.class);
//                Config.kgurl = blazegraphEndpointConfig.getUrl("kb");
//                Config.kguser = blazegraphEndpointConfig.getUsername();
//                Config.kgpassword = blazegraphEndpointConfig.getPassword();
//
//                ontopEndpointConfig = this.readEndpointConfig("ontop", OntopEndpointConfig.class);
//                Config.ontop_url = ontopEndpointConfig.getUrl();

                initialised = true;
            } catch (Exception e) {
                LOGGER.error("This is fine running under test mode");
                LOGGER.error(e.getMessage());
            }
        }
    }
}
