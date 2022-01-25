package uk.ac.cam.cares.jps.agent.flood;

import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Config{
	private static boolean initialised;
	public static String dburl;
	public static String dbuser;
	public static String dbpassword;
    
	public static String kgurl;
	public static String kguser;
	public static String kgpassword;
	
	public static String outputdir;
	
	private static String postgres_host;
	private static String postgres_port;
	private static String postgres_dbname;
	
	private static String kg_host;
	private static String kg_port;
	private static String kg_path;
	private static String kg_protocol;
	
	private static final Logger LOGGER = LogManager.getLogger(Config.class);
	
	// environment variables read by this script, some are mandatory and some are not
	// KG_HOST, KG_PORT, KG_PATH, KG_PROTOCOL, KG_USER, KG_PASSWORD
	// POSTGRES_HOST, POSTGRES_PORT, POSTGRES_DBNAME, POSTGRES_USER, POSTGRES_PASSWORD
	public static void initProperties() {
		if (!initialised) {    		
			Config.outputdir = System.getenv("OUTPUT_DIR");
			if (Config.outputdir != null) {
				LOGGER.info("Detected environment variable OUTPUT_DIR, script will write files to " + Config.outputdir);
			} else {
				throw new RuntimeException("The environment variable OUTPUT_DIR is not set");
			}
			
			// construct postgres jdbc url
			postgres_dbname = System.getenv("POSTGRES_DBNAME");
			if (postgres_dbname != null) {
				postgres_host = System.getenv("POSTGRES_HOST");
				postgres_port = System.getenv("POSTGRES_PORT");
				
				Config.dbuser = System.getenv("POSTGRES_USER");
				Config.dbpassword = System.getenv("POSTGRES_PASSWORD");
				if (Config.dbuser == null || Config.dbpassword == null) {
					throw new RuntimeException("The environment variable POSTGRES_USER/POSTGRES_PASSWORD is not set");
				}
				
				if (postgres_host != null) {
					if (postgres_port != null) {
						Config.dburl = "jdbc:postgresql://" + postgres_host + ":" + postgres_port + "/" + postgres_dbname;
						
					} else {
						Config.dburl = "jdbc:postgresql://" + postgres_host + "/" + postgres_dbname;
					}
				} else {
					Config.dburl = "jdbc:postgresql:" + postgres_dbname;
				}
				
				LOGGER.info("Time series will be queried from " + Config.dburl);
			} else {
				throw new RuntimeException("The environment variable POSTGRES_DBNAME is not set");
			}
			
    		kg_host = System.getenv("KG_HOST");
    		kg_path = System.getenv("KG_PATH");
    		kg_port = System.getenv("KG_PORT");
    		kg_protocol = System.getenv("KG_PROTOCOL");
    		Config.kguser = System.getenv("KG_USER");
    		Config.kgpassword = System.getenv("KG_PASSWORD");
			
    		if (kg_host != null || kg_path != null || kg_protocol != null) {
    			URIBuilder kgurl_builder = new URIBuilder();
    			kgurl_builder.setHost(kg_host);
    			kgurl_builder.setPath(kg_path);
    			kgurl_builder.setScheme(kg_protocol);
    			if (kg_port != null) {
    			    kgurl_builder.setPort(Integer.parseInt(kg_port));
    			}
    			try {
    				Config.kgurl = kgurl_builder.build().toString();
    			} catch (URISyntaxException e) {
    				LOGGER.error("Error in building kg url");
    				LOGGER.error(e.getMessage());
    				throw new RuntimeException(e);
    			}
    			LOGGER.info("Code will read the knowledge graph at " + Config.kgurl);
    		} else {
    			throw new RuntimeException("KG_HOST, KG_PATH and KG_PROTOCOL must be present as environment variables");
    		}
    		
    		initialised = true;
		}
	}
}
