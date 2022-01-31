package uk.ac.cam.cares.jps.agent.flood;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Config{
	public static Properties props = null;
	public static String dburl;
	public static String dbuser;
	public static String dbpassword;
    
	public static String kgurl;
	public static String kguser;
	public static String kgpassword;
	
	public static String outputdir;
	
	public static String apiKey;
	
	private static final Logger LOGGER = LogManager.getLogger(Config.class);
	
	public static void initProperties() {
		if (props == null) {
			try {
	    		InputStream inputstream = new ClassPathResource("credentials.properties").getInputStream();
	
	    		Config.props = new Properties();
	    		Config.props.load(inputstream);
	    		
	    		Config.dburl = Config.props.getProperty("db.url");
	    		Config.dbuser = Config.props.getProperty("db.user");
	    		Config.dbpassword = Config.props.getProperty("db.password");
	    		Config.kgurl = Config.props.getProperty("kg.url");
	    		Config.kguser = Config.props.getProperty("kg.user");
	    		Config.kgpassword = Config.props.getProperty("kg.password");
	    		Config.outputdir = Config.props.getProperty("outputdir");
	    		Config.apiKey = Config.props.getProperty("apiKey");
			} catch (IOException e1) {
				LOGGER.error(e1.getMessage());
				throw new JPSRuntimeException(e1);
			}
		}
	}
}
