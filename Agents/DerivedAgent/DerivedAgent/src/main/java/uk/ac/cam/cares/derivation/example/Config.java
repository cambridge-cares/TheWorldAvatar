package uk.ac.cam.cares.derivation.example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
	
	private static final Logger LOGGER = LoggerFactory.getLogger(Config.class);
	
	public static void initProperties() {
		if (props == null) {
			try {
	    		String credentials_file = Paths.get("main","resources","credentials.properties").toString();
	    		InputStream inputstream = new ClassPathResource(credentials_file).getInputStream();
	
	    		Config.props = new Properties();
	    		Config.props.load(inputstream);
	    		
	    		Config.dburl = Config.props.getProperty("db.url");
	    		Config.dbuser = Config.props.getProperty("db.user");
	    		Config.dbpassword = Config.props.getProperty("db.password");
	    		Config.kgurl = Config.props.getProperty("kg.url");
	    		Config.kguser = Config.props.getProperty("kg.user");
	    		Config.kgpassword = Config.props.getProperty("kg.password");
			} catch (IOException e1) {
				LOGGER.error(e1.getMessage());
				throw new JPSRuntimeException(e1);
			}
		}
	}
}
