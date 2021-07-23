package uk.ac.cam.cares.derivedagent.example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ExampleConfig{
	public static Properties props = null;
	public static String dburl;
	public static String dbuser;
	public static String dbpassword;
    
	public static String kgurl;
	public static String kguser;
	public static String kgpassword;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(ExampleConfig.class);
	
	public static void initProperties() {
		try {
    		String credentials_file = Paths.get("main","resources","credentials.properties").toString();
    		InputStream inputstream = new ClassPathResource(credentials_file).getInputStream();

    		ExampleConfig.props = new Properties();
    		ExampleConfig.props.load(inputstream);
    		
    		ExampleConfig.dburl = ExampleConfig.props.getProperty("db.url");
    		ExampleConfig.dbuser = ExampleConfig.props.getProperty("db.user");
    		ExampleConfig.dbpassword = ExampleConfig.props.getProperty("db.password");
    		ExampleConfig.kgurl = ExampleConfig.props.getProperty("kg.url");
    		ExampleConfig.kguser = ExampleConfig.props.getProperty("kg.user");
    		ExampleConfig.kgpassword = ExampleConfig.props.getProperty("kg.password");
		} catch (IOException e1) {
			LOGGER.error(e1.getMessage());
			throw new JPSRuntimeException(e1);
		}
	}
}
