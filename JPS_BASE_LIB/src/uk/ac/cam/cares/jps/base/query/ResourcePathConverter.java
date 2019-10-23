package uk.ac.cam.cares.jps.base.query;

import java.net.URI;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;

public class ResourcePathConverter {
	
	private static ResourcePathConverter instance = null;
	
	public static synchronized ResourcePathConverter getInstance() {
		if (instance == null) {
			instance = new ResourcePathConverter();
		}
		return instance;
	}

	public static String convert(String path) {
		
		//TODO-AE SC URGENT 20191021 CHANGE BACK this will work on claudius but not anymore locally --> configurable solution?
		// maybe change back not necessary any more, because of the solution below
		//		if (true) {
//			return path;
//		}
	
		String address = KeyValueManager.getServerAddress();
		if (address.contains("www.theworldavatar.com") || address.contains("www.jparksimulator.com")) {
			// i.e. the code is running on claudius
			return path;
		}
		
		// i.e. the code is not running on claudius
		String converted = path;
		if (path.contains("http://www.theworldavatar.com")) {
			converted = path.replace("http://www.theworldavatar.com", address);
		} else if (path.contains("http://www.jparksimulator.com")) {
			converted = path.replace("http://www.jparksimulator.com", address);
		}
		
		JPSBaseLogger.info(getInstance(), "converted resource path " + path + " to " + converted);
		return converted;
	}
	
	public static String convertToLocalPath(String path) {
		
		URI uri = AgentCaller.createURI(path);
		String root = KeyValueManager.get("absdir.root");
		String converted = root + uri.getPath();
		
		JPSBaseLogger.info(getInstance(), "converted resource path " + path + " to " + converted);
		return converted;
	}
}
