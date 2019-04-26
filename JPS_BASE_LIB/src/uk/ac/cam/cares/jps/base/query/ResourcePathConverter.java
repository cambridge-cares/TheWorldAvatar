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
		
		String converted = null;
		
		String s = "http://www.theworldavatar.com/kb";
		if (path.startsWith(s)) {
			converted = path.replace(s, "http://localhost:8080/kb");
		} else {
			return path;
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
