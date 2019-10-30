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
		
		//TODO-AE SC URGENT 20190913 CHANGE BACK this will work on claudius but not anymore locally --> configurable solution?
		if (true) {
			return path;
		}
		
		if (path.startsWith("http://www.theworldavatar.com/kb")) {
			converted = path.replace("http://www.theworldavatar.com/kb", "http://localhost:8080/kb");
		} else if (path.startsWith("http://www.jparksimulator.com/kb")) {
				converted = path.replace("http://www.jparksimulator.com/kb", "http://localhost:8080/kb");
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
