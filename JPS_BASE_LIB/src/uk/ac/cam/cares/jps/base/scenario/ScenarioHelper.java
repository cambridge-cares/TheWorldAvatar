package uk.ac.cam.cares.jps.base.scenario;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.UUID;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ScenarioHelper {
	
	private static final String JPS_WORKING_DIR = AgentLocator.getPathToJpsWorkingDir();
	public static final String SCENARIO_COMP_URL = "/JPS_SCENARIO/scenario";
	
	public static String getJpsWorkingDir() {
		return JPS_WORKING_DIR;
	}
	
	public static String getScenarioWorkingDir() {
		return JPS_WORKING_DIR + SCENARIO_COMP_URL;
	}
	
	public static String getScenarioPath(String scenarioName) {
		return SCENARIO_COMP_URL + "/" + scenarioName;
	}
	
	public static String getScenarioBucket(String scenarioName) {
		
		String scenarioBucket = getJpsWorkingDir() + getScenarioPath(scenarioName);
		// TODO-AE SC URGENT copy-on-write not yet implemented
		// so far only copy-on-read is implemented and thus this method returns always the scenario file
		// for copy-on-write we can create the bucket later (instead of checking always whether the bucket already exists) - at least for read and query
		// for copy-on-write read and query have to return the original path or url (= attribute resource), for update always the scenario file has to be returned
		File directory = new File(scenarioBucket);
	    if (!directory.exists()){
	        directory.mkdirs();
	    }
		
	    return scenarioBucket;
	}
	
	/**
	 * Calculates the hash code of the host name (without consideration of scheme and port) and adds the path of the URL without
	 * the fragment part
	 * 
	 * @param resource
	 * @return
	 */
	public static String getHashedResource(String resource) {
		String resourceWithoutHash = cutHash(resource);
		
	    // flat structure
		// int i = resourceWithoutHash.lastIndexOf("/");
	    // int hashForPath = resourceWithoutHash.substring(0, i).hashCode();
		// return hashForPath + "_" + resourceWithoutHash.substring(i+1);
		
	   URI uri;
	   try {
		   uri = new URI(resourceWithoutHash);
	   } catch (URISyntaxException e) {
		   throw new JPSRuntimeException(e.getMessage(), e);
	   }
	   int hashedHost = uri.getHost().hashCode();
	   return "" + hashedHost + uri.getPath();
	}
	
	public static String getFileNameWithinBucket(String resource, String scenarioBucket) {
		return scenarioBucket + "/" + getHashedResource(resource);
	}
	
	/**
	 * Divides the path into one to three Strings:<br>
	 * - the first string is the scenario name, e.g. for URL http://www.twa.com/JPS_SCENARIO/scenario/foo1234567 the scenarioName is foo1234567<br>
	 * - the second string is an optional operation, e.g. start or clean<br>
	 * 
	 * @param path
	 * @return Array with two strings
	 */
	public static String[] dividePath(String path) {
		
		String scenarioName = null;
		String operation = null; 
		
		// non-empty paths have a leading /
		if ((path == null) || (path.length() <= 1)) {
			scenarioName =  UUID.randomUUID().toString();
		} else {
			// remove leading / at the beginning of path
			path = path.substring(1);
			int index =  path.indexOf("/");
			
			if (index < 0) {
				scenarioName = path;
				operation = null;
			} else {
				scenarioName = path.substring(0, index);
				operation = path.substring(index);
			} 
			
			int i = scenarioName.indexOf(".owl");
			if (i >= 0) {
				scenarioName = scenarioName.substring(0,i);
			}
			
			if (scenarioName.length() < 4) {
				throw new JPSRuntimeException("the length of the scenario name must be at least 4");
			} else {
				// TODO-AE SC check that scenario name only contains letters, digits and - or _
			}
		}
		
		return new String[] {scenarioName, operation};
	}
	
	/**
	 * IRIs are often given in the form as e.g. http://www.theworldavatar.com/kb/powerplants/XYZ.owl#XYZ. This method discards the #-part in order to access the OWL locally.
	 * 
	 * @param fileName
	 * @return fileName without #-part
	 */
	public static String cutHash(String fileName) {
		int index = fileName.lastIndexOf(".owl#");
		if (index < 0) {
			return fileName;
		}
		return fileName.substring(0, index+4);
	}
}
