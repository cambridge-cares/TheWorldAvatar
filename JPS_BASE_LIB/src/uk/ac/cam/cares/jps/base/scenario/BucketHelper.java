package uk.ac.cam.cares.jps.base.scenario;

import org.apache.logging.log4j.ThreadContext;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;

public class BucketHelper {
	
	public static String getScenarioUrl(String scenarioName) {
		return KeyValueManager.getServerAddress() + ScenarioHelper.SCENARIO_COMP_URL + "/" + scenarioName;
	}

	public static String getScenarioUrl() {
		
		String scenarioUrl = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		if (scenarioUrl == null) {
			scenarioUrl = getScenarioUrl(JPSConstants.SCENARIO_NAME_BASE);
		}
		
		return scenarioUrl;
	}
	
	public static String getHashedScenarioUrl(String url) {
		String hashed = ScenarioHelper.getHashedResource(url);
		return getKbScenarioUrl() + "/" + hashed;
	}
	
	public static String getKbScenarioUrl() {
		String scenarioUrl = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		if (scenarioUrl == null) {
			throw new JPSRuntimeException("can't create a scenario kb url for the base scenario");
		} 
		
		return getScenarioUrl() + "/kb";
	}
	


	public static boolean isScenarioUrl(String url) {
		return (url.indexOf(ScenarioHelper.SCENARIO_COMP_URL) >= 0);
	}
	
	public static String getLocalPath(String url) {
		String scenarioUrl = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		return getLocalPath(url, scenarioUrl);
	}
	
	public static String getLocalPath(String url, String scenarioUrl) {

		int i = url.indexOf(ScenarioHelper.SCENARIO_COMP_URL);
		if (i >= 0) { 
			// i.e. url is already scenario url
			return ScenarioHelper.getJpsWorkingDir() + url.substring(i);
		}
		
		if (scenarioUrl == null) {
			// OWL files for the base scenario are stored in Tomcat's root directory
			return ResourcePathConverter.convertToLocalPath(url);
		}
		
		i = scenarioUrl.indexOf(ScenarioHelper.SCENARIO_COMP_URL);
		String scenarioName = scenarioUrl.substring(i + ScenarioHelper.SCENARIO_COMP_URL.length() + 1);
		String hashed = ScenarioHelper.getHashedResource(url);
		String path = ScenarioHelper.getScenarioBucket(scenarioName) + "/kb/" + hashed;
		return path;
	}
}
