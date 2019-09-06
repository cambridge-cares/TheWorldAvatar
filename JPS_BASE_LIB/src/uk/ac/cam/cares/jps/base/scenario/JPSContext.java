package uk.ac.cam.cares.jps.base.scenario;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;

public class JPSContext {
	
    public static void put(JSONObject jo, String key, String value) {
    	
//    	JSONObject jpscontext = null;
//        if (!jo.isNull(JPSConstants.SCENARIO_JPS_CONTEXT)) {
//        	jpscontext = jo.getJSONObject(JPSConstants.SCENARIO_JPS_CONTEXT);
//        } else {
//        	jpscontext = new JSONObject();
//        	jo.put(JPSConstants.SCENARIO_JPS_CONTEXT, jpscontext);
//        }
//        
//        jpscontext.put(key, value);
    	
    	jo.put(key, value);
    }
    
    public static String get(JSONObject jo, String key) {
    	//return jo.getJSONObject(JPSConstants.SCENARIO_JPS_CONTEXT).getString(key);
    	return jo.getString(key);
    }
      
	public static void putScenarioUrl(JSONObject jo, String value) {		
		put(jo, JPSConstants.SCENARIO_URL, value);
	}
	
	public static String getScenarioUrl(JSONObject jo) {
		return get(jo, JPSConstants.SCENARIO_URL);
	}
	
	public static void putUsecaseUrl(JSONObject jo, String value) {		
		put(jo, JPSConstants.SCENARIO_USE_CASE_URL, value);
	}
	
	public static String getUsecaseUrl(JSONObject jo) {
		return get(jo, JPSConstants.SCENARIO_USE_CASE_URL);
	}
	
	public static void put(String key, String value) {
		 ThreadContext.put(key, value);
	}
	
	public static String get(String key) {
		return ThreadContext.get(key);
	}
	
	public static void remove(String key) {
		ThreadContext.remove(key);
	}
	
	public static void  putScenarioUrl(String value) {
		put(JPSConstants.SCENARIO_URL, value);
	}
	
	public static String getScenarioUrl() {
		return get(JPSConstants.SCENARIO_URL);
	}
	
	public static void removeScenarioUrl() {
		remove(JPSConstants.SCENARIO_URL);
	}
	
	public static void  putUsecaseUrl(String value) {
		put(JPSConstants.SCENARIO_USE_CASE_URL, value);
	}
	
	public static String getUsecaseUrl() {
		return get(JPSConstants.SCENARIO_USE_CASE_URL);
	}
	
	public static void removeUsecaseUrl() {
		remove(JPSConstants.SCENARIO_USE_CASE_URL);
	}
}
