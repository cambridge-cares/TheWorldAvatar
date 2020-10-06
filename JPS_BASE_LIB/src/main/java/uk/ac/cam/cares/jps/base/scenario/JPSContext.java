package uk.ac.cam.cares.jps.base.scenario;

import org.apache.logging.log4j.ThreadContext;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;

public class JPSContext {
	
    public static void put(JSONObject jo, String key, String value) {
    	
    	JSONObject jpscontext = null;
        if (jo.isNull(JPSConstants.SCENARIO_JPS_CONTEXT)) {
        	jpscontext = new JSONObject();
        	jo.put(JPSConstants.SCENARIO_JPS_CONTEXT, jpscontext);
        } else {
        	jpscontext = jo.getJSONObject(JPSConstants.SCENARIO_JPS_CONTEXT);
        }
        
        jpscontext.put(key, value);
    }
    
    /**
     * @param jo
     * @param key
     * @return null if key no found 
     */
    public static String get(JSONObject jo, String key) {
    	
    	JSONObject jpscontext = jo.optJSONObject(JPSConstants.SCENARIO_JPS_CONTEXT);
    	if ((jpscontext != null) && !jpscontext.isNull(key)) {
    		return jpscontext.getString(key);
    	} 
    	
    	return null;
    }
      
	public static JSONObject getJpsContext() {
		String s = ThreadContext.get(JPSConstants.SCENARIO_JPS_CONTEXT);
		if (s != null) {
			return new JSONObject(s);
		}
		return null;
	}
	
	public static JSONObject createJpsContext() {
		JSONObject jpsContext = new JSONObject();
		ThreadContext.put(JPSConstants.SCENARIO_JPS_CONTEXT, jpsContext.toString());
		return jpsContext;
	}
	
	public static void removeJPSContext() {
		ThreadContext.remove(JPSConstants.SCENARIO_JPS_CONTEXT);
	}
	
	public static void putJPSContext(JSONObject jpsContext) {
		ThreadContext.put(JPSConstants.SCENARIO_JPS_CONTEXT, jpsContext.toString());
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
	
	public static void putSimulationTime(JSONObject jo, String value) {		
		put(jo, JPSConstants.SCENARIO_SIMULATION_TIME, value);
	}
	
	public static String getSimulationTime(JSONObject jo) {
		return get(jo, JPSConstants.SCENARIO_SIMULATION_TIME);
	}
	
	public static void put(String key, String value) {	
		JSONObject jpsContext = getJpsContext();
		if (jpsContext == null){
			jpsContext = createJpsContext();
		}
		jpsContext.put(key, value);
		putJPSContext(jpsContext);
	}
	
	public static String get(String key) {
		JSONObject jpsContext = getJpsContext();
		if ((jpsContext == null) || jpsContext.isNull(key)){
			return null;
		}
		return jpsContext.getString(key);
	}
	
	public static void remove(String key) {
		JSONObject jpsContext = getJpsContext();
		if (jpsContext != null){
			jpsContext.remove(key);
			putJPSContext(jpsContext);
		}
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
	
	public static void  putSimulationTime(String value) {
		put(JPSConstants.SCENARIO_SIMULATION_TIME, value);
	}
	
	public static String getSimulationTime() {
		return get(JPSConstants.SCENARIO_SIMULATION_TIME);
	}
	
	public static void removeSimulationTime() {
		remove(JPSConstants.SCENARIO_SIMULATION_TIME);
	}
}
