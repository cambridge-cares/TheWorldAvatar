package uk.ac.cam.cares.jps.base.config;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class KeyValueManager {
	
	public static void set(String... keyOrValue) {
		
		for (int i=0; i<keyOrValue.length; i=i+2) {
			KeyValueMap.getInstance().put(keyOrValue[i], keyOrValue[i+1]);
		}
		
		AgentCaller.executeGet("/JPS_BASE/keys/set", keyOrValue);
	}
	
	public static String get(String key) {
		//TODO-AE URGENT URGENT every call to host, port etc. results into a HTTP Get --> caching solution, or redis, ...
		
		if (IKeys.HOST.equals(key) || IKeys.PORT.equals(key)) {
			return KeyValueMap.getInstance().get(key);
		}
		
		String result = AgentCaller.executeGet("/JPS_BASE/keys/get", "key", key);
		try {
			JSONObject jo = new JSONObject(result);
			return jo.getString(key);
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
