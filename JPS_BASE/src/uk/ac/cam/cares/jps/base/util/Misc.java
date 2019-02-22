package uk.ac.cam.cares.jps.base.util;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Misc {

	public static String notNull(JSONObject jo, String key) {
		if (jo.isNull(key)) {
			throw new JPSRuntimeException("missing input parameter " + key);
		}	
		return jo.getString(key);
	}
}
