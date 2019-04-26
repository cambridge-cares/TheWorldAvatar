package uk.ac.cam.cares.jps.base.util;

import java.util.Locale;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class MiscUtil {

	/**
	 * Throws {@link JPSRuntimeException} if the given key has value <code>null</code>.
	 * Throws {@link JSONException} if key is not existent.
	 * Otherwise returns value for key.
	 * 
	 * @param jo
	 * @param key
	 * @return
	 */
	public static String notNull(JSONObject jo, String key) {
		if (jo.isNull(key)) {
			throw new JPSRuntimeException("missing input parameter " + key);
		}	
				
		return jo.getString(key);
	}
	
	/**
	 * Calls <code>String.format</code> with <code>Locale.ENGLISH</code>.
	 * Use this format method to avoid locale specific transformation. E.g.
	 * if the input parameter s is a SPARQL template with placeholder %f then this format method
	 * always replaces %f by 12.40. Contrary to that, if running on a different machine with
	 * Locale.German, %f would be replaced by 12,40 which in turn would result into a SPARQL error.
	 * 
	 * @param s 
	 * @param args
	 * @return
	 */
	public static String format(String s, Object... args) {
		return String.format(Locale.ENGLISH, s, args);
	}
}
