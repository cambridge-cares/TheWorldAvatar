package uk.ac.cam.cares.jps.base.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.json.JSONArray;
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
	
	public static String concat(String[] a, String separator) {
		StringBuffer b = new StringBuffer();
		for (int i=0; i<a.length; i++) {
			b.append(a[i]);
			if (i < a.length-1) {
				b.append(separator);
			}
		}
		return b.toString();
	}
	
	public static List<String> toList(JSONArray ja) {
		List<String> result = new ArrayList<String>();
		for (Object current : ja.toList()) {
			result.add(current.toString());
		};
		return result;
	}
	/** returns null if key: value does not exist in JSONObject jo
	 * 
	 * @param jo
	 * @param key
	 * @return
	 */
	public static String optNullKey(JSONObject jo, String key) {
		if (jo.isNull(key)) {
			return null;
		}
		return jo.getString(key);
	}
	/** Converts UTC time to TimeZone of Server, with offset
	 * e.g. 2021-04-15T06:59:08Z to 2021-04-15T13:59:08+08:00
	 * @param currDate in UTC
	 * @return local time with offset
	 */
	public static String convertToTimeZoneXSD(String currDate) {
		String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
		try {
			SimpleDateFormat utcFormat = new SimpleDateFormat(DATE_FORMAT);
			utcFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
			
			Date date = utcFormat.parse(currDate); //parse date in terms of time zone
			TimeZone timezone = TimeZone.getDefault();
			int offset = timezone.getRawOffset();
			String gmtTZ = String.format("%s%02d:%02d", 
			               offset < 0 ? "-" : "+", 
			               Math.abs(offset) / 3600000,
			               Math.abs(offset) / 60000 % 60);
			DateFormat newFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss" +  gmtTZ);
			return newFormat.format(date);
		}catch (ParseException e) {
			throw new JPSRuntimeException("WeatherIrradiationRetriever: Parse Exception for date");
		}
	}
}
