package uk.ac.cam.cares.jps.composition.util;

import javax.servlet.http.HttpServletRequest;

import org.json.HTTP;
import org.json.JSONObject;

public class JSONReader {
	public static JSONObject readJSONFromRequest(HttpServletRequest request) {
		try {
			StringBuilder sb = new StringBuilder();
			String s;
			while ((s = request.getReader().readLine()) != null) {
				sb.append(s);
			}
			JSONObject jsonObject = HTTP.toJSONObject(sb.toString());
			String ObjectInString = jsonObject.getString("Method").toString();
			return new JSONObject(ObjectInString);

		} catch (Exception ex) {

		}
		return null;
	}
}
